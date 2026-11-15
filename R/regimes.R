#' Growth/inflation regime classification
#'
#' Classifies each period into one of four macro regimes from the direction
#' of growth and inflation momentum. This is the "investment clock" framework
#' widely used in tactical asset allocation:
#'
#' | Growth momentum | Inflation momentum | Regime | Typical leadership |
#' |---|---|---|---|
#' | falling | falling | Reflation | Bonds |
#' | rising | falling | Recovery | Equities |
#' | rising | rising | Overheat | Commodities |
#' | falling | rising | Stagflation | Cash |
#'
#' Momentum is the change in each rate over `window` periods. Inputs should
#' be rates, such as year-over-year growth and inflation from [ek_transform()].
#'
#' @param growth Growth rate, for example year-over-year industrial
#'   production growth.
#' @param inflation Inflation rate, for example year-over-year CPI inflation.
#' @param window Number of periods over which momentum is measured.
#' @return A factor with levels `Reflation`, `Recovery`, `Overheat` and
#'   `Stagflation`, with `NA` where momentum is unavailable.
#' @export
#' @examples
#' d <- ek_get_series()
#' g <- ek_transform(d$INDPRO, "yoy")
#' p <- ek_transform(d$CPIAUCSL, "yoy")
#' table(ek_growth_inflation_regime(g, p))
ek_growth_inflation_regime <- function(growth, inflation, window = 3L) {
  if (length(growth) != length(inflation)) {
    stop("`growth` and `inflation` must have the same length.", call. = FALSE)
  }
  dg <- growth - ek_lag(growth, window)
  dp <- inflation - ek_lag(inflation, window)
  regime <- ifelse(
    dg > 0,
    ifelse(dp > 0, "Overheat", "Recovery"),
    ifelse(dp > 0, "Stagflation", "Reflation")
  )
  factor(regime, levels = c("Reflation", "Recovery", "Overheat", "Stagflation"))
}

#' Markov-switching regime model
#'
#' Fits a `k`-state Markov-switching model with regime-dependent mean and
#' volatility (Hamilton, 1989). Estimation uses the EM algorithm: the
#' Hamilton filter and Kim smoother in the E-step, with closed-form updates
#' in the M-step. States are ordered by increasing mean, so with growth data
#' state 1 is the contraction regime.
#'
#' @section Model card:
#' * **Purpose:** probabilistic regime identification (for example
#'   contraction or expansion, or low or high volatility) for regime-aware
#'   allocation and risk scaling.
#' * **Assumptions:** Gaussian observations within each regime and a
#'   first-order, time-homogeneous Markov chain. There is no autoregressive
#'   dynamics within a regime.
#' * **Limitations:** EM can converge to local optima, and state labels are
#'   identified only by ordering. Smoothed probabilities use the full sample,
#'   so use filtered probabilities for real-time decisions.
#' * **Monitoring:** stability of the estimated parameters and expected
#'   durations across re-estimations.
#'
#' @param x Numeric series with no missing values, such as monthly
#'   industrial production growth.
#' @param k Number of regimes.
#' @param max_iter Maximum EM iterations.
#' @param tol Convergence tolerance on the log-likelihood.
#' @return An object of class `ek_msm` with `mean`, `sd`, `transition`
#'   (rows give the from-state), `duration` (expected months in each state),
#'   `filtered` and `smoothed` probability matrices, `regime` (most likely
#'   smoothed state), `loglik`, `iterations` and `converged`.
#' @references Hamilton, J. D. (1989). A new approach to the economic analysis
#'   of nonstationary time series and the business cycle. *Econometrica*,
#'   57(2), 357-384.
#' @export
#' @examples
#' d <- ek_get_series()
#' g <- ek_transform(d$INDPRO, "log_diff")[-1]
#' m <- ek_markov_switching(g)
#' m
ek_markov_switching <- function(x, k = 2L, max_iter = 500L, tol = 1e-8) {
  if (anyNA(x)) stop("`x` must not contain missing values.", call. = FALSE)
  if (k < 2) stop("`k` must be at least 2.", call. = FALSE)
  x <- as.numeric(x)
  n <- length(x)
  if (n < 10 * k) stop("Series too short for the requested number of regimes.", call. = FALSE)

  mu <- as.numeric(stats::quantile(x, probs = seq(0.15, 0.85, length.out = k)))
  sigma <- rep(stats::sd(x), k)
  trans <- matrix(0.05 / (k - 1), k, k)
  diag(trans) <- 0.95
  init <- rep(1 / k, k)
  sd_floor <- 1e-6 * stats::sd(x)
  loglik_old <- -Inf
  converged <- FALSE

  for (iter in seq_len(max_iter)) {
    e <- ek_msm_estep(x, mu, sigma, trans, init)
    w <- colSums(e$smoothed)
    mu <- colSums(e$smoothed * x) / w
    sigma <- pmax(sqrt(colSums(e$smoothed * outer(x, mu, "-")^2) / w), sd_floor)
    trans <- e$transitions / rowSums(e$transitions)
    init <- e$smoothed[1, ]
    if (abs(e$loglik - loglik_old) < tol) {
      converged <- TRUE
      break
    }
    loglik_old <- e$loglik
  }

  e <- ek_msm_estep(x, mu, sigma, trans, init)
  ord <- order(mu)
  trans <- trans[ord, ord, drop = FALSE]
  labels <- paste0("state", seq_len(k))
  dimnames(trans) <- list(from = labels, to = labels)
  filtered <- e$filtered[, ord, drop = FALSE]
  smoothed <- e$smoothed[, ord, drop = FALSE]
  colnames(filtered) <- colnames(smoothed) <- labels

  structure(
    list(
      mean = stats::setNames(mu[ord], labels),
      sd = stats::setNames(sigma[ord], labels),
      transition = trans,
      duration = stats::setNames(1 / (1 - diag(trans)), labels),
      filtered = filtered,
      smoothed = smoothed,
      regime = max.col(smoothed, ties.method = "first"),
      loglik = e$loglik,
      iterations = iter,
      converged = converged
    ),
    class = "ek_msm"
  )
}

ek_msm_estep <- function(x, mu, sigma, trans, init) {
  n <- length(x)
  k <- length(mu)
  dens <- vapply(seq_len(k), function(j) stats::dnorm(x, mu[j], sigma[j]), numeric(n))
  dens <- matrix(dens, n, k)
  pred <- filt <- matrix(0, n, k)
  loglik <- 0
  for (t in seq_len(n)) {
    pred[t, ] <- if (t == 1) init else drop(filt[t - 1, ] %*% trans)
    num <- pred[t, ] * dens[t, ]
    ct <- max(sum(num), .Machine$double.xmin)
    filt[t, ] <- num / ct
    loglik <- loglik + log(ct)
  }
  smooth <- matrix(0, n, k)
  smooth[n, ] <- filt[n, ]
  transitions <- matrix(0, k, k)
  for (t in (n - 1):1) {
    ratio <- smooth[t + 1, ] / pmax(pred[t + 1, ], .Machine$double.xmin)
    joint <- (filt[t, ] %o% ratio) * trans
    smooth[t, ] <- rowSums(joint)
    transitions <- transitions + joint
  }
  list(filtered = filt, smoothed = smooth, transitions = transitions, loglik = loglik)
}

#' @export
print.ek_msm <- function(x, ...) {
  cat("<ek_msm>", length(x$mean), "regimes | log-likelihood:", round(x$loglik, 2),
      "| converged:", x$converged, "\n")
  print(round(data.frame(
    mean = x$mean, sd = x$sd, duration = x$duration,
    share = as.numeric(table(factor(x$regime, levels = seq_along(x$mean)))) / length(x$regime)
  ), 4))
  invisible(x)
}
