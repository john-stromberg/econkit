#' Bayesian VAR with a Minnesota prior
#'
#' Estimates a vector autoregression with the Minnesota prior implemented
#' through dummy observations (Banbura, Giannone and Reichlin, 2010), with a
#' normal-inverse-Wishart posterior. Each variable's own first lag is shrunk
#' toward `delta` (1 = random walk, 0 = white noise), other coefficients
#' toward zero, more tightly at longer lags; `lambda` sets the overall
#' tightness. Use [ek_bvar_forecast()] for unconditional and conditional
#' (scenario) forecasts.
#'
#' @section Model card:
#' **Purpose:** macro forecasts with uncertainty bands and internally
#' consistent conditional scenarios (for example "what do unemployment and
#' inflation do if the policy rate follows this path?") for SAA, stress
#' testing and credit-loss scenario design. **Assumptions:** linear dynamics
#' with constant parameters and Gaussian errors; the prior hyperparameters
#' are fixed rather than optimized. **Limitations:** no stochastic
#' volatility, structural breaks or lower bound on interest rates (condition
#' the policy rate to impose one); conditional forecasts use reduced-form
#' Cholesky shocks and the minimum-norm solution (Waggoner and Zha, 1999),
#' so they are not structurally identified policy experiments. Results on
#' the sample data are illustrative.
#'
#' @param data Wide data frame with a `date` column, at monthly or quarterly
#'   frequency. `NULL` loads the policy rate, unemployment, core PCE prices
#'   and industrial production from the sample data.
#' @param vars Variables to include; `NULL` uses all columns except `date`.
#'   Rows with any missing variable are dropped; the remaining dates must be
#'   consecutive.
#' @param p Number of lags.
#' @param lambda Overall prior tightness (smaller shrinks more).
#' @param delta Prior mean of each variable's own first lag; a single value
#'   or one per variable.
#' @param log_vars Variables modeled in logs (forecasts are returned in the
#'   original units). `"auto"` logs series whose registry transform is a
#'   growth rate (price indices, payrolls, output and so on).
#' @return An object of class `ek_bvar`, including the input data's
#'   `provenance` attribute (`NULL` if absent).
#' @references Banbura, M., Giannone, D. and Reichlin, L. (2010). Large
#'   Bayesian vector auto regressions. *Journal of Applied Econometrics*,
#'   25(1), 71-92. Waggoner, D. F. and Zha, T. (1999). Conditional forecasts
#'   in dynamic multivariate models. *Review of Economics and Statistics*,
#'   81(4), 639-651.
#' @export
#' @examples
#' fit <- ek_bvar(p = 2)
#' fit
ek_bvar <- function(data = NULL, vars = NULL, p = 4L, lambda = 0.2, delta = 1,
                    log_vars = "auto") {
  if (is.null(data)) data <- ek_get_series(c("FEDFUNDS", "UNRATE", "PCEPILFE", "INDPRO"))
  if (is.null(vars)) vars <- setdiff(names(data), "date")
  provenance <- attr(data, "provenance")
  ek_check_wide(data, vars)
  freq <- ek_periods_per_year(data)
  p <- as.integer(p)
  n <- length(vars)
  if (length(p) != 1 || is.na(p) || p < 1) stop("`p` must be a positive integer.", call. = FALSE)
  if (!is.numeric(lambda) || length(lambda) != 1 || lambda <= 0) {
    stop("`lambda` must be a positive number.", call. = FALSE)
  }
  if (!length(delta) %in% c(1, n) || !is.numeric(delta)) {
    stop("`delta` must have length 1 or one value per variable.", call. = FALSE)
  }
  delta <- rep_len(delta, n)
  if (identical(log_vars, "auto")) {
    cat <- ek_series_catalog()
    growth <- cat$id[cat$transform %in% c("yoy", "annualized", "log_diff", "pct")]
    log_vars <- intersect(vars, growth)
  }
  unknown <- setdiff(log_vars, vars)
  if (length(unknown) > 0) {
    stop("`log_vars` not in `vars`: ", paste(unknown, collapse = ", "), call. = FALSE)
  }

  data <- data[order(data$date), c("date", vars), drop = FALSE]
  data <- data[stats::complete.cases(data[vars]), , drop = FALSE]
  step <- if (freq == 4L) 3L else 1L
  expected <- seq(data$date[1], by = paste(step, "months"), length.out = nrow(data))
  if (nrow(data) == 0 || !identical(as.numeric(expected), as.numeric(data$date))) {
    stop("Observations must be consecutive after dropping missing values.", call. = FALSE)
  }
  z <- as.matrix(data[vars])
  if (any(z[, log_vars] <= 0)) stop("`log_vars` must be positive.", call. = FALSE)
  z[, log_vars] <- log(z[, log_vars])
  t_obs <- nrow(z)
  k <- n * p + 1
  if (t_obs - p < k + n) stop("Too few observations for this lag length.", call. = FALSE)

  y <- z[(p + 1):t_obs, , drop = FALSE]
  x <- cbind(do.call(cbind, lapply(seq_len(p), function(l) {
    z[(p + 1 - l):(t_obs - l), , drop = FALSE]
  })), 1)
  scale <- vapply(seq_len(n), function(i) {
    fit <- stats::lm.fit(cbind(1, z[-t_obs, i]), z[-1, i])
    stats::sd(fit$residuals)
  }, numeric(1))
  if (any(!is.finite(scale) | scale == 0)) stop("Each variable needs variation.", call. = FALSE)

  yd <- rbind(diag(delta * scale, n) / lambda, matrix(0, n * (p - 1), n),
              diag(scale, n), matrix(0, 1, n))
  xd <- rbind(cbind(kronecker(diag(seq_len(p), p), diag(scale, n)) / lambda, 0),
              matrix(0, n, k), c(rep(0, n * p), 1e-4))
  ys <- rbind(y, yd)
  xs <- rbind(x, xd)
  xtx_chol <- chol(crossprod(xs))
  v <- chol2inv(xtx_chol)
  coef <- v %*% crossprod(xs, ys)
  resid <- ys - xs %*% coef
  s <- crossprod(resid)
  df <- nrow(ys) - k
  dimnames(coef) <- list(c(paste0(rep(vars, p), "_l", rep(seq_len(p), each = n)), "const"),
                         vars)
  dimnames(s) <- list(vars, vars)

  structure(list(
    coef = coef, sigma = s / (df - n - 1), s = s, df = df, xtx_chol = xtx_chol,
    vars = vars, p = p, lambda = lambda, delta = stats::setNames(delta, vars),
    log_vars = log_vars, freq = freq, n_obs = nrow(y),
    last = z[t_obs:(t_obs - p + 1), , drop = FALSE], last_date = data$date[t_obs],
    history = data, provenance = provenance
  ), class = "ek_bvar")
}

#' @export
print.ek_bvar <- function(x, ...) {
  cat(sprintf("<ek_bvar> %d variables, %d lags, lambda = %g, %d observations (%s to %s)\n",
              length(x$vars), x$p, x$lambda, x$n_obs,
              format(x$history$date[x$p + 1]), format(x$last_date)))
  cat("Variables:", paste(x$vars, collapse = ", "), "\n")
  if (length(x$log_vars) > 0) cat("Modeled in logs:", paste(x$log_vars, collapse = ", "), "\n")
  invisible(x)
}

#' BVAR forecasts and conditional scenarios
#'
#' Simulates forecast paths from an [ek_bvar()] fit. With `conditions`, paths
#' are constrained to follow the given values for some variables and periods
#' (hard conditions), and the other variables respond consistently with the
#' estimated dynamics and correlations. Conditioning uses the distribution of
#' Cholesky shocks given the constraints: the minimum-norm shocks that meet
#' them plus random draws in the unconstrained directions (Waggoner and Zha,
#' 1999).
#'
#' @param model An `ek_bvar` object.
#' @param h Forecast horizon in periods.
#' @param conditions Optional conditions in original units: a named list of
#'   numeric vectors (element `i` applies to horizon `i`; `NA` leaves it
#'   free), or a data frame with a `date` (or `h`) column and one column per
#'   conditioned variable.
#' @param draws Number of simulated paths.
#' @param parameter_uncertainty Draw coefficients and covariance from the
#'   posterior for each path. If `FALSE`, only shocks are simulated.
#' @param level Coverage of the reported bands.
#' @param seed Optional random seed, for reproducible bands.
#' @return An object of class `ek_bvar_forecast`: a list with `summary` (long
#'   data frame of `date`, `h`, `variable`, `central`, `mean`, `median`,
#'   `lower`, `upper` and `conditioned`), `draws` (array of draws x horizon x
#'   variable, original units), `conditions` (matrix) and `model`. `central`
#'   is the path at the posterior mean with minimum-norm shocks, which does
#'   not depend on the seed.
#' @export
#' @examples
#' fit <- ek_bvar(p = 2)
#' fc <- ek_bvar_forecast(fit, h = 6, draws = 200, seed = 1,
#'                        conditions = list(FEDFUNDS = rep(1, 6)))
#' head(fc$summary)
ek_bvar_forecast <- function(model, h = 12L, conditions = NULL, draws = 500L,
                             parameter_uncertainty = TRUE, level = 0.8, seed = NULL) {
  if (!inherits(model, "ek_bvar")) stop("`model` must be an ek_bvar fit.", call. = FALSE)
  h <- as.integer(h)
  draws <- as.integer(draws)
  if (length(h) != 1 || is.na(h) || h < 1) stop("`h` must be a positive integer.", call. = FALSE)
  if (length(draws) != 1 || is.na(draws) || draws < 1) {
    stop("`draws` must be a positive integer.", call. = FALSE)
  }
  if (!is.numeric(level) || length(level) != 1 || level <= 0 || level >= 1) {
    stop("`level` must be between 0 and 1.", call. = FALSE)
  }
  vars <- model$vars
  n <- length(vars)
  step <- if (model$freq == 4L) "3 months" else "1 month"
  dates <- seq(model$last_date, by = step, length.out = h + 1)[-1]
  cond <- ek_bvar_conditions(conditions, vars, dates, h)
  if (any(cond[, model$log_vars] <= 0, na.rm = TRUE)) {
    stop("Conditions on `log_vars` must be positive.", call. = FALSE)
  }
  target <- cond
  target[, model$log_vars] <- log(target[, model$log_vars])
  idx <- which(!is.na(as.vector(t(target))))

  if (!is.null(seed)) {
    had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
    old_seed <- if (had_seed) get(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit(if (had_seed) {
      assign(".Random.seed", old_seed, envir = globalenv()) # nolint: object_name_linter
    } else {
      rm(list = ".Random.seed", envir = globalenv())
    }, add = TRUE)
    set.seed(seed)
  }
  s_inv <- solve(model$s)
  sim <- array(NA_real_, c(draws, h, n), dimnames = list(NULL, NULL, vars))
  for (d in seq_len(draws)) {
    if (parameter_uncertainty) {
      sigma <- solve(stats::rWishart(1, model$df, s_inv)[, , 1])
      z <- matrix(stats::rnorm(length(model$coef)), nrow(model$coef))
      coef <- model$coef + backsolve(model$xtx_chol, z) %*% chol(sigma)
    } else {
      sigma <- model$sigma
      coef <- model$coef
    }
    sim[d, , ] <- ek_bvar_simulate(coef, sigma, model, h, target, idx, random = TRUE)
  }
  central <- ek_bvar_simulate(model$coef, model$sigma, model, h, target, idx, random = FALSE)
  central[, model$log_vars] <- exp(central[, model$log_vars])
  sim[, , model$log_vars] <- exp(sim[, , model$log_vars])

  alpha <- (1 - level) / 2
  summary <- do.call(rbind, lapply(seq_len(n), function(j) {
    m <- matrix(sim[, , j], nrow = draws)
    q <- apply(m, 2, stats::quantile, probs = c(alpha, 0.5, 1 - alpha), names = FALSE)
    data.frame(date = dates, h = seq_len(h), variable = vars[j], central = central[, j],
               mean = colMeans(m), median = q[2, ], lower = q[1, ], upper = q[3, ],
               conditioned = !is.na(cond[, j]), stringsAsFactors = FALSE)
  }))
  rownames(summary) <- NULL
  structure(list(summary = summary, draws = sim, conditions = cond, dates = dates,
                 level = level, model = model), class = "ek_bvar_forecast")
}

#' @export
print.ek_bvar_forecast <- function(x, ...) {
  cat(sprintf("<ek_bvar_forecast> %d periods to %s, %d draws, %.0f%% bands%s\n",
              length(x$dates), format(max(x$dates)), dim(x$draws)[1], 100 * x$level,
              if (any(!is.na(x$conditions))) ", conditional" else ""))
  last <- x$summary[x$summary$h == max(x$summary$h), c("variable", "central", "lower", "upper")]
  last[-1] <- round(last[-1], 2)
  print(last, row.names = FALSE)
  invisible(x)
}

ek_bvar_conditions <- function(conditions, vars, dates, h) {
  cond <- matrix(NA_real_, h, length(vars), dimnames = list(NULL, vars))
  if (is.null(conditions)) return(cond)
  if (is.data.frame(conditions)) {
    if ("date" %in% names(conditions)) {
      rows <- match(as.Date(conditions$date), dates)
      if (anyNA(rows)) stop("Condition dates must be forecast dates.", call. = FALSE)
    } else if ("h" %in% names(conditions)) {
      rows <- as.integer(conditions$h)
      if (anyNA(rows) || any(rows < 1 | rows > h)) {
        stop("Condition `h` must be within the forecast horizon.", call. = FALSE)
      }
    } else {
      stop("Condition data frames need a `date` or `h` column.", call. = FALSE)
    }
    cols <- setdiff(names(conditions), c("date", "h"))
    values <- lapply(cols, function(v) {
      out <- rep(NA_real_, h)
      out[rows] <- as.numeric(conditions[[v]])
      out
    })
    conditions <- stats::setNames(values, cols)
  }
  if (!is.list(conditions) || is.null(names(conditions)) || any(!nzchar(names(conditions)))) {
    stop("`conditions` must be a named list or a data frame.", call. = FALSE)
  }
  unknown <- setdiff(names(conditions), vars)
  if (length(unknown) > 0) {
    stop("Conditioned variable(s) not in the model: ", paste(unknown, collapse = ", "),
         call. = FALSE)
  }
  for (v in names(conditions)) {
    values <- as.numeric(unlist(lapply(conditions[[v]], function(e) if (is.null(e)) NA else e)))
    if (length(values) > h) stop("Conditions exceed the forecast horizon.", call. = FALSE)
    cond[seq_along(values), v] <- values
  }
  if (all(!is.na(cond))) stop("At least one variable must be left free.", call. = FALSE)
  cond
}

# One forecast path in model units. Shocks are Cholesky shocks for all
# periods, drawn from their distribution given the hard conditions.
ek_bvar_simulate <- function(coef, sigma, model, h, target, idx, random) {
  n <- length(model$vars)
  p <- model$p
  base <- ek_bvar_path(coef, model$last, n, p, h)
  m <- ek_bvar_impact(coef, sigma, n, p, h)
  shocks <- if (random) stats::rnorm(n * h) else rep(0, n * h)
  if (length(idx) > 0) {
    r <- m[idx, , drop = FALSE]
    gap <- as.vector(t(target))[idx] - as.vector(t(base))[idx]
    proj <- t(r) %*% solve(tcrossprod(r))
    shocks <- as.vector(proj %*% gap + shocks - proj %*% (r %*% shocks))
  }
  out <- base + matrix(m %*% shocks, h, n, byrow = TRUE)
  colnames(out) <- model$vars
  out
}

ek_bvar_path <- function(coef, last, n, p, h) {
  hist <- last
  out <- matrix(NA_real_, h, n)
  for (i in seq_len(h)) {
    y <- c(as.vector(t(hist)), 1) %*% coef
    out[i, ] <- y
    hist <- rbind(y, hist[-p, , drop = FALSE])
  }
  out
}

# Response of the stacked forecast path to stacked Cholesky shocks.
ek_bvar_impact <- function(coef, sigma, n, p, h) {
  a <- lapply(seq_len(p), function(l) t(coef[((l - 1) * n + 1):(l * n), , drop = FALSE]))
  psi <- vector("list", h)
  psi[[1]] <- diag(n)
  for (j in seq_len(h - 1)) {
    acc <- matrix(0, n, n)
    for (l in seq_len(min(j, p))) acc <- acc + a[[l]] %*% psi[[j - l + 1]]
    psi[[j + 1]] <- acc
  }
  a0 <- t(chol(sigma))
  m <- matrix(0, n * h, n * h)
  for (i in seq_len(h)) {
    for (s in seq_len(i)) {
      m[(i - 1) * n + seq_len(n), (s - 1) * n + seq_len(n)] <- psi[[i - s + 1]] %*% a0
    }
  }
  m
}
