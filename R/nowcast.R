#' Factor bridge nowcast
#'
#' Nowcasts a quarterly target, such as GDP growth, from monthly indicators
#' that are published earlier. Indicators are transformed (registry defaults;
#' see [ek_apply_transforms()]) and standardized. Principal components of the
#' months where all indicators are observed give the factor loadings. Each
#' month's factors are then estimated from whichever indicators are available
#' by least squares on the loadings, so the latest months with missing
#' releases (the "ragged edge") still contribute. Monthly factors are averaged
#' to quarters and a bridge regression links them to the target, estimated on
#' quarters with all three months observed. Quarters after the last target
#' observation are nowcast from the months available so far.
#'
#' Use `vintage` (with the default data) or pass data from
#' `ek_get_series(vintage = )` to produce the nowcast as it would have been
#' made on a given date.
#'
#' @section Model card:
#' **Purpose:** an early, transparent estimate of current-quarter growth for
#' macro positioning, scenario anchoring and bank planning. **Assumptions:**
#' stable loadings and bridge coefficients over the sample; a missing month
#' in the current quarter is implicitly assumed to equal the average of the
#' observed months. Standardization and loadings use the full sample
#' available at the vintage. **Limitations:** a static principal-components
#' factor rather than a full dynamic factor model with a Kalman filter; no
#' news decomposition; intervals reflect bridge-regression error only, not
#' factor or parameter uncertainty. The default sample-data nowcast (of
#' quarterly industrial production growth) is a synthetic demonstration.
#'
#' @param indicators Wide monthly data frame of indicators. `NULL` loads
#'   payrolls, unemployment, financial conditions and the yield-curve slope
#'   from the sample data.
#' @param target Wide quarterly data frame with `date` and one target column.
#'   `NULL` loads quarterly industrial production from the sample data.
#' @param vintage Vintage date used when loading the default data.
#' @param n_factors Number of factors.
#' @param transforms Optional named transforms overriding the registry, for
#'   indicators or the target.
#' @param level Prediction-interval coverage.
#' @return An object of class `ek_nowcast`: a list with `nowcast` (quarter,
#'   point nowcast, interval bounds and months observed), `fitted` (in-sample
#'   actual and fitted values), `factors` (monthly), `loadings`, `bridge`
#'   (the `lm` fit), `r_squared`, `target` (column name) and `indicators`.
#' @export
#' @examples
#' nc <- ek_nowcast(vintage = "2020-05-15")
#' nc
ek_nowcast <- function(indicators = NULL, target = NULL, vintage = NULL, n_factors = 1L,
                       transforms = NULL, level = 0.8) {
  if (is.null(indicators)) {
    indicators <- ek_get_series(c("PAYEMS", "UNRATE", "NFCI", "T10Y3M"), vintage = vintage)
  }
  if (is.null(target)) target <- ek_get_series("INDPRO", frequency = "quarter", vintage = vintage)
  ek_check_wide(indicators)
  ek_check_wide(target)
  if (ncol(target) != 2) {
    stop("`target` must have a `date` column and one value column.", call. = FALSE)
  }
  ids <- setdiff(names(indicators), "date")
  target_id <- setdiff(names(target), "date")
  n_factors <- as.integer(n_factors)
  if (length(n_factors) != 1 || is.na(n_factors) || n_factors < 1 ||
        n_factors >= length(ids)) {
    stop("`n_factors` must be between 1 and the number of indicators minus 1.", call. = FALSE)
  }
  if (!is.numeric(level) || length(level) != 1 || level <= 0 || level >= 1) {
    stop("`level` must be between 0 and 1.", call. = FALSE)
  }

  x <- ek_apply_transforms(indicators, transforms[intersect(names(transforms), ids)], freq = 12)
  x <- x[order(x$date), , drop = FALSE]
  y <- ek_apply_transforms(target, transforms[intersect(names(transforms), target_id)], freq = 4)

  z <- as.matrix(x[ids])
  center <- colMeans(z, na.rm = TRUE)
  scale <- apply(z, 2, stats::sd, na.rm = TRUE)
  if (any(!is.finite(scale) | scale == 0)) {
    stop("Each indicator needs variation after transformation.", call. = FALSE)
  }
  z <- sweep(sweep(z, 2, center), 2, scale, "/")
  complete <- stats::complete.cases(z)
  if (sum(complete) < 24) {
    stop("Need at least 24 months with all indicators observed.", call. = FALSE)
  }
  pca <- stats::prcomp(z[complete, , drop = FALSE], center = FALSE, scale. = FALSE)
  loadings <- pca$rotation[, seq_len(n_factors), drop = FALSE]
  flip <- colSums(loadings) < 0
  loadings[, flip] <- -loadings[, flip]
  colnames(loadings) <- paste0("factor", seq_len(n_factors))

  factors <- t(vapply(seq_len(nrow(z)), function(t) {
    ek_project_factors(z[t, ], loadings)
  }, numeric(n_factors)))
  if (n_factors == 1) factors <- matrix(factors, ncol = 1)
  colnames(factors) <- colnames(loadings)

  quarter <- ek_floor_date(x$date, "quarter")
  quarters <- sort(unique(quarter))
  fq <- t(vapply(seq_along(quarters), function(i) {
    rows <- quarter == quarters[i] & stats::complete.cases(factors)
    c(sum(rows), if (any(rows)) colMeans(factors[rows, , drop = FALSE]) else rep(NA, n_factors))
  }, numeric(n_factors + 1)))
  fq <- data.frame(quarter = quarters, months = as.integer(fq[, 1]), fq[, -1, drop = FALSE])
  names(fq)[-(1:2)] <- colnames(loadings)
  fq$y <- y[[target_id]][match(fq$quarter, ek_floor_date(y$date, "quarter"))]

  est <- fq[fq$months == 3 & !is.na(fq$y), , drop = FALSE]
  if (nrow(est) < n_factors + 8) {
    stop("Too few quarters with both target and complete indicator data.", call. = FALSE)
  }
  formula <- stats::reformulate(colnames(loadings), response = "y")
  bridge <- stats::lm(formula, data = est)

  last_target <- max(ek_floor_date(y$date[!is.na(y[[target_id]])], "quarter"))
  ahead <- fq[fq$quarter > last_target & fq$months > 0, , drop = FALSE]
  pred <- if (nrow(ahead) > 0) {
    stats::predict(bridge, newdata = ahead, interval = "prediction", level = level)
  } else {
    matrix(numeric(), 0, 3, dimnames = list(NULL, c("fit", "lwr", "upr")))
  }

  structure(list(
    nowcast = data.frame(quarter = ahead$quarter, nowcast = unname(pred[, "fit"]),
                         lower = unname(pred[, "lwr"]), upper = unname(pred[, "upr"]),
                         months_observed = ahead$months),
    fitted = data.frame(quarter = est$quarter, actual = est$y,
                        fitted = unname(stats::fitted(bridge))),
    factors = data.frame(date = x$date, factors),
    loadings = loadings,
    bridge = bridge,
    r_squared = summary(bridge)$r.squared,
    target = target_id,
    indicators = ids,
    level = level
  ), class = "ek_nowcast")
}

# Least-squares factor estimate from the indicators observed this month.
ek_project_factors <- function(z, loadings) {
  obs <- !is.na(z)
  if (sum(obs) < ncol(loadings)) return(rep(NA_real_, ncol(loadings)))
  lo <- loadings[obs, , drop = FALSE]
  out <- tryCatch(solve(crossprod(lo), crossprod(lo, z[obs])), error = function(e) NULL)
  if (is.null(out)) rep(NA_real_, ncol(loadings)) else as.numeric(out)
}

#' @export
print.ek_nowcast <- function(x, ...) {
  cat("<ek_nowcast> target:", x$target, "| indicators:", paste(x$indicators, collapse = ", "),
      "\n")
  cat(sprintf("Bridge R-squared: %.2f on %d quarters; %d factor(s)\n", x$r_squared,
              nrow(x$fitted), ncol(x$loadings)))
  if (nrow(x$nowcast) == 0) {
    cat("No quarters to nowcast: the target is observed through the latest data.\n")
  } else {
    nc <- x$nowcast
    nc[c("nowcast", "lower", "upper")] <- round(nc[c("nowcast", "lower", "upper")], 2)
    cat(sprintf("Nowcast (%.0f%% interval):\n", 100 * x$level))
    print(nc, row.names = FALSE)
  }
  invisible(x)
}
