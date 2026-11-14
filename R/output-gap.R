#' Output gap and trend-cycle decomposition
#'
#' Splits a series into trend and cycle using either the Hamilton (2018)
#' regression filter (default) or the Hodrick-Prescott filter.
#'
#' The Hamilton filter regresses \eqn{y_{t+h}} on \eqn{y_t, \ldots, y_{t-p+1}}.
#' The fitted value is the trend at \eqn{t+h}, and the residual is the cycle.
#' Unlike the two-sided HP filter it uses no future information, so it avoids
#' the HP filter's end-point bias and spurious cycles. The HP filter is
#' included as a benchmark.
#'
#' @param x Numeric series in levels, regularly spaced, with no missing values.
#' @param method `"hamilton"` or `"hp"`.
#' @param log If `TRUE`, the series is transformed to `100 * log(x)` first,
#'   so the cycle is in percent deviation from trend.
#' @param h Hamilton horizon in periods (24 for monthly, 8 for quarterly data).
#' @param p Number of Hamilton lags (12 for monthly, 4 for quarterly data).
#' @param lambda HP smoothing parameter (129600 for monthly, 1600 for
#'   quarterly data).
#' @return A data frame with `trend` and `cycle`, the same length as `x`.
#'   The Hamilton filter leaves the first `h + p - 1` values as `NA`.
#' @references Hamilton, J. D. (2018). Why you should never use the
#'   Hodrick-Prescott filter. *Review of Economics and Statistics*, 100(5),
#'   831-843.
#' @export
#' @examples
#' gap <- ek_output_gap(macro_monthly$INDPRO)
#' tail(gap)
ek_output_gap <- function(x, method = c("hamilton", "hp"), log = TRUE,
                          h = 24L, p = 12L, lambda = 129600) {
  method <- match.arg(method)
  if (anyNA(x)) stop("`x` must not contain missing values.", call. = FALSE)
  if (log) {
    if (any(x <= 0)) stop("`x` must be positive when `log = TRUE`.", call. = FALSE)
    x <- 100 * base::log(x)
  }
  trend <- switch(method,
    hamilton = ek_hamilton_trend(x, h, p),
    hp = ek_hp_trend(x, lambda)
  )
  data.frame(trend = trend, cycle = x - trend)
}

ek_hamilton_trend <- function(x, h, p) {
  n <- length(x)
  if (n <= h + p + 1) stop("Series too short for the chosen `h` and `p`.", call. = FALSE)
  lags <- sapply(0:(p - 1), function(j) ek_lag(x, j))
  y <- ek_lag(x, -h)
  ok <- stats::complete.cases(cbind(y, lags))
  fit <- stats::lm.fit(cbind(1, lags[ok, , drop = FALSE]), y[ok])
  fitted_origin <- drop(cbind(1, lags) %*% fit$coefficients)
  ek_lag(fitted_origin, h)
}

ek_hp_trend <- function(x, lambda) {
  n <- length(x)
  if (n < 4) stop("Series too short for the HP filter.", call. = FALSE)
  d <- diff(diag(n), differences = 2)
  drop(solve(diag(n) + lambda * crossprod(d), x))
}
