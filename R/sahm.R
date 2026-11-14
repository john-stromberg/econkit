#' Sahm rule recession indicator
#'
#' Computes the three-month moving average of the unemployment rate minus its
#' minimum over the previous 12 months. A reading of 0.5 percentage points or
#' more has historically signalled the early months of a US recession
#' (Sahm, 2019).
#'
#' @param unrate Monthly unemployment rate, in percent.
#' @return A numeric vector of Sahm indicator values, in percentage points.
#'   The first 14 values are `NA`.
#' @references Sahm, C. (2019). Direct stimulus payments to individuals.
#'   The Hamilton Project, Brookings Institution.
#' @export
#' @examples
#' s <- ek_sahm_rule(macro_monthly$UNRATE)
#' tail(s)
#' mean(s >= 0.5, na.rm = TRUE)
ek_sahm_rule <- function(unrate) {
  ma3 <- as.numeric(stats::filter(unrate, rep(1 / 3, 3), sides = 1))
  n <- length(ma3)
  out <- rep(NA_real_, n)
  for (t in seq_len(n)) {
    if (t < 15) next
    prior <- ma3[(t - 12):(t - 1)]
    if (is.na(ma3[t]) || anyNA(prior)) next
    out[t] <- ma3[t] - min(prior)
  }
  out
}
