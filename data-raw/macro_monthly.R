# Builds the bundled `macro_monthly` sample dataset.
#
# The data are SYNTHETIC: simulated from a regime-switching model calibrated
# to stylised US macro dynamics (recession frequency/duration, a yield-curve
# spread that inverts ahead of recessions, counter-cyclical unemployment and
# financial conditions). They are suitable for demos, tests and CI, never for
# real decisions. Use `ek_get_series(source = "fred")` for real data.
#
# Run from the package root: Rscript data-raw/macro_monthly.R

set.seed(20261114)

dates <- seq(as.Date("1970-01-01"), as.Date("2025-12-01"), by = "month")
n <- length(dates)

# Recession state: two-state Markov chain.
p_enter <- 0.012
p_stay <- 0.92
rec <- integer(n)
for (t in 2:n) {
  rec[t] <- if (rec[t - 1] == 1L) {
    stats::rbinom(1, 1, p_stay)
  } else {
    stats::rbinom(1, 1, p_enter)
  }
}

# Indicator of a recession starting/occurring 6-18 months ahead (curve leads).
ahead <- vapply(seq_len(n), function(t) {
  idx <- (t + 6):(t + 18)
  idx <- idx[idx <= n]
  if (length(idx) == 0) 0 else max(rec[idx])
}, numeric(1))

ar1 <- function(n, phi, sd) {
  x <- numeric(n)
  e <- stats::rnorm(n, 0, sd)
  for (t in 2:n) x[t] <- phi * x[t - 1] + e[t]
  x
}

t10y3m <- 1.7 - 2.1 * ahead * (1 - rec) + 0.8 * rec + ar1(n, 0.9, 0.25)

tb3ms <- numeric(n)
tb3ms[1] <- 6
for (t in 2:n) {
  tb3ms[t] <- max(
    0.05,
    0.985 * tb3ms[t - 1] + 0.015 * 4.5 - 0.30 * rec[t] + 0.08 * ahead[t] +
      stats::rnorm(1, 0, 0.25)
  )
}
fedfunds <- pmax(0.05, tb3ms + 0.15 + stats::rnorm(n, 0, 0.1))
dgs10 <- tb3ms + t10y3m

growth_ip <- 0.0022 - 0.011 * rec + stats::rnorm(n, 0, 0.006)
indpro <- 40 * exp(cumsum(growth_ip))

growth_emp <- 0.0016 - 0.0045 * rec + stats::rnorm(n, 0, 0.001)
payems <- 71000 * exp(cumsum(growth_emp))

unrate <- numeric(n)
unrate[1] <- 4.5
for (t in 2:n) {
  drift <- if (rec[t] == 1L) 0.28 else -0.03 * (unrate[t - 1] - 4.2)
  unrate[t] <- min(12, max(3, unrate[t - 1] + drift + stats::rnorm(1, 0, 0.08)))
}

infl <- numeric(n)
infl[1] <- 5
core <- numeric(n)
core[1] <- 5
for (t in 2:n) {
  infl[t] <- 3 + 0.96 * (infl[t - 1] - 3) - 0.08 * rec[t] + stats::rnorm(1, 0, 0.35)
  core[t] <- 2.8 + 0.98 * (core[t - 1] - 2.8) - 0.03 * rec[t] + stats::rnorm(1, 0, 0.1)
}
cpiaucsl <- 37.8 * cumprod(1 + infl / 1200)
pcepilfe <- 25 * cumprod(1 + core / 1200)

nfci <- -0.4 + 0.9 * rec + 0.3 * ahead + ar1(n, 0.85, 0.12)

# Synthetic intermediate Treasury maturities. The endpoints are tied to the
# sample 3-month and 10-year rates; a persistent curvature factor fills the
# remaining points for curve-fitting demonstrations.
lambda <- 0.6
tenors <- c(0.25, 2, 5, 10, 30)
decay <- lambda * tenors
slope_load <- -expm1(-decay) / decay
curvature_load <- slope_load - exp(-decay)
curve_design <- cbind(1, slope_load[c(1, 4)])
curvature <- 0.35 + ar1(n, 0.7, 0.12)
factors <- t(vapply(seq_len(n), function(i) {
  endpoint_factors <- solve(
    curve_design, c(tb3ms[i], dgs10[i] - curvature_load[4] * curvature[i])
  )
  c(level = endpoint_factors[1], slope = endpoint_factors[2], curvature = curvature[i])
}, numeric(3)))
curve_loadings <- cbind(1, slope_load, curvature_load)
curve_yields <- factors %*% t(curve_loadings)

r2 <- function(x, d = 2) round(x, d)

macro_monthly <- data.frame(
  date = dates,
  USREC = rec,
  T10Y3M = r2(t10y3m),
  DGS10 = r2(dgs10),
  DGS3MO = r2(curve_yields[, 1]),
  DGS2 = r2(curve_yields[, 2]),
  DGS5 = r2(curve_yields[, 3]),
  DGS30 = r2(curve_yields[, 5]),
  TB3MS = r2(tb3ms),
  FEDFUNDS = r2(fedfunds),
  UNRATE = r2(unrate, 1),
  PAYEMS = r2(payems, 0),
  INDPRO = r2(indpro, 3),
  CPIAUCSL = r2(cpiaucsl, 3),
  PCEPILFE = r2(pcepilfe, 3),
  NFCI = r2(nfci, 3)
)
attr(macro_monthly, "synthetic") <- TRUE

message(
  "Recession months: ", sum(rec), " (", round(100 * mean(rec), 1), "%), episodes: ",
  sum(diff(rec) == 1)
)

dir.create("data", showWarnings = FALSE)
save(macro_monthly, file = "data/macro_monthly.rda", compress = "xz")
