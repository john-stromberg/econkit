test_that("sahm rule matches a hand-computed example", {
  u <- c(rep(4, 14), 4.3, 4.6, 4.9)
  s <- ek_sahm_rule(u)
  expect_true(all(is.na(s[1:14])))
  expect_equal(s[15], 0.1)
  expect_equal(s[16], 0.3)
  expect_equal(s[17], 0.6)
})

test_that("output gap filters behave as expected", {
  x <- exp(seq(0, 1, length.out = 120))
  hp <- ek_output_gap(x, method = "hp")
  expect_lt(max(abs(hp$cycle)), 1e-6)

  ham <- ek_output_gap(x, method = "hamilton", h = 24, p = 12)
  expect_equal(nrow(ham), 120)
  expect_true(all(is.na(ham$trend[1:35])))

  set.seed(7)
  rw <- cumsum(stats::rnorm(200, 0.2))
  ham <- ek_output_gap(rw, method = "hamilton", log = FALSE, h = 8, p = 4)
  ref <- stats::lm(ek_lag(rw, -8) ~ rw + ek_lag(rw, 1) + ek_lag(rw, 2) + ek_lag(rw, 3))
  expect_equal(ham$cycle[!is.na(ham$cycle)], unname(stats::residuals(ref)))

  expect_error(ek_output_gap(c(1, NA, 3)), "missing")
  expect_error(ek_output_gap(c(-1, 2, 3)), "positive")
})

test_that("growth/inflation regimes follow the quadrant rules", {
  g <- c(1, 2, 3, 2, 1)
  p <- c(1, 0, 1, 2, 1)
  r <- ek_growth_inflation_regime(g, p, window = 1)
  expect_equal(as.character(r), c(NA, "Recovery", "Overheat", "Stagflation", "Reflation"))
  expect_error(ek_growth_inflation_regime(1:3, 1:2), "same length")
})

test_that("markov switching recovers simulated parameters", {
  set.seed(42)
  n <- 1500
  trans <- matrix(c(0.9, 0.1, 0.03, 0.97), 2, byrow = TRUE)
  s <- integer(n)
  s[1] <- 2L
  for (t in 2:n) s[t] <- sample(1:2, 1, prob = trans[s[t - 1], ])
  x <- ifelse(s == 1, stats::rnorm(n, -1, 0.8), stats::rnorm(n, 0.5, 0.4))

  m <- ek_markov_switching(x)
  expect_true(m$converged)
  truth <- econkit:::ek_msm_estep(x, c(-1, 0.5), c(0.8, 0.4), trans, c(0.5, 0.5))
  expect_gte(m$loglik, truth$loglik)
  expect_equal(unname(m$mean), c(-1, 0.5), tolerance = 0.2)
  expect_equal(unname(m$sd), c(0.8, 0.4), tolerance = 0.1)
  expect_equal(unname(diag(m$transition)), c(0.9, 0.97), tolerance = 0.05)
  expect_equal(unname(rowSums(m$smoothed)), rep(1, n))
  expect_gt(mean(m$regime == s), 0.9)
  expect_output(print(m), "2 regimes")
})

test_that("markov switching identifies contractions in sample data", {
  g <- ek_transform(macro_monthly$INDPRO, "log_diff")[-1]
  m <- ek_markov_switching(g)
  rec <- macro_monthly$USREC[-1]
  expect_lt(m$mean[["state1"]], 0)
  expect_gt(mean(m$smoothed[rec == 1, 1]), mean(m$smoothed[rec == 0, 1]) + 0.4)
})
