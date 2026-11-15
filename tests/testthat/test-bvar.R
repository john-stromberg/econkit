simulate_var1 <- function(n_obs = 600, seed = 11) {
  set.seed(seed)
  a <- matrix(c(0.6, 0.2, -0.1, 0.5), 2, byrow = TRUE)
  sigma <- matrix(c(1, 0.5, 0.5, 2), 2)
  shocks <- matrix(stats::rnorm(2 * n_obs), n_obs) %*% chol(sigma)
  y <- matrix(0, n_obs, 2)
  for (t in 2:n_obs) y[t, ] <- c(1, 0.5) + a %*% y[t - 1, ] + shocks[t, ]
  list(data = data.frame(date = seq(as.Date("1970-01-01"), by = "month", length.out = n_obs),
                         A = y[, 1], B = y[, 2]),
       a = a, sigma = sigma)
}

test_that("BVAR recovers VAR coefficients and the prior shrinks them", {
  sim <- simulate_var1()
  fit <- ek_bvar(sim$data, p = 1, lambda = 10, delta = 0)
  expect_s3_class(fit, "ek_bvar")
  expect_equal(unname(t(fit$coef[1:2, ])), sim$a, tolerance = 0.1)
  expect_equal(unname(fit$sigma), sim$sigma, tolerance = 0.25)
  expect_length(fit$log_vars, 0L)

  tight <- ek_bvar(sim$data, p = 2, lambda = 1e-4, delta = 1)
  expect_equal(unname(diag(tight$coef[1:2, ])), c(1, 1), tolerance = 1e-3)
  expect_equal(unname(tight$coef[c("B_l1", "A_l2"), "A"]), c(0, 0), tolerance = 1e-3)
  expect_output(print(tight), "2 lags")

  gappy <- sim$data
  gappy$A[100] <- NA
  expect_error(ek_bvar(gappy), "consecutive")
  expect_error(ek_bvar(sim$data, log_vars = "C"), "log_vars")
})

test_that("conditional forecasts meet hard conditions and match the Gaussian conditional mean", {
  sim <- simulate_var1()
  fit <- ek_bvar(sim$data, p = 1, lambda = 10, delta = 0)
  fc <- ek_bvar_forecast(fit, h = 1, conditions = list(A = 3), draws = 50,
                         parameter_uncertainty = FALSE, seed = 1)
  base <- ek_bvar_forecast(fit, h = 1, draws = 1, parameter_uncertainty = FALSE, seed = 1)
  s <- fit$sigma
  b <- base$summary$central
  expected_b <- b[2] + s[2, 1] / s[1, 1] * (3 - b[1])
  expect_equal(fc$summary$central, c(3, expected_b))
  expect_true(all(abs(fc$draws[, 1, "A"] - 3) < 1e-8))
  expect_gt(stats::sd(fc$draws[, 1, "B"]), 0)

  cond <- data.frame(date = seq(as.Date("2020-01-01"), by = "month", length.out = 3),
                     B = c(1, NA, 2))
  fc2 <- ek_bvar_forecast(fit, h = 6, conditions = cond, draws = 100, seed = 2)
  expect_true(all(abs(fc2$draws[, c(1, 3), "B"] - rep(c(1, 2), each = 100)) < 1e-8))
  expect_equal(fc2$summary$conditioned[fc2$summary$variable == "B"],
               c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE))
  expect_error(ek_bvar_forecast(fit, h = 2, conditions = list(C = 1)), "not in the model")
  expect_error(ek_bvar_forecast(fit, h = 2, conditions = list(A = 1:3)), "horizon")
  expect_error(ek_bvar_forecast(fit, h = 1, conditions = list(A = 1, B = 1)), "free")
  past <- data.frame(date = as.Date("1990-01-01"), A = 1)
  expect_error(ek_bvar_forecast(fit, h = 2, conditions = past), "forecast dates")
})

test_that("forecast bands are reproducible, widen with horizon and respect logs", {
  fit <- ek_bvar(p = 2)
  expect_setequal(fit$log_vars, c("PCEPILFE", "INDPRO"))
  set.seed(99)
  before <- .Random.seed
  a <- ek_bvar_forecast(fit, h = 12, draws = 200, seed = 5)
  expect_identical(.Random.seed, before)
  b <- ek_bvar_forecast(fit, h = 12, draws = 200, seed = 5)
  expect_identical(a$summary, b$summary)

  un <- a$summary[a$summary$variable == "UNRATE", ]
  expect_gt(un$upper[12] - un$lower[12], un$upper[1] - un$lower[1])
  expect_true(all(a$draws[, , "INDPRO"] > 0))
  expect_equal(a$dates[1], seq(fit$last_date, by = "month", length.out = 2)[2])

  path <- seq(110, 116, length.out = 6)
  c3 <- ek_bvar_forecast(fit, h = 6, conditions = list(INDPRO = path), draws = 20, seed = 3)
  expect_equal(c3$summary$central[c3$summary$variable == "INDPRO"], path)
  expect_error(ek_bvar_forecast(fit, h = 2, conditions = list(INDPRO = c(-1, 1))), "positive")
  expect_output(print(c3), "conditional")
})
