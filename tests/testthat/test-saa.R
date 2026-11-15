test_that("SAA assumptions use only the requested sample window", {
  returns <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 36),
    a = rep(0.01, 36),
    b = rep(c(0.02, -0.02), 18)
  )
  inputs <- ek_saa_assumptions(
    returns, as_of = as.Date("2021-12-01"),
    lookback_years = 1, min_obs = 12
  )

  expect_equal(inputs$sample_start, as.Date("2021-01-01"))
  expect_equal(inputs$sample_end, as.Date("2021-12-01"))
  expect_equal(inputs$assumptions$n_obs, c(12L, 12L))
  expect_equal(inputs$assumptions$annualized_arithmetic_return, c(0.12, 0))
  expect_equal(inputs$assumptions$annualized_volatility[1], 0)
  expect_equal(dim(inputs$covariance), c(2L, 2L))
})

test_that("SAA portfolio summary validates and applies policy weights", {
  inputs <- ek_saa_assumptions()
  weights <- stats::setNames(
    rep(1 / nrow(inputs$assumptions), nrow(inputs$assumptions)),
    inputs$assumptions$asset_class
  )
  result <- ek_saa_portfolio(inputs, weights)

  expect_true(is.finite(result$annualized_arithmetic_return))
  expect_true(is.finite(result$annualized_volatility))
  expect_equal(sum(result$weights$weight), 1)
  expect_error(ek_saa_portfolio(inputs, c(a = 0.5, b = 0.5)),
               "exactly the assets")
  expect_error(ek_saa_portfolio(inputs, stats::setNames(
    rep(1, length(weights)), names(weights)
  )), "sum to one")
})

test_that("SAA assumptions reject insufficient and invalid return data", {
  returns <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 12),
    a = rep(0.01, 12)
  )
  expect_error(ek_saa_assumptions(returns, lookback_years = 1, min_obs = 13),
               "Need at least 13")
  returns$a[1] <- -1
  expect_error(ek_saa_assumptions(returns, lookback_years = 1, min_obs = 2),
               "greater than -100%")
})
