test_that("recession model fits with a negative spread coefficient", {
  m <- ek_recession_model(macro_monthly, horizon = 12)
  expect_s3_class(m, "ek_recession_model")
  expect_lt(stats::coef(m$fit)[["T10Y3M"]], 0)
  expect_gt(m$metrics$auc, 0.7)
  p <- predict(m)
  expect_true(all(p$probability >= 0 & p$probability <= 1, na.rm = TRUE))
  expect_equal(p$target_date[1], as.Date("1971-01-01"))
  expect_output(print(m), "horizon: 12")
})

test_that("within target is at least as likely as at target", {
  at <- ek_recession_model(macro_monthly, horizon = 12, target = "at")
  within <- ek_recession_model(macro_monthly, horizon = 12, target = "within")
  expect_gt(within$metrics$base_rate, at$metrics$base_rate)
})

test_that("backtest only uses labels known at the forecast origin", {
  h <- 6
  lag <- 3
  n <- 200
  set.seed(1)
  d <- data.frame(
    date = seq(as.Date("2000-01-01"), by = "month", length.out = n),
    USREC = rep(c(0, 0, 0, 1), length.out = n),
    T10Y3M = stats::rnorm(n)
  )
  bt <- ek_backtest_recession(d, horizon = h, label_lag = lag, min_train = 50)
  expect_equal(min(bt$date), d$date[50 + h + lag])

  # Rewriting outcomes that were unknown at the first origin must not change
  # that forecast.
  first <- which(d$date == min(bt$date))
  unknown <- (first - lag + 1):n
  d2 <- d
  d2$USREC[unknown] <- 1 - d2$USREC[unknown]
  bt2 <- ek_backtest_recession(d2, horizon = h, label_lag = lag, min_train = 50)
  expect_equal(bt2$probability[1], bt$probability[1])
})

test_that("backtest is informative on sample data", {
  bt <- ek_backtest_recession(macro_monthly, horizon = 12, label_lag = 6)
  m <- attr(bt, "metrics")
  expect_gt(m$auc, 0.7)
  expect_true(all(bt$probability >= 0 & bt$probability <= 1))
})

test_that("classification metrics match hand calculations", {
  m <- ek_classification_metrics(c(0.1, 0.8, 0.3, 0.9), c(0, 1, 0, 1))
  expect_equal(m$auc, 1)
  expect_equal(m$brier, mean(c(0.01, 0.04, 0.09, 0.01)))
  expect_equal(ek_classification_metrics(c(0.2, 0.2), c(0, 1))$auc, 0.5)
})

test_that("invalid inputs are rejected", {
  expect_error(ek_recession_model(macro_monthly, predictors = "NOPE"), "missing columns")
  expect_error(ek_recession_model(macro_monthly, horizon = 0), "at least 1")
})
