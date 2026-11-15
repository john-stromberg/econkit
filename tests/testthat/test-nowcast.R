simulate_nowcast_data <- function(n_months = 240, seed = 3) {
  set.seed(seed)
  f <- as.numeric(stats::arima.sim(list(ar = 0.7), n_months))
  dates <- seq(as.Date("2000-01-01"), by = "month", length.out = n_months)
  lam <- c(1, 0.8, -0.6, 0.5)
  ind <- data.frame(date = dates)
  for (j in seq_along(lam)) ind[[paste0("X", j)]] <- lam[j] * f + stats::rnorm(n_months, sd = 0.3)
  q <- ek_floor_date(dates, "quarter")
  fq <- tapply(f, q, mean)
  target <- data.frame(date = as.Date(names(fq)),
                       Y = 1 + 2 * as.numeric(fq) + stats::rnorm(length(fq), sd = 0.2))
  list(indicators = ind, target = target, f = f, truth = target$Y)
}

test_that("nowcast recovers the bridge relationship and handles the ragged edge", {
  sim <- simulate_nowcast_data()
  target <- sim$target
  nq <- nrow(target)
  target$Y[nq] <- NA

  full_quarter <- ek_nowcast(sim$indicators, target)
  expect_s3_class(full_quarter, "ek_nowcast")
  expect_gt(full_quarter$r_squared, 0.8)
  expect_equal(full_quarter$nowcast$quarter, target$date[nq])
  expect_equal(full_quarter$nowcast$months_observed, 3L)
  expect_lt(abs(full_quarter$nowcast$nowcast - sim$truth[nq]), 1)

  ind <- sim$indicators[seq_len(nrow(sim$indicators) - 2), ]
  ind$X4[nrow(ind)] <- NA
  nc <- ek_nowcast(ind, target)
  expect_equal(nc$nowcast$months_observed, 1L)
  expect_true(nc$nowcast$lower < nc$nowcast$nowcast && nc$nowcast$nowcast < nc$nowcast$upper)
  expect_gt(stats::cor(nc$factors$factor1, sim$f[seq_len(nrow(ind))], use = "complete"), 0.9)

  z <- scale(as.matrix(ind[2:5]))
  full <- stats::complete.cases(z)
  expect_equal(nc$factors$factor1[full], as.numeric(z[full, ] %*% nc$loadings[, 1]))
  expect_false(is.na(nc$factors$factor1[nrow(ind)]))
  expect_output(print(nc), "Nowcast")
})

test_that("nowcast validates inputs and runs on sample vintages", {
  sim <- simulate_nowcast_data()
  expect_error(ek_nowcast(sim$indicators, sim$target, n_factors = 4), "n_factors")
  expect_error(ek_nowcast(sim$indicators, cbind(sim$target, Z = 1)), "one value column")
  expect_error(ek_nowcast(sim$indicators[1:20, ], sim$target), "24 months")
  nc <- ek_nowcast(sim$indicators, sim$target)
  expect_equal(nrow(nc$nowcast), 0L)
  expect_output(print(nc), "No quarters")

  sample_nc <- ek_nowcast(vintage = "2020-05-15")
  expect_equal(sample_nc$target, "INDPRO")
  expect_true(all(sample_nc$nowcast$quarter >= as.Date("2020-01-01")))
  expect_gte(nrow(sample_nc$nowcast), 1L)
})
