test_that("registry transforms follow the catalog and handle quarterly gaps", {
  x <- ek_get_series(c("PAYEMS", "UNRATE"), start = "2018-01-01", end = "2019-12-31")
  tx <- ek_apply_transforms(x)
  expect_equal(tx$PAYEMS, ek_transform(x$PAYEMS, "yoy"))
  expect_equal(tx$UNRATE, x$UNRATE)
  expect_equal(attr(tx, "transforms")[["PAYEMS"]], "yoy")
  expect_equal(ek_apply_transforms(x, c(UNRATE = "diff"))$UNRATE, ek_transform(x$UNRATE, "diff"))
  expect_error(ek_apply_transforms(x, c(NOPE = "diff")), "not in data")

  q <- data.frame(date = seq(as.Date("2020-01-01"), by = "month", length.out = 7),
                  GDPC1 = c(100, NA, NA, 102, NA, NA, 103))
  expect_equal(ek_apply_transforms(q)$GDPC1[c(4, 7)], 100 * ((c(102 / 100, 103 / 102))^4 - 1))
})

test_that("Taylor rule matches the formula", {
  d <- data.frame(date = seq(as.Date("2020-01-01"), by = "month", length.out = 3),
                  FEDFUNDS = c(1, 2, 3), PCE = c(2, 3, 4), UNRATE = c(4, 5, 6))
  base <- 1 + d$PCE + 0.5 * (d$PCE - 2) - 0.5 * 2 * (d$UNRATE - 4)
  tr <- ek_taylor_rule(d, inflation = "PCE", inflation_transform = "level", r_star = 1,
                       pi_star = 2, u_star = 4, inflation_weight = 0.5, gap_weight = 0.5, okun = 2)
  expect_equal(tr$prescribed_rate, base)
  expect_equal(tr$policy_gap, d$FEDFUNDS - base)

  inert <- ek_taylor_rule(d, inflation = "PCE", inflation_transform = "level", u_star = 4,
                          inertia = 0.8, elb = 0.5)
  expect_equal(inert$prescribed_rate, pmax(c(NA, 0.8 * d$FEDFUNDS[1:2] + 0.2 * base[2:3]), 0.5))
  expect_error(ek_taylor_rule(d[c("date", "PCE", "UNRATE")], inflation = "PCE",
                              inertia = 0.5), "FEDFUNDS")
  expect_error(ek_taylor_rule(d, inflation = "PCE", inertia = 1), "inertia")

  sample_tr <- ek_taylor_rule()
  expect_named(sample_tr, c("date", "inflation", "unemployment_gap", "prescribed_rate",
                            "policy_rate", "policy_gap"))
  expect_true(any(is.finite(sample_tr$prescribed_rate)))
})

test_that("surprise index is signed, standardized without look-ahead and smoothed", {
  set.seed(1)
  n <- 80
  d <- data.frame(date = seq(as.Date("2010-01-01"), by = "month", length.out = n),
                  A = cumsum(rnorm(n)), UNRATE = 5 + cumsum(rnorm(n, sd = 0.1)))
  s <- ek_surprise_index(d, window = 24, min_obs = 12, half_life = 0, transform = FALSE)
  t <- 40
  expect_equal(s$A[t], (d$A[t] - d$A[t - 1]) / stats::sd(diff(d$A)[(t - 25):(t - 2)]))
  expect_equal(s$UNRATE[t], -(d$UNRATE[t] - d$UNRATE[t - 1]) /
                 stats::sd(diff(d$UNRATE)[(t - 25):(t - 2)]))
  expect_equal(s$surprise_index[t], mean(c(s$A[t], s$UNRATE[t])))
  expect_true(all(is.na(s$surprise_index[1:13])))

  truncated <- ek_surprise_index(d[1:50, ], window = 24, min_obs = 12, transform = FALSE)
  full <- ek_surprise_index(d, window = 24, min_obs = 12, transform = FALSE)
  expect_equal(truncated$surprise_index, full$surprise_index[1:50])

  smooth <- ek_surprise_index(d, window = 24, min_obs = 12, half_life = 2, transform = FALSE)
  first <- which(!is.na(s$surprise_index))[1]
  expect_equal(smooth$surprise_index[first + 1],
               sqrt(0.5) * s$surprise_index[first] +
                 (1 - sqrt(0.5)) * s$surprise_index[first + 1])

  trend <- ek_surprise_index(d, expectation = "trend", window = 24, min_obs = 12,
                             half_life = 0, transform = FALSE)
  expect_false(isTRUE(all.equal(trend$A, s$A)))
  expect_error(ek_surprise_index(d, signs = c(A = 2)), "signs")
  expect_true(all(c("PAYEMS", "INDPRO", "UNRATE") %in% names(ek_surprise_index())))
})
