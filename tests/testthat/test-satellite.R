test_that("satellite fit recovers known coefficients", {
  set.seed(42)
  n <- 120
  data <- data.frame(date = seq(as.Date("2010-01-01"), by = "month", length.out = n),
                     u = 4 + cumsum(stats::rnorm(n, 0, 0.2)), f = stats::rnorm(n))
  data$y <- 1 + 0.5 * data$u - 0.3 * data$f + stats::rnorm(n, 0, 0.01)
  fit <- ek_satellite_fit(data, "y", c("u", "f"))

  expect_s3_class(fit, "ek_satellite")
  expect_equal(unname(fit$coefficients), c(1, 0.5, -0.3), tolerance = 0.02)
  expect_gt(fit$r_squared, 0.99)
  expect_equal(fit$freq, 12L)
  ref <- summary(stats::lm(y ~ u + f, data))$coefficients
  expect_equal(unname(fit$std_error), unname(ref[, 2]), tolerance = 1e-8)

  ar_data <- data
  for (i in 2:n) ar_data$y[i] <- 0.2 + 0.3 * data$u[i] + 0.6 * ar_data$y[i - 1]
  ar_fit <- ek_satellite_fit(ar_data, "y", "u", ar = TRUE)
  expect_equal(unname(ar_fit$coefficients), c(0.2, 0.3, 0.6), tolerance = 1e-6)
  expect_equal(names(ar_fit$coefficients)[3], "y_l1")

  expect_error(ek_satellite_fit(data, "y", "y"), "distinct")
  expect_error(ek_satellite_fit(data[1:5, ], "y", c("u", "f")), "Too few")
  data$g <- 2 * data$u
  expect_error(ek_satellite_fit(data, "y", c("u", "g")), "collinear")
})

test_that("satellite projections follow scenario paths", {
  scenarios <- ek_scenario_library()
  data <- ek_get_series(c("DGS10", "UNRATE", "NFCI"))
  fit <- ek_satellite_fit(data, "DGS10", c("UNRATE", "NFCI"))
  proj <- ek_satellite_project(fit, scenarios)
  expect_equal(nrow(proj), nrow(scenarios$paths))
  base <- scenarios$paths[scenarios$paths$scenario_id == "base", ]
  base <- base[order(base$step), ]
  expect_equal(proj$DGS10[proj$scenario_id == "base"],
               unname(fit$coefficients[1] + fit$coefficients[2] * base$UNRATE +
                        fit$coefficients[3] * base$NFCI))

  ar_fit <- ek_satellite_fit(data, "DGS10", "UNRATE", ar = TRUE)
  ar_proj <- ek_satellite_project(ar_fit, scenarios, y0 = 2)
  first <- ar_proj[ar_proj$scenario_id == "base", ][1:2, ]
  b <- unname(ar_fit$coefficients)
  expect_equal(first$DGS10[1], b[1] + b[2] * base$UNRATE[1] + b[3] * 2)
  expect_equal(first$DGS10[2], b[1] + b[2] * base$UNRATE[2] + b[3] * first$DGS10[1])

  quarterly <- ek_get_series(c("TB3MS", "UNRATE"), frequency = "quarter")
  q_fit <- ek_satellite_fit(quarterly, "TB3MS", "UNRATE")
  expect_equal(q_fit$freq, 4L)
  q_proj <- ek_satellite_project(q_fit, scenarios)
  q_base <- q_proj[q_proj$scenario_id == "base", ]
  expect_equal(nrow(q_base), 4L)
  expect_equal(q_base$date, base$date[c(3, 6, 9, 12)])
  expect_equal(q_base$TB3MS[1],
               unname(q_fit$coefficients[1] + q_fit$coefficients[2] * mean(base$UNRATE[1:3])))

  expect_error(ek_satellite_project(fit, scenarios$paths[c("scenario_id", "UNRATE")]),
               "need columns")
  expect_error(ek_satellite_project(q_fit, scenarios$paths[scenarios$paths$step <= 4, ]),
               "multiple of three")
})
