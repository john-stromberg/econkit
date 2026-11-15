test_that("sample Treasury curves fit descriptive Nelson-Siegel factors", {
  curve <- ek_yield_curve_fit()

  expect_s3_class(curve, "ek_yield_curve")
  expect_true(curve$synthetic)
  expect_equal(curve$source, "sample")
  expect_equal(curve$model_version, "nelson-siegel-v1")
  expect_equal(nrow(curve$factors), 672L)
  expect_equal(nrow(curve$fitted), 672L * 5L)
  expect_setequal(unique(curve$fitted$tenor), names(curve$tenors))
  expect_true(all(is.finite(curve$factors$level)))
  expect_true(all(is.finite(curve$factors$slope)))
  expect_true(all(is.finite(curve$factors$curvature)))
  expect_lt(mean(curve$factors$rmse), 0.02)

  first <- curve$fitted[curve$fitted$date == curve$factors$date[1], ]
  expect_equal(first$fitted, first$observed, tolerance = 0.02)
})

test_that("curve fit handles incomplete dates and preserves provenance", {
  data <- ek_get_series(c("DGS3MO", "DGS2", "DGS5", "DGS10", "DGS30"),
                        vintage = "2020-06-30")
  curve <- ek_yield_curve_fit(data)
  expect_true(curve$synthetic)
  expect_equal(curve$data_vintage, "2020-06-30")
  expect_equal(
    curve$dropped_rows,
    sum(rowSums(is.finite(as.matrix(data[, -1, drop = FALSE]))) < 3L)
  )

  partial <- data.frame(
    date = as.Date(c("2020-01-01", "2020-02-01")),
    a = c(1, 1), b = c(2, 2), c = c(3, NA_real_)
  )
  fit <- ek_yield_curve_fit(partial, c(a = 0.25, b = 2, c = 10))
  expect_equal(nrow(fit$factors), 1L)
  expect_equal(fit$dropped_rows, 1L)
  expect_equal(nrow(fit$fitted), 3L)
  reversed <- ek_yield_curve_fit(data[rev(seq_len(nrow(data))), ])
  expect_equal(reversed$factors$date, sort(curve$factors$date))
})

test_that("curve fit validates maturities and yield data", {
  tenors <- c(DGS3MO = 0.25, DGS2 = 2, DGS5 = 5)
  data <- ek_get_series(names(tenors))
  expect_error(ek_yield_curve_fit(data, c(a = 1, b = 1, c = 3)), "distinct")
  expect_error(ek_yield_curve_fit(data, c(a = 0, b = 2, c = 3)), "positive")
  expect_error(ek_yield_curve_fit(data, c(a = 1, b = 2)), "at least three")
  expect_error(ek_yield_curve_fit(data, lambda = 0), "positive")
  expect_error(ek_yield_curve_fit(data, lambda = NA_real_), "positive")

  bad <- data
  bad$DGS2 <- as.character(bad$DGS2)
  expect_error(ek_yield_curve_fit(bad, tenors), "numeric finite")
  bad <- data
  bad$DGS2[1] <- Inf
  expect_error(ek_yield_curve_fit(bad, tenors), "numeric finite")
  bad <- data
  bad$date[2] <- bad$date[1]
  expect_error(ek_yield_curve_fit(bad, tenors), "unique")

  sparse <- data[1, ]
  sparse$DGS5 <- NA_real_
  expect_error(ek_yield_curve_fit(sparse, tenors), "at least three finite yields")
})
