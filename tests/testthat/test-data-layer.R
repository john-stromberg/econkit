test_that("catalog covers every sample series", {
  cat <- ek_series_catalog()
  expect_true(all(c("id", "title", "frequency", "transform") %in% names(cat)))
  expect_setequal(cat$id, setdiff(names(macro_monthly), "date"))
})

test_that("sample source subsets by id and date", {
  x <- ek_get_series(c("UNRATE", "T10Y3M"), start = "2000-01-01", end = "2000-12-31")
  expect_named(x, c("date", "UNRATE", "T10Y3M"))
  expect_equal(nrow(x), 12)
  expect_equal(attr(x, "source"), "sample")
  expect_error(ek_get_series("NOPE"), "not in sample data")
})

test_that("fred source fails clearly without an API key", {
  skip_if_not_installed("fredr")
  old_key <- Sys.getenv("FRED_API_KEY")
  Sys.setenv(FRED_API_KEY = "")
  on.exit(Sys.setenv(FRED_API_KEY = old_key))
  expect_error(ek_get_series("UNRATE", source = "fred"), "FRED_API_KEY")
})

test_that("frequency alignment aggregates daily data", {
  d <- data.frame(date = as.Date("2024-01-01") + 0:90, x = c(rep(1, 31), rep(2, 29), rep(3, 31)))
  m <- ek_align_frequency(d, "month")
  expect_equal(m$x, c(1, 2, 3))
  q <- ek_align_frequency(d, "quarter")
  expect_equal(q$date, as.Date("2024-01-01"))
  expect_equal(q$x, mean(d$x))
  expect_equal(ek_floor_date(as.Date("2024-11-30"), "quarter"), as.Date("2024-10-01"))
})

test_that("lags and transforms are correct", {
  expect_equal(ek_lag(1:5, 2), c(NA, NA, 1, 2, 3))
  expect_equal(ek_lag(1:5, -2), c(3, 4, 5, NA, NA))
  expect_equal(ek_lag(1:3, 5), rep(NA_integer_, 3))
  x <- 100 * 1.01^(0:24)
  expect_equal(ek_transform(x, "yoy")[13:25], rep(100 * (1.01^12 - 1), 13))
  expect_equal(ek_transform(x, "annualized")[2], 100 * (1.01^12 - 1))
  expect_equal(ek_transform(x, "diff", n = 1)[2], 1)
})
