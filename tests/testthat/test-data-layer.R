test_that("catalog flags exactly the sample series and carries registry fields", {
  cat <- ek_series_catalog()
  expect_true(all(c("aggregation", "release_lag_days", "license", "in_sample") %in% names(cat)))
  expect_type(cat$in_sample, "logical")
  expect_setequal(cat$id[cat$in_sample], setdiff(names(macro_monthly), "date"))
  expect_setequal(names(ek_get_series())[-1], cat$id[cat$in_sample])
})

test_that("catalog validation rejects bad registry entries", {
  entry <- list(id = "X", title = "x", units = "u", frequency = "monthly",
                category = "c", transform = "level", source = "fred", source_id = "X",
                aggregation = "mean", seasonal_adjustment = "SA", release_lag_days = 5,
                license = "public", in_sample = FALSE)
  expect_equal(nrow(ek_parse_catalog(list(series = list(entry)))), 1L)
  expect_error(ek_parse_catalog(list(series = list(entry, entry))), "duplicate id")
  bad <- entry
  bad$aggregation <- "median"
  expect_error(ek_parse_catalog(list(series = list(bad))), "aggregation")
  bad <- entry
  bad$release_lag_days <- NULL
  expect_error(ek_parse_catalog(list(series = list(bad))), "release_lag_days")
})

test_that("sample source subsets by id and date", {
  x <- ek_get_series(c("UNRATE", "T10Y3M"), start = "2000-01-01", end = "2000-12-31")
  expect_named(x, c("date", "UNRATE", "T10Y3M"))
  expect_equal(nrow(x), 12)
  expect_equal(attr(x, "source"), "sample")
  expect_error(ek_get_series("NOPE"), "Unknown series")
  expect_error(ek_get_series("GDPC1"), "not in sample data")
  expect_error(ek_get_series("UNRATE", vintage = "not-a-date"), "vintage")
})

test_that("quarterly output applies each series' aggregation rule", {
  m <- ek_get_series(c("UNRATE", "PAYEMS", "USREC"), start = "2008-01-01", end = "2008-06-30")
  q <- ek_get_series(c("UNRATE", "PAYEMS", "USREC"), frequency = "quarter",
                     start = "2008-01-01", end = "2008-06-30")
  expect_equal(q$date, as.Date(c("2008-01-01", "2008-04-01")))
  expect_equal(q$UNRATE, c(mean(m$UNRATE[1:3]), mean(m$UNRATE[4:6])))
  expect_equal(q$PAYEMS, m$PAYEMS[c(3, 6)])
  expect_equal(q$USREC, c(max(m$USREC[1:3]), max(m$USREC[4:6])))
})

test_that("sample vintages mask values by release lag and drop whole periods", {
  x <- ek_get_series(c("UNRATE", "PCEPILFE"), vintage = "2020-06-30")
  expect_equal(max(x$date), as.Date("2020-06-01"))
  last_obs <- function(v) max(x$date[!is.na(x[[v]])])
  expect_equal(last_obs("UNRATE"), as.Date("2020-05-01"))
  expect_equal(last_obs("PCEPILFE"), as.Date("2020-04-01"))

  q <- ek_get_series("UNRATE", frequency = "quarter", vintage = "2020-06-30")
  expect_equal(max(q$date[!is.na(q$UNRATE)]), as.Date("2020-01-01"))
  expect_true(is.na(q$UNRATE[q$date == as.Date("2020-04-01")]))
})

test_that("provenance and long format describe the extract", {
  x <- ek_get_series(c("UNRATE", "FEDFUNDS"), start = "2020-01-01", vintage = "2021-01-15")
  prov <- ek_provenance(x)
  expect_true(prov$synthetic)
  expect_equal(prov$vintage, "2021-01-15")
  expect_equal(prov$vintage_method, "release_lag_approximation")
  expect_equal(prov$series$id, c("UNRATE", "FEDFUNDS"))
  expect_equal(ek_provenance(ek_get_series("UNRATE"))$vintage_method, "latest")
  expect_error(ek_provenance(data.frame()), "no provenance")

  long <- ek_series_long(x)
  expect_named(long, c("date", "series_id", "value", "units", "frequency", "source",
                       "vintage", "vintage_method"))
  expect_equal(nrow(long), sum(!is.na(x$UNRATE)) + sum(!is.na(x$FEDFUNDS)))
  expect_equal(unique(long$units[long$series_id == "UNRATE"]), "percent")
  expect_gt(nrow(ek_series_long(x, drop_na = FALSE)), nrow(long))
})

test_that("fred source uses registry rules and ALFRED vintages", {
  skip_if_not_installed("fredr")
  withr::local_envvar(FRED_API_KEY = "test-key", ECONKIT_CACHE = withr::local_tempdir())
  calls <- list()
  local_mocked_bindings(ek_fred_fetch = function(source_id, vintage = NULL) {
    calls[[length(calls) + 1]] <<- list(id = source_id, vintage = vintage)
    if (source_id == "GDPC1") {
      return(data.frame(date = as.Date(c("2024-01-01", "2024-04-01")), value = c(100, 101)))
    }
    data.frame(date = as.Date("2024-01-01") + 0:90, value = as.numeric(1:91))
  })

  x <- ek_get_series(c("DGS2", "GDPC1"), source = "fred", vintage = "2024-07-31")
  expect_equal(calls[[1]]$vintage, as.Date("2024-07-31"))
  expect_equal(x$DGS2, c(mean(1:31), mean(32:60), mean(61:91), NA))
  expect_equal(x$GDPC1, c(100, NA, NA, 101))
  expect_equal(ek_provenance(x)$vintage_method, "alfred")
  expect_false(ek_provenance(x)$synthetic)

  q <- ek_get_series(c("DGS2", "GDPC1"), source = "fred", frequency = "quarter",
                     vintage = "2024-07-31")
  expect_length(calls, 2L)
  expect_equal(q$DGS2, c(mean(1:91), NA))
  expect_equal(q$GDPC1, c(100, 101))

  ek_get_series("DGS2", source = "fred")
  expect_length(calls, 3L)
  expect_null(calls[[3]]$vintage)
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
