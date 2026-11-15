local_vendor_source <- function(supports_vintage = TRUE, env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  csv <- file.path(dir, "vendor.csv")
  utils::write.csv(data.frame(
    source_id = c("HY.OAS", "HY.OAS", "HY.OAS", "HY.OAS"),
    date = c("2024-01-01", "2024-01-01", "2024-02-01", "2024-03-01"),
    value = c(3.0, 3.2, 3.5, 4.0),
    realtime_start = c("2024-02-03", "2024-03-10", "2024-03-04", "2024-04-03")
  ), csv, row.names = FALSE)
  registry <- file.path(dir, "vendor.yml")
  yaml::write_yaml(list(series = list(list(
    id = "VENDOR_HY_OAS", title = "Vendor high-yield OAS", units = "percent",
    frequency = "monthly", category = "credit", transform = "level", source = "vendor",
    source_id = "HY.OAS", aggregation = "mean", seasonal_adjustment = "NSA",
    release_lag_days = 3, license = "vendor", in_sample = FALSE
  ))), registry)
  withr::local_options(econkit.registry = registry, .local_envir = env)
  ek_register_source("vendor", ek_csv_fetcher(csv), # nolint: object_usage_linter
                     supports_vintage = supports_vintage,
                     description = "Test vendor", overwrite = TRUE)
  withr::defer(ek_unregister_source("vendor"), envir = env) # nolint: object_usage_linter
  csv
}

test_that("csv fetcher serves latest and point-in-time values", {
  csv <- local_vendor_source()
  fetch <- ek_csv_fetcher(csv)
  expect_equal(fetch("HY.OAS")$value, c(3.2, 3.5, 4.0))
  pit <- fetch("HY.OAS", as.Date("2024-03-05"))
  expect_equal(pit$date, as.Date(c("2024-01-01", "2024-02-01")))
  expect_equal(pit$value, c(3.0, 3.5))
  expect_error(fetch("NOPE"), "not found")
})

test_that("registered sources feed ek_get_series with registry rules", {
  local_vendor_source()
  expect_true("VENDOR_HY_OAS" %in% ek_series_catalog()$id)
  src <- ek_sources()
  expect_equal(src$n_series[src$name == "vendor"], 1L)
  expect_true(src$builtin[src$name == "fred"])

  x <- ek_get_series("VENDOR_HY_OAS", source = "vendor", vintage = "2024-03-05")
  expect_equal(x$VENDOR_HY_OAS, c(3.0, 3.5))
  prov <- ek_provenance(x)
  expect_equal(prov$vintage_method, "source_vintage")
  expect_false(prov$synthetic)
  expect_equal(ek_get_series("VENDOR_HY_OAS", source = "vendor",
                             frequency = "quarter")$VENDOR_HY_OAS, mean(c(3.2, 3.5, 4.0)))
  expect_error(ek_get_series("UNRATE", source = "vendor"), "not provided by source")
  expect_error(ek_get_series("VENDOR_HY_OAS", source = "nope"), "Unknown source")
})

test_that("adapters without vintages fall back to the release-lag approximation", {
  local_vendor_source(supports_vintage = FALSE)
  x <- ek_get_series("VENDOR_HY_OAS", source = "vendor", vintage = "2024-03-05")
  expect_equal(x$VENDOR_HY_OAS, c(3.2, 3.5, NA))
  expect_equal(ek_provenance(x)$vintage_method, "release_lag_approximation")
})

test_that("source registration validates its inputs", {
  expect_error(ek_register_source("fred", identity), "built-in")
  expect_error(ek_register_source("sample", identity), "built-in")
  expect_error(ek_register_source("bad name", identity), "identifier")
  expect_error(ek_register_source("ok", "not a function"), "function")
  ek_register_source("dup_test", function(source_id, vintage) NULL)
  on.exit(ek_unregister_source("dup_test"))
  expect_error(ek_register_source("dup_test", function(source_id, vintage) NULL),
               "already registered")

  entry <- list(id = "X", title = "x", units = "u", frequency = "monthly",
                category = "c", transform = "level", source = "vendor", source_id = "X",
                aggregation = "mean", seasonal_adjustment = "SA", release_lag_days = 5,
                license = "vendor", in_sample = TRUE)
  expect_error(ek_parse_catalog(list(series = list(entry))), "in_sample")
  entry$in_sample <- FALSE
  entry$source <- "bad source"
  expect_error(ek_parse_catalog(list(series = list(entry))), "data adapter")
})

test_that("adapter output is validated", {
  local_vendor_source()
  ek_register_source("vendor", function(source_id, vintage) data.frame(x = 1),
                     overwrite = TRUE)
  expect_error(ek_get_series("VENDOR_HY_OAS", source = "vendor"), "invalid data")
})
