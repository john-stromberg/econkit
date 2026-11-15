test_that("csv export writes long data with a provenance sidecar", {
  dir <- withr::local_tempdir()
  x <- ek_get_series(c("UNRATE", "FEDFUNDS"), start = "2020-01-01", end = "2020-12-31",
                     vintage = "2021-01-15")
  path <- file.path(dir, "out", "macro.csv")
  out <- ek_export_series(x, path, format = "csv")
  expect_true(file.exists(path))
  back <- utils::read.csv(path, stringsAsFactors = FALSE)
  expect_equal(nrow(back), out$n_rows)
  expect_equal(back$value, ek_series_long(x)$value)

  prov <- jsonlite::read_json(out$provenance_path)
  expect_equal(prov$vintage_method, "release_lag_approximation")
  expect_equal(prov$data_md5, out$data_md5)
  expect_equal(vapply(prov$series, `[[`, "", "id"), c("UNRATE", "FEDFUNDS"))
  expect_length(list.files(file.path(dir, "out"), all.files = TRUE, no.. = TRUE), 2L)

  expect_error(ek_export_series(x, path, format = "csv"), "overwrite")
  expect_silent(ek_export_series(x, path, format = "csv", overwrite = TRUE))
})

test_that("parquet export embeds provenance and matches the csv checksum", {
  skip_if_not_installed("arrow")
  dir <- withr::local_tempdir()
  x <- ek_get_series("UNRATE", start = "2024-01-01")
  pq <- ek_export_series(x, file.path(dir, "macro.parquet"))
  csv <- ek_export_series(x, file.path(dir, "macro.csv"), format = "csv")
  expect_equal(pq$data_md5, csv$data_md5)
  tbl <- arrow::read_parquet(pq$path, as_data_frame = FALSE)
  prov <- jsonlite::fromJSON(tbl$metadata$econkit_provenance)
  expect_equal(prov$data_md5, pq$data_md5)
  expect_true(prov$synthetic)
  expect_s3_class(as.data.frame(tbl)$date, "Date")
})

test_that("release calendar matches the point-in-time masking rule", {
  cal <- ek_release_calendar(c("UNRATE", "GDPC1"), from = "2025-01-01", to = "2025-03-31")
  un <- cal[cal$series_id == "UNRATE", ]
  expect_equal(un$period, as.Date(c("2024-12-01", "2025-01-01", "2025-02-01")))
  expect_equal(un$expected_release, as.Date(c("2025-01-10", "2025-02-10", "2025-03-10")))
  gdp <- cal[cal$series_id == "GDPC1", ]
  expect_equal(gdp$period, as.Date("2024-10-01"))
  expect_equal(gdp$expected_release, as.Date("2025-02-04"))
  expect_true(all(cal$basis == "registry_release_lag"))
  expect_false(is.unsorted(cal$expected_release))

  jan <- function(v) ek_get_series("UNRATE", vintage = v, start = "2025-01-01")$UNRATE[1]
  expect_false(is.na(jan("2025-02-10")))
  expect_true(is.na(jan("2025-02-09")))

  q <- ek_release_calendar("UNRATE", from = "2025-01-01", to = "2025-03-31",
                           frequency = "quarter")
  expect_equal(q$expected_release, as.Date("2025-01-10"))
  expect_equal(nrow(ek_release_calendar("UNRATE", from = "2025-01-11", to = "2025-01-12")), 0L)
  expect_error(ek_release_calendar("UNRATE", from = "2025-02-01", to = "2025-01-01"), "from")
})
