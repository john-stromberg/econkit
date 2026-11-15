test_that("catalog and series API routes return meta and long data", {
  skip_if_not_installed("plumber")
  api <- system.file("plumber", "api.R", package = "econkit")
  router <- plumber::pr(api)
  get <- function(path, query = "") {
    req <- new.env()
    req$REQUEST_METHOD <- "GET"
    req$PATH_INFO <- path
    req$QUERY_STRING <- query
    req$rook.input <- list(
      read = function() raw(0),
      rewind = function() invisible(NULL),
      read_lines = function() character()
    )
    response <- router$call(req)
    body <- response$body
    if (!is.raw(body)) body <- charToRaw(paste(body, collapse = ""))
    list(status = response$status, text = rawToChar(body),
         body = plumber::parser_json()(body, "application/json"))
  }

  catalog <- get("/v1/catalog")
  expect_equal(catalog$status, 200L)
  expect_equal(catalog$body$meta$n_series, nrow(ek_series_catalog()))
  expect_true("in_sample" %in% names(catalog$body$data))

  series <- get("/v1/series", paste0(
    "ids=UNRATE,PAYEMS&frequency=quarter&start=2019-01-01&vintage=2020-06-30"
  ))
  expect_equal(series$status, 200L)
  expect_true(series$body$meta$synthetic)
  expect_equal(series$body$meta$vintage_method, "release_lag_approximation")
  expect_setequal(unique(series$body$data$series_id), c("UNRATE", "PAYEMS"))
  expect_equal(max(series$body$data$date), "2020-01-01")
  expect_equal(series$body$meta$n_obs, nrow(series$body$data))
  expect_match(series$text, "\"vintage_method\":\"release_lag_approximation\"", fixed = TRUE)

  bad <- get("/v1/series", "ids=NOPE")
  expect_equal(bad$status, 400L)
  expect_match(bad$body$error, "Unknown series")
  expect_equal(get("/v1/series", "frequency=week")$status, 400L)
})
