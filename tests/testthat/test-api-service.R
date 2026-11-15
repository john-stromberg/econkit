test_that("API keys are enforced only when configured", {
  skip_if_not_installed("plumber")
  withr::local_envvar(ECONKIT_API_KEYS = "")
  router <- ek_test_router()
  expect_false(ek_test_call(router, "/health")$body$auth)
  expect_equal(ek_test_call(router, "/v1/sources")$status, 200L)

  withr::local_envvar(ECONKIT_API_KEYS = "alpha, beta")
  health <- ek_test_call(router, "/health")
  expect_equal(health$status, 200L)
  expect_true(health$body$auth)
  denied <- ek_test_call(router, "/v1/sources")
  expect_equal(denied$status, 401L)
  expect_match(denied$body$error, "API key")
  expect_equal(ek_test_call(router, "/v1/sources", key = "wrong")$status, 401L)
  expect_equal(ek_test_call(router, "/v1/sources", key = "beta")$status, 200L)
})

test_that("sources, calendar and nowcast routes return meta and data", {
  skip_if_not_installed("plumber")
  withr::local_envvar(ECONKIT_API_KEYS = "")
  router <- ek_test_router()

  sources <- ek_test_call(router, "/v1/sources")
  expect_true(all(c("sample", "fred") %in% sources$body$data$name))
  expect_equal(sources$body$meta$n_sources, nrow(sources$body$data))

  calendar <- ek_test_call(router, "/v1/calendar",
                           "ids=UNRATE,GDPC1&from=2025-01-01&to=2025-03-31")
  expect_equal(calendar$status, 200L)
  expect_true(all(calendar$body$data$expected_release >= "2025-01-01"))
  expect_true("2025-01-10" %in% calendar$body$data$expected_release)
  expect_equal(ek_test_call(router, "/v1/calendar", "from=notadate")$status, 400L)

  nowcast <- ek_test_call(router, "/v1/nowcast", "vintage=2020-05-15")
  expect_equal(nowcast$status, 200L)
  expect_equal(nowcast$body$meta$target, "INDPRO")
  expect_gt(nrow(nowcast$body$data), 0L)
  expect_true(all(c("quarter", "nowcast", "lower", "upper") %in% names(nowcast$body$data)))
  expect_equal(ek_test_call(router, "/v1/nowcast", "n_factors=9")$status, 400L)
})

test_that("series export returns files with provenance headers", {
  skip_if_not_installed("plumber")
  withr::local_envvar(ECONKIT_API_KEYS = "")
  router <- ek_test_router()
  query <- "ids=UNRATE,PAYEMS&start=2020-01-01&end=2020-12-01&vintage=2021-01-31"

  csv <- ek_test_call(router, "/v1/series/export", paste0(query, "&format=csv"))
  expect_equal(csv$status, 200L)
  expect_match(csv$headers[["Content-Type"]], "text/csv")
  expect_match(csv$headers[["Content-Disposition"]], "econkit-series.csv")
  expect_equal(csv$headers[["X-Econkit-Synthetic"]], "true")
  expect_equal(csv$headers[["X-Econkit-Vintage"]], "2021-01-31")
  data <- utils::read.csv(text = rawToChar(csv$raw))
  expect_setequal(unique(data$series_id), c("UNRATE", "PAYEMS"))
  expect_equal(nrow(data), 24L)

  if (requireNamespace("arrow", quietly = TRUE)) {
    parquet <- ek_test_call(router, "/v1/series/export", query)
    expect_equal(parquet$status, 200L)
    expect_equal(parquet$headers[["X-Econkit-Data-MD5"]], csv$headers[["X-Econkit-Data-MD5"]])
    path <- withr::local_tempfile(fileext = ".parquet")
    writeBin(parquet$raw, path)
    expect_equal(nrow(arrow::read_parquet(path)), 24L)
  }

  bad <- ek_test_call(router, "/v1/series/export", "format=xlsx")
  expect_equal(bad$status, 400L)
  expect_match(bad$body$error, "format")
})

test_that("forecast and scenario-build routes run conditional BVARs", {
  skip_if_not_installed("plumber")
  withr::local_envvar(ECONKIT_API_KEYS = "")
  router <- ek_test_router()

  body <- paste0('{"vars":["FEDFUNDS","UNRATE","INDPRO"],"p":2,"h":4,"draws":30,',
                 '"conditions":{"FEDFUNDS":[6,6,null,null]}}')
  forecast <- ek_test_call(router, "/v1/forecast", body = body)
  expect_equal(forecast$status, 200L)
  expect_equal(forecast$body$meta$vars, c("FEDFUNDS", "UNRATE", "INDPRO"))
  expect_true(forecast$body$meta$synthetic)
  expect_equal(nrow(forecast$body$data), 12L)
  fed <- forecast$body$data[forecast$body$data$variable == "FEDFUNDS", ]
  expect_equal(fed$central[1:2], c(6, 6), tolerance = 1e-8)
  expect_equal(fed$conditioned, c(TRUE, TRUE, FALSE, FALSE))
  again <- ek_test_call(router, "/v1/forecast", body = body)
  expect_equal(again$body$data, forecast$body$data)

  expect_equal(ek_test_call(router, "/v1/forecast", body = '{"h":100}')$status, 400L)
  unknown <- ek_test_call(router, "/v1/forecast", body = '{"horizon":4}')
  expect_equal(unknown$status, 400L)
  expect_match(unknown$body$error, "Unknown field")

  build <- paste0('{"vars":["FEDFUNDS","UNRATE","INDPRO"],"p":2,"h":6,"draws":20,',
                  '"version":"api-v1","scenarios":{"base":{},',
                  '"hike":{"FEDFUNDS":[7,7,7,7,7,7]}}}')
  scenarios <- ek_test_call(router, "/v1/scenarios/build", body = build)
  expect_equal(scenarios$status, 200L)
  expect_equal(scenarios$body$meta$scenarios, c("base", "hike"))
  expect_false(scenarios$body$meta$asset_shocks)
  paths <- scenarios$body$paths
  expect_equal(nrow(paths), 12L)
  expect_equal(unique(paths$version), "api-v1")
  expect_equal(paths$FEDFUNDS[paths$scenario_id == "hike"], rep(7, 6), tolerance = 1e-8)
  expect_equal(ek_test_call(router, "/v1/scenarios/build",
                            body = '{"scenarios":[]}')$status, 400L)
})

test_that("models and runs routes expose governance data", {
  skip_if_not_installed("plumber")
  log <- withr::local_tempfile(fileext = ".jsonl")
  withr::local_envvar(ECONKIT_API_KEYS = "", ECONKIT_RUN_LOG = "")
  router <- ek_test_router()

  models <- ek_test_call(router, "/v1/models")
  expect_equal(models$body$meta$n_models, nrow(ek_model_registry()))
  expect_true("bvar_minnesota" %in% models$body$data$id)
  one <- ek_test_call(router, "/v1/models", "id=nowcast_factor_bridge")
  expect_equal(nrow(one$body$data), 1L)
  expect_true(all(one$body$monitoring$model_id == "nowcast_factor_bridge"))
  expect_equal(ek_test_call(router, "/v1/models", "id=nope")$status, 404L)

  expect_equal(ek_test_call(router, "/v1/runs")$status, 400L)
  body <- '{"vars":["FEDFUNDS","UNRATE"],"p":1,"h":2,"draws":5}'
  expect_null(ek_test_call(router, "/v1/forecast", body = body)$body$meta$run_id)

  withr::local_envvar(ECONKIT_RUN_LOG = log)
  forecast <- ek_test_call(router, "/v1/forecast", body = body)
  expect_type(forecast$body$meta$run_id, "character")
  nowcast <- ek_test_call(router, "/v1/nowcast", "vintage=2020-05-15")
  runs <- ek_test_call(router, "/v1/runs")
  expect_equal(runs$body$meta$n_runs, 2L)
  expect_equal(runs$body$data$run_id, c(forecast$body$meta$run_id, nowcast$body$meta$run_id))
  expect_equal(runs$body$data$model_id, c("bvar_minnesota", "nowcast_factor_bridge"))
  expect_equal(ek_test_call(router, "/v1/runs", "model_id=bvar_minnesota")$body$meta$n_runs, 1L)
  logged <- ek_read_runs(log)
  expect_equal(jsonlite::fromJSON(logged$params[1])$vars, c("FEDFUNDS", "UNRATE"))
  expect_true(grepl("r_squared", logged$metrics[2], fixed = TRUE))
})
