test_that("yield curve API returns factors and fitted yields", {
  skip_if_not_installed("plumber")
  withr::local_envvar(ECONKIT_API_KEYS = "", ECONKIT_RUN_LOG = "")
  router <- ek_test_router()

  result <- ek_test_call(router, "/v1/rates/curve")
  expect_equal(result$status, 200L)
  expect_equal(result$body$meta$model_version, "nelson-siegel-v1")
  expect_true(result$body$meta$synthetic)
  expect_equal(nrow(result$body$factors), 672L)
  expect_equal(nrow(result$body$fitted), 3360L)
  expect_equal(result$body$meta$lambda, 0.6)
  expect_equal(ek_test_call(router, "/v1/rates/curve", "lambda=0")$status, 400L)
  expect_equal(
    ek_test_call(router, "/v1/rates/curve", "vintage=notadate")$status, 400L
  )
})

test_that("yield curve API records governed runs when enabled", {
  skip_if_not_installed("plumber")
  log <- withr::local_tempfile(fileext = ".jsonl")
  withr::local_envvar(ECONKIT_API_KEYS = "", ECONKIT_RUN_LOG = log)
  result <- ek_test_call(ek_test_router(), "/v1/rates/curve", "start=2020-01-01&end=2020-12-01")

  expect_equal(result$status, 200L)
  expect_type(result$body$meta$run_id, "character")
  runs <- ek_read_runs(log)
  expect_equal(runs$model_id, "yield_curve_nelson_siegel")
  expect_equal(runs$run_id, result$body$meta$run_id)
  expect_match(runs$metrics, "rmse")
})
