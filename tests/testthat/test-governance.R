test_that("model registry lists models with valid entry points and thresholds", {
  models <- ek_model_registry()
  expect_true(all(c("bvar_minnesota", "credit_scenario_stress", "satellite_ols") %in% models$id))
  expect_false(anyDuplicated(models$id) > 0)
  expect_true(all(vapply(models[["function"]], exists, logical(1),
                         envir = asNamespace("econkit"))))
  expect_true(all(models$risk_tier %in% c("low", "medium", "high")))
  expect_s3_class(models$last_review, "Date")

  monitoring <- ek_model_registry(monitoring = TRUE)
  expect_true(all(monitoring$model_id %in% models$id))
  expect_equal(sum(models$n_metrics), nrow(monitoring))

  extra <- withr::local_tempfile(fileext = ".yml")
  writeLines(c(
    "models:",
    "  - id: bvar_minnesota",
    "    name: Bank BVAR", "    function: ek_bvar", "    version: \"2.0.0\"",
    "    category: forecasting", "    risk_tier: high", "    status: validated",
    "    owner: Model risk", "    intended_use: CCAR scenario design",
    "    last_review: \"2025-06-30\"", "    monitoring: []",
    "  - id: deposit_beta",
    "    name: Deposit beta", "    function: ek_satellite_fit", "    version: \"1.0.0\"",
    "    category: credit", "    risk_tier: medium", "    status: prototype",
    "    owner: Treasury", "    intended_use: NII scenarios",
    "    monitoring:", "      - metric: r_squared", "        lower: 0.5"
  ), extra)
  custom <- ek_model_registry(path = extra)
  expect_equal(custom$version[custom$id == "bvar_minnesota"], "2.0.0")
  expect_equal(custom$last_review[custom$id == "bvar_minnesota"], as.Date("2025-06-30"))
  expect_true("deposit_beta" %in% custom$id)
  expect_equal(nrow(custom), nrow(models) + 1L)

  bad <- withr::local_tempfile(fileext = ".yml")
  writeLines(c("models:", "  - id: x", "    name: X", "    function: f", "    version: v",
               "    category: macro", "    risk_tier: extreme", "    status: prototype",
               "    owner: o", "    intended_use: u"), bad)
  expect_error(ek_model_registry(path = bad), "risk_tier")
})

test_that("run lineage records hashes, provenance and metrics", {
  log <- withr::local_tempfile(fileext = ".jsonl")
  expect_equal(nrow(ek_read_runs(log)), 0L)

  data <- ek_get_series(c("FEDFUNDS", "UNRATE"), vintage = "2020-06-30")
  again <- ek_get_series(c("FEDFUNDS", "UNRATE"), vintage = "2020-06-30")
  expect_equal(ek_object_md5(data), ek_object_md5(again))
  expect_false(identical(ek_object_md5(data), ek_object_md5(data[-1, ])))

  fit <- ek_bvar(data, p = 2)
  fc <- ek_bvar_forecast(fit, h = 3, draws = 10, seed = 1)
  first <- ek_record_run("bvar_minnesota", inputs = data, outputs = fc$summary,
                         params = list(p = 2, h = 3), metrics = c(rmse = 0.4),
                         user = "analyst", note = "test", log = log)
  second <- ek_record_run("bvar_minnesota", inputs = fit, outputs = fc$summary, log = log)
  expect_false(identical(first$run_id, second$run_id))

  runs <- ek_read_runs(log)
  expect_equal(nrow(runs), 2L)
  expect_equal(runs$run_id, c(first$run_id, second$run_id))
  expect_equal(runs$model_version[1],
               ek_model_registry()$version[ek_model_registry()$id == "bvar_minnesota"])
  expect_equal(runs$input_md5[1], ek_object_md5(data))
  expect_equal(runs$output_md5[1], runs$output_md5[2])
  expect_true(runs$synthetic[1])
  expect_equal(runs$sources[1], "sample")
  expect_equal(runs$vintages[1], "2020-06-30")
  expect_true(runs$synthetic[2])
  expect_equal(runs$user, c("analyst", NA))
  expect_equal(jsonlite::fromJSON(runs$params[1]), list(p = 2L, h = 3L))
  expect_s3_class(runs$timestamp, "POSIXct")
  expect_equal(nrow(ek_read_runs(log, model_id = "nowcast_factor_bridge")), 0L)
  expect_equal(nrow(ek_read_runs(log, since = Sys.time() + 3600)), 0L)
  expect_equal(nrow(ek_read_runs(log, since = Sys.time() - 3600)), 2L)

  expect_error(ek_record_run("unknown", log = log), "model id")
  expect_error(ek_record_run("bvar_minnesota", metrics = 1, log = log), "named numeric")
})

test_that("monitoring flags breaches and drift", {
  metrics <- data.frame(
    model_id = rep(c("nowcast_factor_bridge", "bvar_minnesota"), each = 6),
    metric = rep(c("r_squared", "rmse"), each = 6),
    date = rep(seq(as.Date("2024-01-01"), by = "month", length.out = 6), 2),
    value = c(0.62, 0.58, 0.6, 0.61, 0.59, 0.25, 0.40, 0.42, 0.41, 0.39, 0.40, 0.90)
  )
  result <- ek_monitor_metrics(metrics)
  expect_equal(result$status, c("breach", "drift"))
  nowcast <- result[result$model_id == "nowcast_factor_bridge", ]
  expect_equal(nowcast$value, 0.25)
  expect_equal(nowcast$lower, 0.3)
  expect_equal(nowcast$n_history, 5L)
  expect_equal(nowcast$date, as.Date("2024-06-01"))
  bvar <- result[result$model_id == "bvar_minnesota", ]
  expect_gt(bvar$z_score, 3)
  expect_false(bvar$breach)

  calm <- metrics
  calm$value[6] <- 0.6
  calm$value[12] <- 0.41
  expect_true(all(ek_monitor_metrics(calm)$status == "ok"))
  custom <- data.frame(model_id = "bvar_minnesota", metric = "rmse", lower = NA,
                       upper = 0.5, drift_z = NA)
  with_custom <- ek_monitor_metrics(metrics, custom)
  expect_equal(with_custom$status[with_custom$model_id == "bvar_minnesota"], "breach")
  expect_equal(with_custom$status[with_custom$model_id == "nowcast_factor_bridge"], "ok")
  expect_true(is.na(ek_monitor_metrics(metrics[1:2, ])$z_score))

  log <- withr::local_tempfile(fileext = ".jsonl")
  for (v in c(0.6, 0.62, 0.61, 0.2)) {
    ek_record_run("satellite_ols", metrics = c(r_squared = v), log = log)
  }
  from_runs <- ek_monitor_metrics(ek_read_runs(log))
  expect_equal(from_runs$status, "breach")
  expect_true("timestamp" %in% names(from_runs))
  expect_error(ek_monitor_metrics(data.frame(x = 1)), "model_id")
})
