test_that("sample scenario library is versioned, monthly and synthetic", {
  scenarios <- ek_scenario_library(
    data = macro_monthly,
    as_of = as.Date("2018-12-01"),
    version = "test-v1"
  )

  expect_s3_class(scenarios, "ek_scenario_library")
  expect_true(scenarios$synthetic)
  expect_setequal(unique(scenarios$paths$scenario_id),
                  c("base", "adverse", "severe"))
  expect_equal(unique(scenarios$paths$version), "test-v1")
  expect_true(all(scenarios$paths$data_vintage == as.Date("2018-12-01")))
  expect_true(all(scenarios$paths$date > scenarios$paths$as_of))
  expect_equal(nrow(scenarios$paths), 36L)
  expect_equal(nrow(scenarios$asset_returns), 24L)
  expect_invisible(ek_validate_scenario_library(scenarios))

  custom <- ek_scenario_library(
    scenarios$paths, scenarios$asset_returns, synthetic = TRUE
  )
  expect_true(custom$synthetic)
})

test_that("SAA scenario stress applies weights to cumulative asset shocks", {
  scenarios <- ek_scenario_library()
  assets <- sort(unique(scenarios$asset_returns$asset_class))
  weights <- stats::setNames(rep(1 / length(assets), length(assets)), assets)
  stress <- ek_saa_scenario_stress(scenarios, weights)

  expect_true(stress$synthetic)
  expect_equal(nrow(stress$summary), 3L)
  expect_lt(
    stress$summary$portfolio_return[stress$summary$scenario_id == "severe"],
    stress$summary$portfolio_return[stress$summary$scenario_id == "adverse"]
  )
  expect_lt(
    stress$summary$portfolio_return[stress$summary$scenario_id == "adverse"],
    stress$summary$portfolio_return[stress$summary$scenario_id == "base"]
  )
  contributions <- aggregate(
    contribution ~ scenario_id, stress$contributions, sum
  )
  expect_equal(
    stress$summary$portfolio_return[
      match(contributions$scenario_id, stress$summary$scenario_id)
    ],
    contributions$contribution
  )
  expect_error(
    ek_saa_scenario_stress(scenarios, c(global_equity = 1)),
    "exactly the assets"
  )
})

test_that("scenario validation rejects broken monthly paths and shock keys", {
  scenarios <- ek_scenario_library()
  broken_path <- scenarios
  broken_path$paths$date[1] <- broken_path$paths$date[1] + 1
  expect_error(
    ek_validate_scenario_library(broken_path),
    "contiguous monthly steps"
  )

  broken_shocks <- scenarios
  broken_shocks$asset_returns <- broken_shocks$asset_returns[-1, ]
  expect_error(
    ek_validate_scenario_library(broken_shocks),
    "same asset classes"
  )
})

test_that("macro-only libraries accept any numeric macro columns", {
  scenarios <- ek_scenario_library()
  paths <- scenarios$paths[c("scenario_id", "version", "as_of", "data_vintage",
                             "step", "date", "UNRATE")]
  paths$BAA10Y <- 2 + paths$UNRATE / 10
  macro_only <- ek_scenario_library(paths)

  expect_equal(nrow(macro_only$asset_returns), 0L)
  expect_false(macro_only$synthetic)
  expect_error(ek_saa_scenario_stress(macro_only, c(global_equity = 1)),
               "asset-return shocks")
  expect_equal(nrow(ek_credit_scenario_stress(macro_only)$summary), 3L)

  no_unrate <- ek_scenario_library(paths[names(paths) != "UNRATE"])
  expect_error(ek_credit_scenario_stress(no_unrate), "UNRATE")

  expect_error(ek_scenario_library(paths[1:6]), "missing required columns")
  bad <- paths
  bad$UNRATE[1] <- -1
  expect_error(ek_scenario_library(bad), "invalid numeric values")
  bad <- paths
  bad$BAA10Y[2] <- NA
  expect_error(ek_scenario_library(bad), "invalid numeric values")
  expect_error(ek_scenario_library(asset_returns = scenarios$asset_returns),
               "requires `paths`")
})

test_that("BVAR forecasts become a macro scenario library", {
  fit <- ek_bvar(p = 2)
  base <- ek_bvar_forecast(fit, h = 6, draws = 20, seed = 1)
  hike <- ek_bvar_forecast(fit, h = 6, draws = 20, seed = 1,
                           conditions = list(FEDFUNDS = rep(8, 6)))
  lib <- ek_scenario_from_forecast(list(base = base, hike = hike), version = "t1")

  expect_s3_class(lib, "ek_scenario_library")
  expect_true(lib$synthetic)
  expect_equal(nrow(lib$paths), 12L)
  expect_true(all(c(fit$vars, "scenario_id", "step") %in% names(lib$paths)))
  expect_true(all(lib$paths$as_of == fit$last_date))
  hike_path <- lib$paths[lib$paths$scenario_id == "hike", ]
  expect_equal(hike_path$FEDFUNDS, rep(8, 6), tolerance = 1e-8)
  expect_equal(hike_path$UNRATE,
               hike$summary$central[hike$summary$variable == "UNRATE"])
  upper <- ek_scenario_from_forecast(list(base = base), path = "upper")
  expect_equal(upper$paths$UNRATE, base$summary$upper[base$summary$variable == "UNRATE"])
  expect_equal(nrow(ek_credit_scenario_stress(lib)$summary), 2L)

  short <- ek_bvar_forecast(fit, h = 3, draws = 5, seed = 1)
  expect_error(ek_scenario_from_forecast(list(base = base, short = short)),
               "end date and horizon")
  expect_error(ek_scenario_from_forecast(list(base)), "uniquely named")
  quarterly <- ek_bvar(ek_get_series(c("UNRATE", "FEDFUNDS"), frequency = "quarter"), p = 1)
  expect_error(
    ek_scenario_from_forecast(list(base = ek_bvar_forecast(quarterly, h = 2, draws = 5))),
    "monthly"
  )
})
