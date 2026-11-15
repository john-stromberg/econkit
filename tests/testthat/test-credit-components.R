test_that("sample LGD and EAD satellites project bounded loss components", {
  fit <- ek_credit_lgd_ead_satellite_fit()
  scenarios <- ek_scenario_library()
  projected <- ek_credit_lgd_ead_project(fit, scenarios)

  expect_s3_class(fit, "ek_credit_lgd_ead_satellites")
  expect_true(fit$synthetic)
  expect_equal(fit$drivers, c("UNRATE", "NFCI"))
  expect_equal(fit$segments,
               c("commercial_industrial", "commercial_real_estate", "consumer"))
  expect_equal(fit$last_date, as.Date("2025-12-01"))
  expect_equal(nrow(projected), 36L * 3L)
  expect_true(all(projected$lgd > 0 & projected$lgd < 1))
  expect_true(all(projected$ead_multiplier > 0))
  expect_true(all(attr(projected, "synthetic")))
})

test_that("credit stress applies scenario LGD and EAD along monthly paths", {
  scenarios <- ek_scenario_library()
  fit <- ek_credit_lgd_ead_satellite_fit()
  portfolio <- data.frame(
    segment = c("commercial_industrial", "consumer"),
    ead = c(200, 100), pd_12m = c(0.02, 0.04), lgd = c(0.45, 0.6),
    unemployment_beta = c(0.15, 0.25), stringsAsFactors = FALSE
  )
  result <- ek_credit_scenario_stress(scenarios, portfolio, component_fit = fit)

  expect_equal(result$component_fit_version, "credit-lgd-ead-satellite-v1")
  expect_true(result$component_fit_synthetic)
  expect_equal(nrow(result$component_paths), 36L * 2L)
  paths <- result$component_paths
  expect_equal(paths$expected_loss,
               paths$marginal_pd * paths$lgd * paths$ead)
  severe <- result$summary[result$summary$scenario_id == "severe", ]
  severe_paths <- result$component_paths[result$component_paths$scenario_id == "severe", ]
  expect_equal(severe$expected_loss, sum(severe_paths$expected_loss))
  expect_true(all(result$segments$mean_ead > 0))
  expect_true(all(result$segments$mean_lgd > 0 & result$segments$mean_lgd < 1))
  expect_named(
    ek_credit_scenario_stress(scenarios, portfolio),
    c("model_version", "reference_scenario", "scenario_synthetic", "portfolio_source",
      "summary", "segments")
  )
})

test_that("LGD and EAD calibration validates histories and scenario drivers", {
  history <- ek_load_dataset("credit_loss_components")
  macro <- ek_get_series(c("UNRATE", "NFCI"))
  bad <- history
  bad$lgd[1] <- 1
  expect_error(ek_credit_lgd_ead_satellite_fit(bad, macro), "strictly between")
  bad <- history
  bad$ead_index[1] <- 0
  expect_error(ek_credit_lgd_ead_satellite_fit(bad, macro), "positive and finite")
  expect_error(
    ek_credit_lgd_ead_satellite_fit(rbind(history, history[1, ]), macro),
    "one row per date and segment"
  )
  expect_error(
    ek_credit_lgd_ead_satellite_fit(history, macro[c("date", "UNRATE")]),
    "NFCI"
  )
  fit <- ek_credit_lgd_ead_satellite_fit()
  expect_error(ek_credit_lgd_ead_project(list(), ek_scenario_library()),
               "must be an ek_credit_lgd_ead_satellites")
  paths <- ek_scenario_library()$paths
  expect_error(ek_credit_lgd_ead_project(fit, paths[setdiff(names(paths), "NFCI")]),
               "need columns")
  expect_error(
    ek_credit_scenario_stress(
      ek_scenario_library(), component_fit = fit,
      portfolio = data.frame(segment = "unknown", ead = 100, pd_12m = 0.02,
                             lgd = 0.4, unemployment_beta = 0.2)
    ),
    "missing portfolio segment"
  )
})
