#' Stress a credit portfolio using macro scenarios
#'
#' Calculates illustrative scenario-specific credit losses for homogeneous
#' portfolio segments. A segment's baseline 12-month probability of default
#' is converted to a constant monthly hazard. The hazard is multiplied by
#' `exp(unemployment_beta * unemployment_difference)` in each month, where
#' the difference is in percentage points relative to the reference scenario.
#' Cumulative default probabilities account for survival between months.
#'
#' Exposure and loss given default remain constant, with no amortization,
#' prepayment, currency conversion, discounting or additional defaults after
#' the first default.
#' Scenario probabilities, accounting stages and lifetime loan terms are not
#' modeled. Results are not CECL or IFRS 9 allowances, regulatory stress
#' results, or calibrated forecasts. The default portfolio and scenarios are
#' synthetic illustrations. The as-of date and vintage describe the scenario;
#' the caller must ensure that supplied portfolio inputs are appropriate for it.
#'
#' @param scenarios An `ek_scenario_library` object. Each scenario must have
#'   a reference path with the same version, as-of date, data vintage, dates
#'   and horizon.
#' @param portfolio Data frame with exactly one row per unique `segment` and
#'   numeric `ead`, `pd_12m`, `lgd` and `unemployment_beta` columns. `ead` is
#'   non-negative exposure at default in common units, with a positive total;
#'   `pd_12m` is the reference-scenario 12-month default probability in
#'   `[0, 1)`; `lgd` is loss given default in `[0, 1]`; and `unemployment_beta`
#'   is the finite log-hazard sensitivity per percentage-point change in
#'   unemployment. Defaults to a synthetic portfolio.
#' @param reference_scenario Scenario ID used as the reference for each
#'   version. Defaults to `"base"`.
#' @param component_fit Optional `ek_credit_lgd_ead_satellites` fit. When
#'   supplied, scenario LGD and EAD multipliers replace the static portfolio
#'   assumptions month by month in expected-loss calculations.
#' @return A list with scenario `summary`, segment-level `segments`, model
#'   version and input provenance. Component-adjusted runs also return
#'   `component_paths`. Amounts use the same units as `ead`.
#' @export
#' @examples
#' stress <- ek_credit_scenario_stress()
#' stress$summary
#' portfolio <- data.frame(segment = "CRE", ead = 1e6, pd_12m = 0.025,
#'                         lgd = 0.4, unemployment_beta = 0.35)
#' ek_credit_scenario_stress(portfolio = portfolio)$summary
ek_credit_scenario_stress <- function(scenarios = ek_scenario_library(),
                                      portfolio = NULL,
                                      reference_scenario = "base",
                                      component_fit = NULL) {
  ek_validate_scenario_library(scenarios) # nolint: object_usage_linter
  if (!"UNRATE" %in% names(scenarios$paths)) {
    ek_credit_input_error("Credit stress requires an `UNRATE` column in scenario paths.")
  }
  portfolio_source <- if (is.null(portfolio)) "synthetic_sample" else "provided"
  if (is.null(portfolio)) {
    portfolio <- data.frame(
      segment = c("commercial_real_estate", "commercial_industrial", "consumer"),
      ead = c(250e6, 200e6, 100e6),
      pd_12m = c(0.025, 0.02, 0.04),
      lgd = c(0.40, 0.45, 0.60),
      unemployment_beta = c(0.35, 0.25, 0.45),
      stringsAsFactors = FALSE
    )
  }
  ek_validate_credit_portfolio(portfolio)
  if (!is.null(component_fit) &&
        !inherits(component_fit, "ek_credit_lgd_ead_satellites")) {
    ek_credit_input_error(
      "`component_fit` must be an ek_credit_lgd_ead_satellites object or NULL."
    )
  }
  component_paths <- if (is.null(component_fit)) {
    NULL
  } else {
    ek_credit_lgd_ead_project(component_fit, scenarios) # nolint: object_usage_linter
  }
  if (!is.character(reference_scenario) || length(reference_scenario) != 1L ||
        is.na(reference_scenario) || !nzchar(trimws(reference_scenario))) {
    ek_credit_input_error("`reference_scenario` must be one non-empty string.")
  }

  total_ead <- sum(portfolio$ead)
  results <- lapply(ek_credit_scenario_pairs(scenarios, reference_scenario), function(pair) {
    path <- pair$path
    reference <- pair$reference

    unemployment_difference <- path$UNRATE - reference$UNRATE
    cumulative_pd <- vapply(seq_len(nrow(portfolio)), function(i) {
      adjustment <- portfolio$unemployment_beta[i] * unemployment_difference
      if (any(!is.finite(adjustment))) {
        ek_credit_input_error("Unemployment adjustment produces non-finite hazards.")
      }
      multiplier <- exp(adjustment)
      hazard <- -log1p(-portfolio$pd_12m[i]) / 12 * sum(multiplier)
      if (any(!is.finite(multiplier)) || !is.finite(hazard)) {
        ek_credit_input_error("Unemployment adjustment produces non-finite hazards.")
      }
      -expm1(-hazard)
    }, numeric(1))
    expected_loss <- portfolio$ead * portfolio$lgd * cumulative_pd
    component_detail <- NULL
    if (!is.null(component_paths)) {
      missing_segments <- setdiff(portfolio$segment, component_fit$segments)
      if (length(missing_segments)) {
        ek_credit_input_error(
          "Component fit is missing portfolio segment(s): ",
          paste(missing_segments, collapse = ", "), "."
        )
      }
      component_results <- lapply(seq_len(nrow(portfolio)), function(i) {
        segment <- portfolio$segment[i]
        component <- component_paths[
          component_paths$scenario_id == path$scenario_id[1] &
            component_paths$version == path$version[1] &
            component_paths$segment == segment,
          , drop = FALSE
        ]
        component <- component[order(component$step), , drop = FALSE]
        if (nrow(component) != nrow(path) ||
              !identical(component$step, path$step) ||
              !identical(component$date, path$date)) {
          ek_credit_input_error(
            "LGD/EAD projections must align with each credit scenario path."
          )
        }
        lgd_offset <- stats::qlogis(portfolio$lgd[i]) -
          component_fit$models[[segment]]$lgd$last_y
        projected_lgd <- stats::plogis(component$logit_lgd + lgd_offset)
        projected_ead <- portfolio$ead[i] * component$ead_multiplier
        adjustment <- portfolio$unemployment_beta[i] * unemployment_difference
        hazard <- -log1p(-portfolio$pd_12m[i]) / 12 * exp(adjustment)
        survival <- exp(-cumsum(hazard))
        marginal_pd <- -diff(c(1, survival))
        loss <- marginal_pd * projected_lgd * projected_ead
        if (any(!is.finite(loss)) || any(!is.finite(projected_lgd)) ||
              any(projected_lgd <= 0 | projected_lgd >= 1) ||
              any(!is.finite(projected_ead)) || any(projected_ead <= 0)) {
          ek_credit_input_error("Credit component stress produced invalid losses.")
        }
        list(
          segment = data.frame(
            segment = segment, ead = portfolio$ead[i], pd_12m = portfolio$pd_12m[i],
            lgd = portfolio$lgd[i], unemployment_beta = portfolio$unemployment_beta[i],
            cumulative_pd = 1 - tail(survival, 1),
            mean_lgd = mean(projected_lgd), mean_ead = mean(projected_ead),
            ending_ead_multiplier = tail(component$ead_multiplier, 1),
            expected_loss = sum(loss), stringsAsFactors = FALSE
          ),
          paths = data.frame(
            scenario_id = path$scenario_id[1], version = path$version[1],
            segment = segment, step = path$step, date = path$date,
            unemployment_gap = unemployment_difference, hazard = hazard,
            marginal_pd = marginal_pd, survival = survival,
            lgd = projected_lgd, ead = projected_ead,
            expected_loss = loss, stringsAsFactors = FALSE
          )
        )
      })
      component_segments <- do.call(rbind, lapply(component_results, `[[`, "segment"))
      component_detail <- do.call(rbind, lapply(component_results, `[[`, "paths"))
      expected_loss <- component_segments$expected_loss
      cumulative_pd <- component_segments$cumulative_pd
    }
    segment_results <- if (is.null(component_fit)) {
      data.frame(
        segment = portfolio$segment, ead = portfolio$ead, pd_12m = portfolio$pd_12m,
        lgd = portfolio$lgd, unemployment_beta = portfolio$unemployment_beta,
        cumulative_pd = cumulative_pd, expected_loss = expected_loss,
        stringsAsFactors = FALSE
      )
    } else {
      component_segments
    }
    list(
      summary = data.frame(
        scenario_id = path$scenario_id[1],
        version = path$version[1],
        as_of = path$as_of[1],
        data_vintage = path$data_vintage[1],
        horizon_months = nrow(path),
        portfolio_ead = total_ead,
        expected_loss = sum(expected_loss),
        loss_rate = sum(expected_loss) / total_ead,
        stringsAsFactors = FALSE
      ),
      segments = data.frame(
        scenario_id = path$scenario_id[1],
        version = path$version[1],
        segment_results, stringsAsFactors = FALSE
      ),
      component_paths = component_detail
    )
  })
  summary <- do.call(rbind, lapply(results, `[[`, "summary"))
  segments <- do.call(rbind, lapply(results, `[[`, "segments"))
  rownames(summary) <- NULL
  rownames(segments) <- NULL
  out <- list(
    model_version = "relative-unemployment-hazard-v1",
    reference_scenario = reference_scenario,
    scenario_synthetic = scenarios$synthetic,
    portfolio_source = portfolio_source,
    summary = summary,
    segments = segments
  )
  if (!is.null(component_fit)) {
    paths <- do.call(rbind, lapply(results, `[[`, "component_paths"))
    rownames(paths) <- NULL
    out$component_fit_version <- component_fit$model_version
    out$component_fit_synthetic <- component_fit$synthetic
    out$component_paths <- paths
  }
  out
}

ek_validate_credit_portfolio <- function(portfolio) {
  required <- c("segment", "ead", "pd_12m", "lgd", "unemployment_beta")
  if (!is.data.frame(portfolio) || !nrow(portfolio) ||
        anyDuplicated(names(portfolio)) || !setequal(names(portfolio), required)) {
    ek_credit_input_error(
      "`portfolio` must contain exactly the required segment and numeric columns."
    )
  }
  if (!is.character(portfolio$segment) || anyNA(portfolio$segment) ||
        any(!nzchar(trimws(portfolio$segment))) ||
        anyDuplicated(portfolio$segment)) {
    ek_credit_input_error("Portfolio segments must be unique, non-empty strings.")
  }
  numeric_columns <- required[-1]
  if (!all(vapply(portfolio[numeric_columns], is.numeric, logical(1))) ||
        any(!is.finite(as.matrix(portfolio[numeric_columns])))) {
    ek_credit_input_error("Portfolio credit inputs must be finite numeric values.")
  }
  if (any(portfolio$ead < 0) || !is.finite(sum(portfolio$ead)) ||
        sum(portfolio$ead) <= 0) {
    ek_credit_input_error("Portfolio exposures must be non-negative with a positive finite total.")
  }
  if (any(portfolio$pd_12m < 0 | portfolio$pd_12m >= 1) ||
        any(portfolio$lgd < 0 | portfolio$lgd > 1)) {
    ek_credit_input_error("Portfolio PDs must be in [0, 1) and LGDs in [0, 1].")
  }
  invisible(portfolio)
}

ek_credit_input_error <- function(...) {
  stop(errorCondition(paste0(..., collapse = ""),
                      class = "econkit_credit_input_error"))
}

# Splits scenario paths by scenario and version, each ordered by step and
# paired with its aligned reference path (NULL when `reference_scenario` is NULL).
ek_credit_scenario_pairs <- function(scenarios, reference_scenario) {
  paths <- scenarios$paths
  path_key <- paste(paths$scenario_id, paths$version, sep = "\r")
  lapply(unique(path_key), function(key) {
    path <- paths[path_key == key, , drop = FALSE]
    path <- path[order(path$step), , drop = FALSE]
    if (is.null(reference_scenario)) return(list(path = path, reference = NULL))
    reference <- paths[
      paths$scenario_id == reference_scenario &
        paths$version == path$version[1],
      , drop = FALSE
    ]
    if (!nrow(reference)) {
      ek_credit_input_error(
        paste0("Reference scenario is missing for version `", path$version[1], "`.")
      )
    }
    reference <- reference[order(reference$step), , drop = FALSE]
    if (path$as_of[1] != reference$as_of[1] ||
          path$data_vintage[1] != reference$data_vintage[1] ||
          !identical(path$date, reference$date)) {
      ek_credit_input_error(
        "Each scenario and reference path must have aligned dates, as-of and vintage."
      )
    }
    list(path = path, reference = reference)
  })
}
