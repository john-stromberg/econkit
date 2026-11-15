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
#' @return A list with scenario `summary`, segment-level `segments`, model
#'   version and input provenance. Amounts use the same units as `ead`.
#' @export
#' @examples
#' stress <- ek_credit_scenario_stress()
#' stress$summary
#' portfolio <- data.frame(segment = "CRE", ead = 1e6, pd_12m = 0.025,
#'                         lgd = 0.4, unemployment_beta = 0.35)
#' ek_credit_scenario_stress(portfolio = portfolio)$summary
ek_credit_scenario_stress <- function(scenarios = ek_scenario_library(),
                                      portfolio = NULL,
                                      reference_scenario = "base") {
  ek_validate_scenario_library(scenarios) # nolint: object_usage_linter
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
  if (!is.character(reference_scenario) || length(reference_scenario) != 1L ||
        is.na(reference_scenario) || !nzchar(trimws(reference_scenario))) {
    ek_credit_input_error("`reference_scenario` must be one non-empty string.")
  }

  paths <- scenarios$paths
  path_key <- paste(paths$scenario_id, paths$version, sep = "\r")
  total_ead <- sum(portfolio$ead)
  results <- lapply(unique(path_key), function(key) {
    path <- paths[path_key == key, , drop = FALSE]
    path <- path[order(path$step), , drop = FALSE]
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
        segment = portfolio$segment,
        ead = portfolio$ead,
        pd_12m = portfolio$pd_12m,
        lgd = portfolio$lgd,
        unemployment_beta = portfolio$unemployment_beta,
        cumulative_pd = cumulative_pd,
        expected_loss = expected_loss,
        stringsAsFactors = FALSE
      )
    )
  })
  summary <- do.call(rbind, lapply(results, `[[`, "summary"))
  segments <- do.call(rbind, lapply(results, `[[`, "segments"))
  rownames(summary) <- NULL
  rownames(segments) <- NULL
  list(
    model_version = "relative-unemployment-hazard-v1",
    reference_scenario = reference_scenario,
    scenario_synthetic = scenarios$synthetic,
    portfolio_source = portfolio_source,
    summary = summary,
    segments = segments
  )
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

ek_credit_input_error <- function(message) {
  stop(errorCondition(message, class = "econkit_credit_input_error"))
}
