#' Estimate lifetime expected credit losses under macro scenarios
#'
#' Projects monthly default hazards, exposures and discounted losses for
#' homogeneous portfolio segments over their remaining term, in the style of
#' CECL and IFRS 9 expected credit loss (ECL) estimates.
#'
#' Each segment's monthly hazard is its baseline `pd_12m` converted to a
#' constant monthly hazard and multiplied by
#' `exp(unemployment_beta * (unemployment - anchor))`. Over the scenario
#' horizon (the reasonable and supportable period) the unemployment path
#' comes from the scenario; the log adjustment then reverts linearly to zero
#' over `reversion_months` and stays at the baseline hazard for the rest of
#' the term. Exposure is constant (`"bullet"`) or amortizes linearly to zero
#' at maturity (`"linear"`). Monthly losses are marginal default probability
#' times LGD times exposure, discounted monthly at `rate` when `discount` is
#' `TRUE`.
#'
#' Under `"cecl"` every segment uses its remaining term. Under `"ifrs9"`,
#' stage 1 segments use a 12-month horizon and stage 2 segments their
#' remaining term. Stage 3 (credit-impaired) segments have a loss of
#' `ead * lgd` under both standards.
#'
#' @section Model card:
#' **Purpose:** illustrate scenario-conditional and probability-weighted
#' lifetime ECL for allowance analysis, budgeting and stress testing.
#' **Assumptions:** proportional hazards driven by unemployment, constant LGD,
#' no prepayment, draws, cures or re-defaults, and level or linearly
#' amortizing balances. **Limitations:** results depend on the anchor,
#' reversion period, scenario weights and segment inputs, which require
#' institution-specific calibration and governance (see
#' [ek_credit_pd_sensitivity()]). Stage assignment, significant increase in
#' credit risk tests, qualitative overlays, purchased credit-deteriorated
#' assets and off-balance-sheet exposures are not modeled. Results are not
#' accounting allowances, regulatory results or audited estimates.
#'
#' @param scenarios An `ek_scenario_library` with an `UNRATE` column.
#' @param portfolio Data frame with one row per unique `segment` and numeric
#'   `ead`, `pd_12m`, `lgd`, `unemployment_beta` (as in
#'   [ek_credit_scenario_stress()]), `term_months` (remaining term, a whole
#'   number from 1 to 600) and `rate` (annual effective interest rate in
#'   `[0, 1)`) columns, plus optional `amortization` (`"bullet"` or
#'   `"linear"`, default `"bullet"`) and `stage` (1, 2 or 3, default 1).
#'   Defaults to a synthetic portfolio.
#' @param weights Optional named scenario probabilities (by `scenario_id`),
#'   non-negative and summing to 1, covering every scenario in each version.
#'   When supplied, probability-weighted results are returned.
#' @param standard `"cecl"` or `"ifrs9"`.
#' @param reference_scenario Scenario used as the anchor path when `anchor` is
#'   `NULL`, so `pd_12m` applies along the reference path. Defaults to
#'   `"base"`.
#' @param anchor Optional unemployment rate (percent) at which `pd_12m`
#'   applies, such as a long-run average. Use it when `pd_12m` is a
#'   through-the-cycle PD, so that every scenario, including the reference,
#'   is adjusted for current conditions.
#' @param reversion_months Whole number of months over which the unemployment
#'   adjustment reverts to zero after the scenario horizon.
#' @param discount Discount losses at `rate`. Defaults to `TRUE` for
#'   `"ifrs9"` and `FALSE` for `"cecl"`.
#' @return A list with settings, `summary` and `segments` by scenario,
#'   monthly `paths`, and `weighted` and `weighted_segments` (`NULL` without
#'   `weights`). Amounts use the units of `ead`.
#' @export
#' @examples
#' ecl <- ek_credit_lifetime_ecl(weights = c(base = 0.6, adverse = 0.3, severe = 0.1))
#' ecl$summary
#' ecl$weighted
#' ek_credit_lifetime_ecl(standard = "ifrs9")$segments
ek_credit_lifetime_ecl <- function(scenarios = ek_scenario_library(), portfolio = NULL,
                                   weights = NULL, standard = c("cecl", "ifrs9"),
                                   reference_scenario = "base", anchor = NULL,
                                   reversion_months = 12L, discount = NULL) {
  standard <- match.arg(standard)
  ek_validate_scenario_library(scenarios) # nolint: object_usage_linter
  if (!"UNRATE" %in% names(scenarios$paths)) {
    ek_credit_input_error("Lifetime ECL requires an `UNRATE` column in scenario paths.")
  }
  portfolio_source <- if (is.null(portfolio)) "synthetic_sample" else "provided"
  if (is.null(portfolio)) portfolio <- ek_sample_ecl_portfolio()
  portfolio <- ek_validate_ecl_portfolio(portfolio)
  if (!is.null(anchor) && (!is.numeric(anchor) || length(anchor) != 1L ||
                             !is.finite(anchor))) {
    ek_credit_input_error("`anchor` must be NULL or one finite unemployment rate.")
  }
  if (is.null(anchor) && (!is.character(reference_scenario) ||
                            length(reference_scenario) != 1L ||
                            is.na(reference_scenario) || !nzchar(reference_scenario))) {
    ek_credit_input_error("`reference_scenario` must be one non-empty string.")
  }
  if (!is.numeric(reversion_months) || length(reversion_months) != 1L ||
        !is.finite(reversion_months) || reversion_months < 0 ||
        reversion_months != round(reversion_months)) {
    ek_credit_input_error("`reversion_months` must be a non-negative whole number.")
  }
  if (is.null(discount)) discount <- standard == "ifrs9"
  if (!is.logical(discount) || length(discount) != 1L || is.na(discount)) {
    ek_credit_input_error("`discount` must be TRUE or FALSE.")
  }

  pairs <- ek_credit_scenario_pairs(scenarios, if (is.null(anchor)) reference_scenario)
  weights <- ek_check_ecl_weights(weights, scenarios$paths)
  horizon <- ifelse(portfolio$stage == 1L & standard == "ifrs9",
                    pmin(12L, portfolio$term_months), portfolio$term_months)
  total_ead <- sum(portfolio$ead)

  results <- lapply(pairs, function(pair) {
    path <- pair$path
    gap <- path$UNRATE - if (is.null(anchor)) pair$reference$UNRATE else anchor
    segment_paths <- lapply(seq_len(nrow(portfolio)), function(i) {
      ek_ecl_segment_path(portfolio[i, , drop = FALSE], horizon[i], gap, reversion_months,
                          discount, path$as_of[1])
    })
    impaired <- portfolio$stage == 3L
    path_loss <- vapply(segment_paths, function(p) sum(p$expected_loss), numeric(1))
    path_pd <- vapply(seq_along(segment_paths), function(i) {
      1 - segment_paths[[i]]$survival[horizon[i]]
    }, numeric(1))
    ecl <- ifelse(impaired, portfolio$ead * portfolio$lgd, path_loss)
    horizon_pd <- ifelse(impaired, 1, path_pd)
    paths <- do.call(rbind, segment_paths[!impaired])
    if (!is.null(paths)) {
      paths <- cbind(scenario_id = path$scenario_id[1], version = path$version[1], paths,
                     stringsAsFactors = FALSE)
    }
    list(
      summary = data.frame(
        scenario_id = path$scenario_id[1], version = path$version[1], as_of = path$as_of[1],
        data_vintage = path$data_vintage[1], scenario_months = nrow(path),
        portfolio_ead = total_ead, ecl = sum(ecl), ecl_rate = sum(ecl) / total_ead,
        stringsAsFactors = FALSE
      ),
      segments = data.frame(
        scenario_id = path$scenario_id[1], version = path$version[1],
        segment = portfolio$segment, stage = portfolio$stage, ead = portfolio$ead,
        horizon_months = as.integer(ifelse(impaired, 0L, horizon)),
        horizon_pd = horizon_pd, ecl = ecl, ecl_rate = ecl / portfolio$ead,
        stringsAsFactors = FALSE
      ),
      paths = paths
    )
  })
  bind <- function(name) {
    out <- do.call(rbind, lapply(results, `[[`, name))
    if (!is.null(out)) rownames(out) <- NULL
    out
  }
  summary <- bind("summary")
  segments <- bind("segments")
  weighted <- NULL
  weighted_segments <- NULL
  if (!is.null(weights)) {
    segments$weight <- unname(weights[segments$scenario_id])
    weighted_segments <- stats::aggregate(
      cbind(ecl = segments$ecl * segments$weight),
      by = list(version = segments$version, segment = segments$segment), FUN = sum
    )
    weighted_segments$ead <- portfolio$ead[match(weighted_segments$segment, portfolio$segment)]
    weighted_segments$ecl_rate <- weighted_segments$ecl / weighted_segments$ead
    weighted_segments <- weighted_segments[c("version", "segment", "ead", "ecl", "ecl_rate")]
    weighted <- stats::aggregate(cbind(ecl = weighted_segments$ecl),
                                 by = list(version = weighted_segments$version), FUN = sum)
    weighted$portfolio_ead <- total_ead
    weighted$ecl_rate <- weighted$ecl / total_ead
    weighted <- weighted[c("version", "portfolio_ead", "ecl", "ecl_rate")]
    segments$weight <- NULL
  }
  list(
    model_version = "lifetime-hazard-ecl-v1",
    standard = standard,
    discount = discount,
    reference_scenario = if (is.null(anchor)) reference_scenario else NA_character_,
    anchor = if (is.null(anchor)) NA_real_ else anchor,
    reversion_months = as.integer(reversion_months),
    scenario_synthetic = scenarios$synthetic,
    portfolio_source = portfolio_source,
    weights = weights,
    summary = summary,
    segments = segments,
    weighted = weighted,
    weighted_segments = weighted_segments,
    paths = bind("paths")
  )
}

ek_ecl_segment_path <- function(segment, months, gap, reversion_months, discount, as_of) {
  scenario_months <- length(gap)
  shift <- numeric(months)
  covered <- seq_len(min(scenario_months, months))
  shift[covered] <- gap[covered]
  if (months > scenario_months && reversion_months > 0) {
    k <- seq_len(min(reversion_months, months - scenario_months))
    shift[scenario_months + k] <- gap[scenario_months] * (1 - k / reversion_months)
  }
  log_multiplier <- segment$unemployment_beta * shift
  hazard <- -log1p(-segment$pd_12m) / 12 * exp(log_multiplier)
  if (any(!is.finite(hazard))) {
    ek_credit_input_error("Unemployment adjustment produces non-finite hazards.")
  }
  survival <- exp(-cumsum(hazard))
  marginal_pd <- -diff(c(1, survival))
  month <- seq_len(months)
  exposure <- if (segment$amortization == "linear") {
    segment$ead * (segment$term_months - month + 1) / segment$term_months
  } else {
    rep(segment$ead, months)
  }
  discount_factor <- if (discount) (1 + segment$rate)^(-month / 12) else rep(1, months)
  data.frame(
    segment = segment$segment, month = month, date = ek_add_months(rep(as_of, months), month),
    unemployment_gap = shift, hazard_multiplier = exp(log_multiplier),
    marginal_pd = marginal_pd, survival = survival, exposure = exposure,
    discount_factor = discount_factor,
    expected_loss = marginal_pd * segment$lgd * exposure * discount_factor,
    stringsAsFactors = FALSE
  )
}

ek_sample_ecl_portfolio <- function() {
  data.frame(
    segment = c("commercial_real_estate", "commercial_industrial", "consumer"),
    ead = c(250e6, 200e6, 100e6),
    pd_12m = c(0.025, 0.02, 0.04),
    lgd = c(0.40, 0.45, 0.60),
    unemployment_beta = c(0.35, 0.25, 0.45),
    term_months = c(60, 36, 48),
    rate = c(0.065, 0.07, 0.11),
    amortization = c("bullet", "linear", "linear"),
    stage = c(1, 1, 2),
    stringsAsFactors = FALSE
  )
}

ek_validate_ecl_portfolio <- function(portfolio) {
  base <- c("segment", "ead", "pd_12m", "lgd", "unemployment_beta")
  required <- c(base, "term_months", "rate")
  allowed <- c(required, "amortization", "stage")
  if (!is.data.frame(portfolio) || !all(required %in% names(portfolio)) ||
        !all(names(portfolio) %in% allowed) || anyDuplicated(names(portfolio))) {
    ek_credit_input_error(paste0(
      "`portfolio` must contain ", paste(required, collapse = ", "),
      " and optionally amortization and stage."
    ))
  }
  ek_validate_credit_portfolio(portfolio[base])
  term <- portfolio$term_months
  if (!is.numeric(term) || any(!is.finite(term)) || any(term < 1 | term > 600) ||
        any(term != round(term))) {
    ek_credit_input_error("`term_months` must be whole numbers from 1 to 600.")
  }
  if (!is.numeric(portfolio$rate) || any(!is.finite(portfolio$rate)) ||
        any(portfolio$rate < 0 | portfolio$rate >= 1)) {
    ek_credit_input_error("`rate` must be annual rates in [0, 1).")
  }
  if (is.null(portfolio$amortization)) portfolio$amortization <- "bullet"
  if (!is.character(portfolio$amortization) ||
        !all(portfolio$amortization %in% c("bullet", "linear"))) {
    ek_credit_input_error("`amortization` must be \"bullet\" or \"linear\".")
  }
  if (is.null(portfolio$stage)) portfolio$stage <- 1L
  if (!is.numeric(portfolio$stage) || !all(portfolio$stage %in% 1:3)) {
    ek_credit_input_error("`stage` must be 1, 2 or 3.")
  }
  portfolio$term_months <- as.integer(term)
  portfolio$stage <- as.integer(portfolio$stage)
  portfolio
}

ek_check_ecl_weights <- function(weights, paths) {
  if (is.null(weights)) return(NULL)
  if (!is.numeric(weights) || is.null(names(weights)) || anyDuplicated(names(weights)) ||
        any(!is.finite(weights)) || any(weights < 0)) {
    ek_credit_input_error("`weights` must be named, non-negative scenario probabilities.")
  }
  for (version in unique(paths$version)) {
    ids <- unique(paths$scenario_id[paths$version == version])
    if (!setequal(ids, names(weights))) {
      ek_credit_input_error(paste0(
        "`weights` must name exactly the scenarios in version `", version, "`: ",
        paste(ids, collapse = ", "), "."
      ))
    }
  }
  if (abs(sum(weights) - 1) > 1e-8) ek_credit_input_error("`weights` must sum to 1.")
  weights
}
