#' Allocate matched-tenor funding costs under rate scenarios
#'
#' Calculates illustrative funds-transfer-pricing (FTP) rates by adding
#' position-specific funding spreads to the matched-tenor Treasury par yield.
#' Asset contributions are customer rates less transfer rates; liability
#' contributions are transfer rates less customer rates.
#'
#' @section Model card:
#' **Purpose:** demonstrate matched-tenor FTP and scenario sensitivity.
#' **Assumptions:** balances and customer rates remain static, and the supplied
#' funding spread captures institution-specific liquidity and funding costs.
#' **Limitations:** Treasury par yields are not an internal funding curve;
#' this does not model optionality, contingent liquidity, behavioral balances,
#' hedges or regulatory transfer-pricing policy. Sample balances and spreads
#' are synthetic and require institutional calibration.
#'
#' @param curves An `ek_yield_curve_projection` from
#'   [ek_yield_curve_project()].
#' @param portfolio Data frame with unique `position`, `side` (`"asset"` or
#'   `"liability"`), non-negative `balance`, customer `rate` in decimal units,
#'   curve `tenor` name and non-negative `funding_spread_bps`. Defaults to
#'   synthetic sample positions.
#' @return A list with scenario summaries, monthly aggregate paths, position
#'   contributions and synthetic-data metadata. Amounts use the balance units.
#' @export
#' @examples
#' curves <- ek_yield_curve_project(ek_yield_curve_satellite_fit(),
#'                                  ek_scenario_library())
#' ek_ftp_analysis(curves)$summary
ek_ftp_analysis <- function(curves, portfolio = NULL) {
  ek_validate_rate_projection(curves)
  portfolio_source <- if (is.null(portfolio)) "synthetic_sample" else "provided"
  if (is.null(portfolio)) portfolio <- ek_sample_ftp_portfolio()
  portfolio <- ek_validate_ftp_portfolio(portfolio, names(curves$tenors))
  groups <- ek_rate_projection_groups(curves)

  positions <- do.call(rbind, lapply(groups, function(path) {
    do.call(rbind, lapply(seq_len(nrow(portfolio)), function(i) {
      position <- portfolio[i, , drop = FALSE]
      benchmark <- path[[position$tenor]]
      transfer <- benchmark / 100 + position$funding_spread_bps / 10000
      side <- if (position$side == "asset") 1 else -1
      margin <- side * (position$rate - transfer)
      data.frame(
        scenario_id = path$scenario_id, version = path$version,
        step = path$step, date = path$date, position = position$position,
        side = position$side, tenor = position$tenor, balance = position$balance,
        benchmark_rate = benchmark / 100, funding_spread_bps = position$funding_spread_bps,
        transfer_rate = transfer, customer_rate = position$rate,
        margin_bps = margin * 10000,
        monthly_contribution = position$balance * margin / 12,
        stringsAsFactors = FALSE
      )
    }))
  }))
  rownames(positions) <- NULL
  if (any(!is.finite(positions$transfer_rate)) ||
        any(!is.finite(positions$monthly_contribution))) {
    stop("FTP calculations produced non-finite transfer rates or contributions.",
         call. = FALSE)
  }
  paths <- stats::aggregate(
    cbind(monthly_contribution = positions$monthly_contribution,
          transfer_balance = positions$transfer_rate * positions$balance,
          balance = positions$balance),
    by = list(scenario_id = positions$scenario_id, version = positions$version,
              step = positions$step, date = positions$date),
    FUN = sum
  )
  paths$weighted_transfer_rate <- paths$transfer_balance / paths$balance
  paths$annualized_contribution <- paths$monthly_contribution * 12
  paths$transfer_balance <- NULL
  summary <- stats::aggregate(
    cbind(annualized_contribution = paths$annualized_contribution,
          cumulative_contribution = paths$monthly_contribution,
          weighted_transfer_rate = paths$weighted_transfer_rate),
    by = list(scenario_id = paths$scenario_id, version = paths$version), FUN = mean
  )
  cumulative <- stats::aggregate(
    cbind(cumulative_contribution = paths$monthly_contribution),
    by = list(scenario_id = paths$scenario_id, version = paths$version), FUN = sum
  )
  summary$cumulative_contribution <- cumulative$cumulative_contribution[
    match(paste(summary$scenario_id, summary$version),
          paste(cumulative$scenario_id, cumulative$version))
  ]
  list(
    model_version = "matched-tenor-ftp-v1",
    scenario_synthetic = curves$synthetic,
    portfolio_source = portfolio_source,
    summary = summary,
    paths = paths,
    positions = positions
  )
}

#' Build DCF and DCM benchmark-rate inputs from scenario curves
#'
#' Combines scenario-implied Treasury par yields with explicit, user-supplied
#' equity risk premia, betas and debt or issuance spreads. Outputs illustrative
#' cost-of-equity, cost-of-debt, WACC and DCM indicative-yield inputs; it does
#' not calculate enterprise values or bond prices.
#'
#' @section Model card:
#' **Purpose:** provide transparent benchmark and spread inputs for valuation
#' and issuance analysis. **Assumptions:** par yields proxy for the risk-free
#' benchmark at the selected tenor; supplied risk premia, betas and spreads are
#' appropriate for the instrument and date. **Limitations:** no zero-coupon
#' curve, issuer credit model, cash-flow valuation, tax-shield constraints,
#' capital structure changes, coupon schedule, convexity or day-count rules.
#' Bundled assumptions are synthetic and illustrative, not market quotes or
#' investment-banking advice.
#'
#' @param curves An `ek_yield_curve_projection` from
#'   [ek_yield_curve_project()].
#' @param dcf Data frame with `project`, `tenor`, `beta`,
#'   `equity_risk_premium_bps`, `debt_spread_bps`, `equity_weight`,
#'   `debt_weight` and `tax_rate`. Defaults to synthetic sample assumptions.
#' @param dcm Data frame with `issue`, `tenor` and `spread_bps`. Defaults to
#'   synthetic sample assumptions.
#' @return A list with scenario-keyed `dcf` and `dcm` input tables and
#'   synthetic-data metadata.
#' @export
#' @examples
#' curves <- ek_yield_curve_project(ek_yield_curve_satellite_fit(),
#'                                  ek_scenario_library())
#' rates <- ek_curve_pricing_inputs(curves)
#' rates$dcf
#' rates$dcm
ek_curve_pricing_inputs <- function(curves, dcf = NULL, dcm = NULL) {
  ek_validate_rate_projection(curves)
  dcf_source <- if (is.null(dcf)) "synthetic_sample" else "provided"
  dcm_source <- if (is.null(dcm)) "synthetic_sample" else "provided"
  if (is.null(dcf)) dcf <- ek_sample_dcf_assumptions()
  if (is.null(dcm)) dcm <- ek_sample_dcm_assumptions()
  dcf <- ek_validate_dcf_assumptions(dcf, names(curves$tenors))
  dcm <- ek_validate_dcm_assumptions(dcm, names(curves$tenors))
  groups <- ek_rate_projection_groups(curves)

  dcf_paths <- do.call(rbind, lapply(groups, function(path) {
    do.call(rbind, lapply(seq_len(nrow(dcf)), function(i) {
      assumption <- dcf[i, , drop = FALSE]
      risk_free <- path[[assumption$tenor]] / 100
      cost_equity <- risk_free +
        assumption$beta * assumption$equity_risk_premium_bps / 10000
      cost_debt <- risk_free + assumption$debt_spread_bps / 10000
      wacc <- assumption$equity_weight * cost_equity +
        assumption$debt_weight * cost_debt * (1 - assumption$tax_rate)
      data.frame(
        scenario_id = path$scenario_id, version = path$version,
        step = path$step, date = path$date, project = assumption$project,
        tenor = assumption$tenor, risk_free_rate = risk_free,
        beta = assumption$beta,
        equity_risk_premium_bps = assumption$equity_risk_premium_bps,
        cost_of_equity = cost_equity, debt_spread_bps = assumption$debt_spread_bps,
        cost_of_debt = cost_debt, equity_weight = assumption$equity_weight,
        debt_weight = assumption$debt_weight, tax_rate = assumption$tax_rate,
        wacc = wacc, stringsAsFactors = FALSE
      )
    }))
  }))
  dcm_paths <- do.call(rbind, lapply(groups, function(path) {
    do.call(rbind, lapply(seq_len(nrow(dcm)), function(i) {
      issue <- dcm[i, , drop = FALSE]
      benchmark <- path[[issue$tenor]] / 100
      data.frame(
        scenario_id = path$scenario_id, version = path$version,
        step = path$step, date = path$date, issue = issue$issue,
        tenor = issue$tenor, benchmark_rate = benchmark,
        spread_bps = issue$spread_bps,
        indicative_yield = benchmark + issue$spread_bps / 10000,
        stringsAsFactors = FALSE
      )
    }))
  }))
  rownames(dcf_paths) <- NULL
  rownames(dcm_paths) <- NULL
  if (any(!is.finite(dcf_paths$wacc)) ||
        any(!is.finite(dcm_paths$indicative_yield))) {
    stop("Pricing-input calculations produced non-finite rates.", call. = FALSE)
  }
  list(
    model_version = "curve-pricing-inputs-v1",
    scenario_synthetic = curves$synthetic,
    dcf_source = dcf_source,
    dcm_source = dcm_source,
    dcf = dcf_paths,
    dcm = dcm_paths
  )
}

ek_validate_rate_projection <- function(curves) {
  if (!inherits(curves, "ek_yield_curve_projection") ||
        !is.data.frame(curves$yields) || is.null(curves$tenors) ||
        !is.numeric(curves$tenors) || is.null(names(curves$tenors)) ||
        anyNA(names(curves$tenors)) || any(!nzchar(names(curves$tenors))) ||
        anyDuplicated(names(curves$tenors)) || any(!is.finite(curves$tenors)) ||
        any(curves$tenors <= 0) || !is.logical(curves$synthetic) ||
        length(curves$synthetic) != 1L || is.na(curves$synthetic)) {
    stop("`curves` must be a valid ek_yield_curve_projection.", call. = FALSE)
  }
  required <- c("scenario_id", "version", "step", "date", names(curves$tenors))
  paths <- curves$yields
  if (!all(required %in% names(paths)) || !nrow(paths) ||
        !inherits(paths$date, "Date") ||
        !is.character(paths$scenario_id) || !is.character(paths$version) ||
        anyNA(paths$scenario_id) || anyNA(paths$version) ||
        any(!nzchar(paths$scenario_id)) || any(!nzchar(paths$version)) ||
        !is.numeric(paths$step) || any(!is.finite(paths$step)) ||
        any(paths$step < 1 | paths$step != round(paths$step)) ||
        anyNA(paths$date) ||
        !all(vapply(paths[names(curves$tenors)], is.numeric, logical(1))) ||
        any(!is.finite(as.matrix(paths[names(curves$tenors)])))) {
    stop("Projected yields have invalid scenario keys, steps, dates or tenors.",
         call. = FALSE)
  }
  invisible(curves)
}

ek_rate_projection_groups <- function(curves) {
  paths <- curves$yields
  path_key <- paste(paths$scenario_id, paths$version, sep = "\r")
  groups <- split(paths, factor(path_key, levels = unique(path_key)))
  groups <- lapply(groups, function(path) path[order(path$step), , drop = FALSE])
  if (any(vapply(groups, function(path) {
    anyDuplicated(path$step) > 0L ||
      !identical(path$step, seq_len(nrow(path))) ||
      anyDuplicated(path$date) > 0L
  }, logical(1)))) {
    stop("Projected curves must have unique dates and consecutive steps.", call. = FALSE)
  }
  groups
}

ek_sample_ftp_portfolio <- function() {
  data.frame(
    position = c("commercial_loans", "fixed_mortgages", "floating_loans",
                 "core_deposits", "wholesale_funding"),
    side = c("asset", "asset", "asset", "liability", "liability"),
    balance = c(300e6, 180e6, 120e6, 420e6, 100e6),
    rate = c(0.072, 0.055, 0.068, 0.018, 0.045),
    tenor = c("DGS5", "DGS10", "DGS2", "DGS3MO", "DGS2"),
    funding_spread_bps = c(100, 100, 100, 75, 100),
    stringsAsFactors = FALSE
  )
}

ek_validate_ftp_portfolio <- function(portfolio, tenors) {
  required <- c("position", "side", "balance", "rate", "tenor", "funding_spread_bps")
  if (!is.data.frame(portfolio) || !nrow(portfolio) ||
        anyDuplicated(names(portfolio)) || !setequal(names(portfolio), required)) {
    stop("`portfolio` must contain exactly the required FTP position columns.",
         call. = FALSE)
  }
  text <- c("position", "side", "tenor")
  if (!all(vapply(portfolio[text], is.character, logical(1))) ||
        anyNA(as.matrix(portfolio[text])) ||
        any(!nzchar(trimws(as.matrix(portfolio[text]))))) {
    stop("FTP position, side and tenor fields must be non-empty strings.",
         call. = FALSE)
  }
  numeric <- setdiff(required, text)
  if (!all(vapply(portfolio[numeric], is.numeric, logical(1))) ||
        any(!is.finite(as.matrix(portfolio[numeric])))) {
    stop("FTP balance, rate and funding spreads must be finite numbers.", call. = FALSE)
  }
  if (anyDuplicated(portfolio$position) || any(!portfolio$side %in% c("asset", "liability")) ||
        any(portfolio$balance < 0) || !is.finite(sum(portfolio$balance)) ||
        sum(portfolio$balance) <= 0 || any(portfolio$rate < 0 | portfolio$rate >= 1) ||
        any(!portfolio$tenor %in% tenors) ||
        any(portfolio$funding_spread_bps < 0 | portfolio$funding_spread_bps > 5000)) {
    stop("FTP positions contain invalid sides, balances, rates or funding spreads.",
         call. = FALSE)
  }
  invisible(portfolio)
}

ek_sample_dcf_assumptions <- function() {
  data.frame(
    project = c("sample_equity_valuation", "sample_enterprise_valuation"),
    tenor = c("DGS10", "DGS5"),
    beta = c(1, 0.9),
    equity_risk_premium_bps = c(500, 500),
    debt_spread_bps = c(200, 175),
    equity_weight = c(0.8, 0.7),
    debt_weight = c(0.2, 0.3),
    tax_rate = c(0.25, 0.25),
    stringsAsFactors = FALSE
  )
}

ek_sample_dcm_assumptions <- function() {
  data.frame(
    issue = c("sample_investment_grade", "sample_high_yield"),
    tenor = c("DGS5", "DGS5"),
    spread_bps = c(125, 425),
    stringsAsFactors = FALSE
  )
}

ek_validate_dcf_assumptions <- function(dcf, tenors) {
  required <- c("project", "tenor", "beta", "equity_risk_premium_bps",
                "debt_spread_bps", "equity_weight", "debt_weight", "tax_rate")
  if (!is.data.frame(dcf) || !nrow(dcf) || anyDuplicated(names(dcf)) ||
        !setequal(names(dcf), required)) {
    stop("`dcf` must contain exactly the required DCF assumption columns.",
         call. = FALSE)
  }
  if (!is.character(dcf$project) || !is.character(dcf$tenor) ||
        anyNA(dcf$project) || anyNA(dcf$tenor) ||
        any(!nzchar(trimws(dcf$project))) || anyDuplicated(dcf$project) ||
        any(!dcf$tenor %in% tenors)) {
    stop("DCF projects must have unique names and valid curve tenors.", call. = FALSE)
  }
  numeric <- setdiff(required, c("project", "tenor"))
  if (!all(vapply(dcf[numeric], is.numeric, logical(1))) ||
        any(!is.finite(as.matrix(dcf[numeric])))) {
    stop("DCF assumptions must be finite numeric values.", call. = FALSE)
  }
  if (any(dcf$beta < 0 | dcf$beta > 5) ||
        any(dcf$equity_risk_premium_bps < 0 | dcf$equity_risk_premium_bps > 5000) ||
        any(dcf$debt_spread_bps < 0 | dcf$debt_spread_bps > 5000) ||
        any(dcf$equity_weight < 0 | dcf$equity_weight > 1) ||
        any(dcf$debt_weight < 0 | dcf$debt_weight > 1) ||
        any(abs(dcf$equity_weight + dcf$debt_weight - 1) > 1e-8) ||
        any(dcf$tax_rate < 0 | dcf$tax_rate > 1)) {
    stop("DCF assumptions contain invalid beta, spreads, weights or tax rates.",
         call. = FALSE)
  }
  invisible(dcf)
}

ek_validate_dcm_assumptions <- function(dcm, tenors) {
  required <- c("issue", "tenor", "spread_bps")
  if (!is.data.frame(dcm) || !nrow(dcm) || anyDuplicated(names(dcm)) ||
        !setequal(names(dcm), required)) {
    stop("`dcm` must contain exactly the required DCM assumption columns.",
         call. = FALSE)
  }
  if (!is.character(dcm$issue) || !is.character(dcm$tenor) ||
        anyNA(dcm$issue) || anyNA(dcm$tenor) ||
        any(!nzchar(trimws(dcm$issue))) || anyDuplicated(dcm$issue) ||
        any(!dcm$tenor %in% tenors)) {
    stop("DCM issues must have unique names and valid curve tenors.", call. = FALSE)
  }
  if (!is.numeric(dcm$spread_bps) || any(!is.finite(dcm$spread_bps)) ||
        any(dcm$spread_bps < 0 | dcm$spread_bps > 5000)) {
    stop("DCM spreads must be finite numbers between 0 and 5000 basis points.",
         call. = FALSE)
  }
  invisible(dcm)
}
