#' Stress banking NII and economic value under rate scenarios
#'
#' Applies projected Treasury-curve changes to a static balance sheet. Floating
#' positions reprice at their reset frequency; deposit funding reprices monthly
#' at its beta to the three-month yield. Economic-value changes use a modified
#' duration approximation. Scenario changes are relative to the selected
#' reference curve, not absolute earnings forecasts or fair values.
#'
#' @section Model card:
#' **Purpose:** illustrate asset-liability rate sensitivity for ALCO discussion
#' and scenario design. **Assumptions:** balances remain static, floating-rate
#' positions pass through tenor yield changes one-for-one, deposit betas are
#' constant, and duration provides a linear value approximation.
#' **Limitations:** no behavioral balance decay, deposit floors/caps, convexity,
#' basis risk, prepayments, new business, hedges or full cash-flow valuation.
#' The underlying projected par yields are not discount curves. Portfolio
#' assumptions require institution-specific calibration; results are not
#' regulatory NII/EVE measures or valuation inputs.
#'
#' @param curves An `ek_yield_curve_projection` from
#'   [ek_yield_curve_project()].
#' @param portfolio Data frame with unique `position`, `side` (`"asset"` or
#'   `"liability"`), non-negative `balance`, annual `rate` in decimal units,
#'   curve `tenor` name, `repricing` (`"fixed"`, `"floating"` or `"deposit"`),
#'   modified `duration` in years, whole-number `reset_months`, and
#'   `deposit_beta` in `[0, 1]`. Defaults to a synthetic balance sheet.
#' @param reference_scenario Scenario ID whose curve path anchors rate changes.
#' @param horizon_months Positive whole number of months not exceeding every
#'   projected path.
#' @return A list with scenario summaries, aggregate monthly paths, position
#'   contributions, settings and synthetic-data metadata. NII and EVE changes
#'   are in the same currency units as `balance`.
#' @export
#' @examples
#' fit <- ek_yield_curve_satellite_fit()
#' curves <- ek_yield_curve_project(fit, ek_scenario_library())
#' alm <- ek_alm_scenario_analysis(curves)
#' alm$summary
ek_alm_scenario_analysis <- function(curves, portfolio = NULL,
                                     reference_scenario = "base",
                                     horizon_months = 12L) {
  if (!inherits(curves, "ek_yield_curve_projection") ||
        !is.data.frame(curves$yields) || is.null(curves$tenors) ||
        !is.numeric(curves$tenors) || is.null(names(curves$tenors)) ||
        anyNA(names(curves$tenors)) || any(!nzchar(names(curves$tenors))) ||
        anyDuplicated(names(curves$tenors)) || any(!is.finite(curves$tenors)) ||
        any(curves$tenors <= 0) || !"DGS3MO" %in% names(curves$tenors)) {
    stop("`curves` must be a yield-curve projection with a DGS3MO tenor.",
         call. = FALSE)
  }
  if (!is.character(reference_scenario) || length(reference_scenario) != 1L ||
        is.na(reference_scenario) || !nzchar(trimws(reference_scenario))) {
    stop("`reference_scenario` must be one non-empty string.", call. = FALSE)
  }
  if (!is.numeric(horizon_months) || length(horizon_months) != 1L ||
        !is.finite(horizon_months) || horizon_months < 1 ||
        horizon_months != round(horizon_months)) {
    stop("`horizon_months` must be a positive whole number.", call. = FALSE)
  }
  portfolio_source <- if (is.null(portfolio)) "synthetic_sample" else "provided"
  if (is.null(portfolio)) portfolio <- ek_sample_alm_portfolio()

  paths <- curves$yields
  keys <- c("scenario_id", "version", "step", "date")
  required <- c(keys, names(curves$tenors))
  if (!all(required %in% names(paths)) || !inherits(paths$date, "Date") ||
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
  path_key <- paste(paths$scenario_id, paths$version, sep = "\r")
  groups <- split(paths, factor(path_key, levels = unique(path_key)))
  groups <- lapply(groups, function(path) path[order(path$step), , drop = FALSE])
  if (any(vapply(groups, function(path) {
    anyDuplicated(path$step) > 0L || nrow(path) < horizon_months
  }, logical(1)))) {
    stop("Each projected curve must have unique steps and cover `horizon_months`.",
         call. = FALSE)
  }
  if (any(vapply(groups, function(path) {
    !identical(path$step, seq_len(nrow(path))) || anyDuplicated(path$date) > 0L
  }, logical(1)))) {
    stop("Projected curve steps must be consecutive and dates unique.", call. = FALSE)
  }
  portfolio <- ek_validate_alm_portfolio(portfolio, names(curves$tenors))

  scenario_results <- lapply(groups, function(path) {
    reference <- paths[
      paths$scenario_id == reference_scenario & paths$version == path$version[1],
      , drop = FALSE
    ]
    if (!nrow(reference)) {
      stop("Reference scenario is missing for version `", path$version[1], "`.",
           call. = FALSE)
    }
    reference <- reference[order(reference$step), , drop = FALSE]
    path <- path[seq_len(horizon_months), , drop = FALSE]
    reference <- reference[seq_len(horizon_months), , drop = FALSE]
    if (!identical(path$step, reference$step) || !identical(path$date, reference$date)) {
      stop("Each scenario and reference curve must have aligned steps and dates.",
           call. = FALSE)
    }
    yield_change <- as.matrix(path[names(curves$tenors)] -
                                reference[names(curves$tenors)]) / 100
    colnames(yield_change) <- names(curves$tenors)
    position_paths <- lapply(seq_len(nrow(portfolio)), function(i) {
      position <- portfolio[i, , drop = FALSE]
      side <- if (position$side == "asset") 1 else -1
      tenor_change <- yield_change[, position$tenor]
      if (position$repricing == "deposit") {
        rate_change <- position$deposit_beta * yield_change[, "DGS3MO"]
      } else if (position$repricing == "floating") {
        reset <- (seq_len(horizon_months) - 1L) %% position$reset_months == 0L
        rate_change <- numeric(horizon_months)
        for (month in seq_len(horizon_months)) {
          if (reset[month]) {
            rate_change[month] <- tenor_change[month]
          } else {
            rate_change[month] <- rate_change[month - 1L]
          }
        }
      } else {
        rate_change <- numeric(horizon_months)
      }
      data.frame(
        scenario_id = path$scenario_id[1], version = path$version[1],
        position = position$position, side = position$side,
        step = path$step, date = path$date, rate_change = rate_change,
        nii_change = side * position$balance * rate_change / 12,
        eve_change = -side * position$balance * position$duration * tenor_change,
        stringsAsFactors = FALSE
      )
    })
    detail <- do.call(rbind, position_paths)
    detail <- detail[order(detail$position, detail$step), , drop = FALSE]
    baseline_nii <- sum(ifelse(portfolio$side == "asset", 1, -1) *
                          portfolio$balance * portfolio$rate) / 12
    aggregate_paths <- stats::aggregate(
      cbind(nii_change = detail$nii_change, eve_change = detail$eve_change),
      by = list(scenario_id = detail$scenario_id, version = detail$version,
                step = detail$step, date = detail$date), FUN = sum
    )
    aggregate_paths$base_nii <- baseline_nii
    aggregate_paths$scenario_nii <- baseline_nii + aggregate_paths$nii_change
    aggregate_paths <- aggregate_paths[
      c("scenario_id", "version", "step", "date", "base_nii", "scenario_nii",
        "nii_change", "eve_change")
    ]
    if (any(!is.finite(aggregate_paths$nii_change)) ||
          any(!is.finite(aggregate_paths$eve_change)) ||
          any(!is.finite(aggregate_paths$scenario_nii))) {
      stop("ALM calculations produced non-finite NII or EVE changes.", call. = FALSE)
    }
    list(
      summary = data.frame(
        scenario_id = path$scenario_id[1], version = path$version[1],
        horizon_months = as.integer(horizon_months),
        base_nii = baseline_nii * horizon_months,
        scenario_nii = baseline_nii * horizon_months + sum(aggregate_paths$nii_change),
        nii_change = sum(aggregate_paths$nii_change),
        terminal_eve_change = aggregate_paths$eve_change[horizon_months],
        stringsAsFactors = FALSE
      ),
      paths = aggregate_paths,
      positions = detail
    )
  })
  bind <- function(name) {
    result <- do.call(rbind, lapply(scenario_results, `[[`, name))
    rownames(result) <- NULL
    result
  }
  list(
    model_version = "alm-static-repricing-v1",
    reference_scenario = reference_scenario,
    horizon_months = as.integer(horizon_months),
    scenario_synthetic = curves$synthetic,
    portfolio_source = portfolio_source,
    summary = bind("summary"),
    paths = bind("paths"),
    positions = bind("positions")
  )
}

ek_sample_alm_portfolio <- function() {
  data.frame(
    position = c("commercial_loans", "fixed_mortgages", "floating_loans",
                 "core_deposits", "wholesale_funding"),
    side = c("asset", "asset", "asset", "liability", "liability"),
    balance = c(300e6, 180e6, 120e6, 420e6, 100e6),
    rate = c(0.072, 0.055, 0.068, 0.018, 0.045),
    tenor = c("DGS5", "DGS10", "DGS2", "DGS3MO", "DGS2"),
    repricing = c("fixed", "fixed", "floating", "deposit", "floating"),
    duration = c(3.2, 6.5, 1.8, 2.1, 1.6),
    reset_months = c(12L, 12L, 3L, 1L, 3L),
    deposit_beta = c(0, 0, 0, 0.35, 0),
    stringsAsFactors = FALSE
  )
}

ek_validate_alm_portfolio <- function(portfolio, tenors) {
  required <- c("position", "side", "balance", "rate", "tenor", "repricing",
                "duration", "reset_months", "deposit_beta")
  if (!is.data.frame(portfolio) || !nrow(portfolio) ||
        anyDuplicated(names(portfolio)) || !setequal(names(portfolio), required)) {
    stop("`portfolio` must contain exactly the required ALM position columns.",
         call. = FALSE)
  }
  text <- c("position", "side", "tenor", "repricing")
  if (!all(vapply(portfolio[text], is.character, logical(1))) ||
        anyNA(as.matrix(portfolio[text])) ||
        any(!nzchar(trimws(as.matrix(portfolio[text]))))) {
    stop("ALM position, side, tenor and repricing fields must be non-empty strings.",
         call. = FALSE)
  }
  numeric <- setdiff(required, text)
  if (!all(vapply(portfolio[numeric], is.numeric, logical(1))) ||
        any(!is.finite(as.matrix(portfolio[numeric])))) {
    stop("ALM balances, rates, durations, reset months and betas must be finite numbers.",
         call. = FALSE)
  }
  if (anyDuplicated(portfolio$position) || any(!portfolio$side %in% c("asset", "liability")) ||
        any(portfolio$balance < 0) || !is.finite(sum(portfolio$balance)) ||
        sum(portfolio$balance) <= 0 ||
        any(portfolio$rate < 0 | portfolio$rate >= 1) ||
        any(!portfolio$tenor %in% tenors) ||
        any(!portfolio$repricing %in% c("fixed", "floating", "deposit")) ||
        any(portfolio$duration < 0 | portfolio$duration > 100) ||
        any(portfolio$reset_months < 1 | portfolio$reset_months > 600 |
              portfolio$reset_months != round(portfolio$reset_months)) ||
        any(portfolio$deposit_beta < 0 | portfolio$deposit_beta > 1)) {
    stop("ALM positions contain invalid sides, balances, rates or repricing assumptions.",
         call. = FALSE)
  }
  if (any(portfolio$repricing == "deposit" & portfolio$side != "liability")) {
    stop("Deposit positions must be liabilities.", call. = FALSE)
  }
  invisible(portfolio)
}
