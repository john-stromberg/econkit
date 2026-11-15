#' Build or validate a versioned macro scenario library
#'
#' With no inputs, creates illustrative 12-month base, adverse and severe
#' paths anchored to the latest available macro observation. Sample asset
#' return shocks are deliberately stylized, not forecasts or calibrated
#' market estimates. Custom libraries can be supplied as macro paths and
#' matching asset-return shocks.
#'
#' Macro paths require `scenario_id`, `version`, `as_of`, `data_vintage`,
#' `step`, `date`, `INDPRO`, `CPIAUCSL`, `UNRATE`, `T10Y3M` and `NFCI`.
#' Asset shocks require `scenario_id`, `version`, `horizon_months`,
#' `asset_class` and `scenario_return`, where returns are cumulative decimal
#' returns over the same horizon.
#'
#' @param paths Optional data frame of monthly macro paths.
#' @param asset_returns Optional data frame of cumulative asset-class shocks.
#' @param data Monthly macro data used to anchor the default sample paths.
#' @param as_of Optional latest macro observation date used for anchoring.
#' @param version Version label for generated sample scenarios.
#' @param synthetic Whether custom input data are synthetic. Defaults to
#'   `TRUE` for generated scenarios and `FALSE` for custom inputs.
#' @return An `ek_scenario_library` list containing `paths`, `asset_returns`
#'   and a `synthetic` flag.
#' @export
#' @examples
#' scenarios <- ek_scenario_library()
#' unique(scenarios$paths[c("scenario_id", "version")])
ek_scenario_library <- function(paths = NULL, asset_returns = NULL,
                                data = NULL, as_of = NULL,
                                version = "sample-v1", synthetic = NULL) {
  sample_input <- is.null(paths) && is.null(asset_returns)
  if (xor(is.null(paths), is.null(asset_returns))) {
    stop("Supply both `paths` and `asset_returns`, or neither.", call. = FALSE)
  }
  if (sample_input) {
    if (is.null(data)) data <- ek_load_dataset("macro_monthly")
    generated <- ek_make_sample_scenarios(data, as_of, version)
    paths <- generated$paths
    asset_returns <- generated$asset_returns
    if (is.null(synthetic)) synthetic <- TRUE
  } else {
    if (!is.null(data) || !is.null(as_of)) {
      stop("`data` and `as_of` apply only to generated sample scenarios.",
           call. = FALSE)
    }
    if (is.null(synthetic)) synthetic <- FALSE
  }
  if (!is.logical(synthetic) || length(synthetic) != 1L || is.na(synthetic)) {
    stop("`synthetic` must be one non-missing logical value.", call. = FALSE)
  }
  out <- structure(
    list(paths = paths, asset_returns = asset_returns, synthetic = synthetic),
    class = "ek_scenario_library"
  )
  ek_validate_scenario_library(out)
  out
}

#' Validate a macro scenario library
#'
#' Checks scenario identity, monthly path sequencing, macro values, asset
#' shock coverage and horizon consistency.
#'
#' @param scenarios An `ek_scenario_library` object.
#' @return The input object, invisibly.
#' @export
#' @examples
#' ek_validate_scenario_library(ek_scenario_library())
ek_validate_scenario_library <- function(scenarios) {
  if (!inherits(scenarios, "ek_scenario_library") ||
        !is.list(scenarios) || !is.data.frame(scenarios$paths) ||
        !is.data.frame(scenarios$asset_returns)) {
    stop("`scenarios` must be an `ek_scenario_library` object.", call. = FALSE)
  }
  if (!is.logical(scenarios$synthetic) || length(scenarios$synthetic) != 1L ||
        is.na(scenarios$synthetic)) {
    stop("`scenarios$synthetic` must be one non-missing logical value.",
         call. = FALSE)
  }
  path_columns <- c(
    "scenario_id", "version", "as_of", "data_vintage", "step", "date",
    "INDPRO", "CPIAUCSL", "UNRATE", "T10Y3M", "NFCI"
  )
  shock_columns <- c(
    "scenario_id", "version", "horizon_months", "asset_class",
    "scenario_return"
  )
  paths <- scenarios$paths
  shocks <- scenarios$asset_returns
  if (!all(path_columns %in% names(paths)) ||
        !all(shock_columns %in% names(shocks)) ||
        !nrow(paths) || !nrow(shocks)) {
    stop("Scenario paths or asset shocks are missing required columns or rows.",
         call. = FALSE)
  }
  if (!is.character(paths$scenario_id) || !is.character(paths$version) ||
        !is.character(shocks$scenario_id) || !is.character(shocks$version) ||
        !is.character(shocks$asset_class) ||
        anyNA(paths$scenario_id) || anyNA(paths$version) ||
        anyNA(shocks$scenario_id) || anyNA(shocks$version) ||
        anyNA(shocks$asset_class) || any(!nzchar(paths$scenario_id)) ||
        any(!nzchar(paths$version)) || any(!nzchar(shocks$scenario_id)) ||
        any(!nzchar(shocks$version)) || any(!nzchar(shocks$asset_class))) {
    stop("Scenario identifiers, versions and asset classes must be non-empty.",
         call. = FALSE)
  }
  if (!inherits(paths$as_of, "Date") ||
        !inherits(paths$data_vintage, "Date") ||
        !inherits(paths$date, "Date") || anyNA(paths$as_of) ||
        anyNA(paths$data_vintage) || anyNA(paths$date) ||
        !is.numeric(paths$step) || anyNA(paths$step) ||
        any(!is.finite(paths$step)) || any(paths$step < 1) ||
        any(paths$step != floor(paths$step)) ||
        !is.numeric(shocks$horizon_months) || anyNA(shocks$horizon_months) ||
        any(!is.finite(shocks$horizon_months)) ||
        any(shocks$horizon_months < 1) ||
        any(shocks$horizon_months != floor(shocks$horizon_months))) {
    stop("Scenario dates and steps have invalid types or values.", call. = FALSE)
  }
  macro_columns <- c("INDPRO", "CPIAUCSL", "UNRATE", "T10Y3M", "NFCI")
  if (!all(vapply(paths[macro_columns], is.numeric, logical(1))) ||
        any(!is.finite(as.matrix(paths[macro_columns]))) ||
        any(paths$INDPRO <= 0) || any(paths$CPIAUCSL <= 0) ||
        any(paths$UNRATE < 0) || !is.numeric(shocks$scenario_return) ||
        any(!is.finite(shocks$scenario_return)) ||
        any(shocks$scenario_return <= -1)) {
    stop("Macro paths and asset shocks contain invalid numeric values.",
         call. = FALSE)
  }

  path_key <- paste(paths$scenario_id, paths$version, sep = "\r")
  shock_key <- paste(shocks$scenario_id, shocks$version, sep = "\r")
  path_step_key <- paste(path_key, paths$step, sep = "\r")
  shock_asset_key <- paste(shock_key, shocks$asset_class, sep = "\r")
  if (anyDuplicated(path_step_key) || anyDuplicated(shock_asset_key) ||
        !setequal(unique(path_key), unique(shock_key))) {
    stop("Scenario paths and asset shocks must have unique, matching keys.",
         call. = FALSE)
  }
  for (key in unique(path_key)) {
    path <- paths[path_key == key, , drop = FALSE]
    path <- path[order(path$step), , drop = FALSE]
    if (!identical(as.integer(path$step), seq_len(nrow(path))) ||
          length(unique(path$as_of)) != 1L ||
          length(unique(path$data_vintage)) != 1L ||
          path$data_vintage[1] > path$as_of[1] ||
          any(path$date != ek_add_months(path$as_of[1], path$step))) {
      stop("Each scenario path must have contiguous monthly steps after its as-of date.",
           call. = FALSE)
    }
    scenario_shocks <- shocks[shock_key == key, , drop = FALSE]
    if (length(unique(scenario_shocks$horizon_months)) != 1L ||
          scenario_shocks$horizon_months[1] != nrow(path)) {
      stop("Asset shocks must match the number of monthly scenario steps.",
           call. = FALSE)
    }
  }
  asset_sets <- lapply(unique(shock_key), function(key) {
    sort(shocks$asset_class[shock_key == key])
  })
  if (any(!vapply(asset_sets, function(x) identical(x, asset_sets[[1]]),
                  logical(1)))) {
    stop("Every scenario must provide shocks for the same asset classes.",
         call. = FALSE)
  }
  invisible(scenarios)
}

ek_make_sample_scenarios <- function(data, as_of, version) {
  required <- c("date", "INDPRO", "CPIAUCSL", "UNRATE", "T10Y3M", "NFCI")
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop("`data` is missing columns: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  if (!inherits(data$date, "Date") || anyNA(data$date) ||
        anyDuplicated(data$date) || is.unsorted(data$date, strictly = TRUE)) {
    stop("`data$date` must contain unique, strictly increasing Date values.",
         call. = FALSE)
  }
  if (!is.character(version) || length(version) != 1L || is.na(version) ||
        !nzchar(version)) {
    stop("`version` must be one non-empty string.", call. = FALSE)
  }
  if (!is.null(as_of)) {
    as_of <- as.Date(as_of)
    if (length(as_of) != 1L || is.na(as_of)) {
      stop("`as_of` must be one valid date.", call. = FALSE)
    }
    data <- data[data$date <= as_of, , drop = FALSE]
  }
  if (!nrow(data)) stop("No macro data are available for the scenario anchor.",
                        call. = FALSE)
  macro_columns <- c("INDPRO", "CPIAUCSL", "UNRATE", "T10Y3M", "NFCI")
  if (!all(vapply(data[macro_columns], is.numeric, logical(1)))) {
    stop("Scenario macro columns must be numeric.", call. = FALSE)
  }
  finite <- rowSums(is.finite(as.matrix(data[macro_columns]))) ==
    length(macro_columns)
  anchor_rows <- which(finite)
  if (!length(anchor_rows)) {
    stop("No complete finite macro observations are available for anchoring.",
         call. = FALSE)
  }
  anchor_row <- data[utils::tail(anchor_rows, 1), , drop = FALSE]
  anchor_date <- anchor_row$date[1]
  anchor <- vapply(macro_columns, function(column) {
    anchor_row[[column]][1]
  }, numeric(1))
  step <- seq_len(12L)
  fraction <- step / length(step)
  profile <- list(
    base = list(
      growth = rep(0.0015, 12),
      inflation = rep(0.002, 12),
      unemployment = -0.25,
      term_spread = 0.50,
      financial_conditions = 0.00
    ),
    adverse = list(
      growth = c(rep(-0.003, 4), rep(0.001, 8)),
      inflation = c(rep(0.003, 6), rep(0.002, 6)),
      unemployment = 0.80,
      term_spread = -0.25,
      financial_conditions = 0.75
    ),
    severe = list(
      growth = c(rep(-0.01, 6), rep(0.002, 6)),
      inflation = c(rep(0.004, 6), rep(0.002, 6)),
      unemployment = 2.00,
      term_spread = -1.00,
      financial_conditions = 1.75
    )
  )
  paths <- do.call(rbind, lapply(names(profile), function(scenario_id) {
    p <- profile[[scenario_id]]
    data.frame(
      scenario_id = scenario_id,
      version = version,
      as_of = rep(anchor_date, length(step)),
      data_vintage = rep(anchor_date, length(step)),
      step = step,
      date = ek_add_months(anchor_date, step),
      INDPRO = anchor[["INDPRO"]] * cumprod(1 + p$growth),
      CPIAUCSL = anchor[["CPIAUCSL"]] * cumprod(1 + p$inflation),
      UNRATE = pmax(0, anchor[["UNRATE"]] + p$unemployment * fraction),
      T10Y3M = anchor[["T10Y3M"]] +
        (p$term_spread - anchor[["T10Y3M"]]) * fraction,
      NFCI = anchor[["NFCI"]] +
        (p$financial_conditions - anchor[["NFCI"]]) * fraction,
      stringsAsFactors = FALSE
    )
  }))
  rownames(paths) <- NULL

  assets <- c(
    "global_equity", "government_bonds", "investment_grade_credit",
    "high_yield_credit", "commodities", "gold", "us_dollar", "cash"
  )
  returns <- rbind(
    base = c(0.06, 0.025, 0.04, 0.055, 0.025, 0.03, -0.02, 0.03),
    adverse = c(-0.20, 0.08, -0.05, -0.20, -0.15, 0.10, 0.08, 0.04),
    severe = c(-0.35, 0.12, -0.12, -0.35, -0.25, 0.15, 0.15, 0.05)
  )
  dimnames(returns) <- list(names(profile), assets)
  asset_returns <- do.call(rbind, lapply(names(profile), function(scenario_id) {
    data.frame(
      scenario_id = scenario_id,
      version = version,
      horizon_months = 12L,
      asset_class = assets,
      scenario_return = unname(returns[scenario_id, ]),
      stringsAsFactors = FALSE
    )
  }))
  rownames(asset_returns) <- NULL
  list(paths = paths, asset_returns = asset_returns)
}
