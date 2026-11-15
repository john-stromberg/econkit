#' Fit macro satellites for credit LGD and EAD
#'
#' Fits per-segment satellites to the logit of LGD and log of a positive EAD
#' index. The EAD projection is returned as a multiplier relative to the
#' latest observed index, allowing it to scale a portfolio's as-of exposure.
#'
#' @section Model card:
#' **Purpose:** translate macro scenarios into illustrative collateral-loss and
#' exposure changes for credit stress analysis. **Assumptions:** contemporaneous
#' linear relationships in transformed LGD and EAD, stable segment definitions,
#' and a positive EAD index. **Limitations:** no collateral valuation,
#' recoveries, utilization, loan-level mix, lags, uncertainty or causal
#' interpretation. The bundled history is synthetic; institution-specific
#' histories and independent validation are required for professional use.
#'
#' @param history Long data frame with Date `date`, character `segment`,
#'   numeric `lgd` strictly between zero and one, and positive `ead_index`.
#'   Defaults to the synthetic `credit_loss_components` sample.
#' @param macro Wide data frame with `date` and all `drivers`, at the same
#'   monthly or quarterly frequency as `history`. Defaults to matching sample
#'   macro data.
#' @param drivers Macro driver names, defaulting to `UNRATE` and `NFCI`.
#' @return An `ek_credit_lgd_ead_satellites` object with per-segment LGD and
#'   EAD models, fit diagnostics, dates, drivers and synthetic-data metadata.
#' @export
#' @examples
#' fit <- ek_credit_lgd_ead_satellite_fit()
#' projected <- ek_credit_lgd_ead_project(fit, ek_scenario_library())
#' head(projected)
ek_credit_lgd_ead_satellite_fit <- function( # nolint: object_length_linter
  history = NULL, macro = NULL, drivers = c("UNRATE", "NFCI")) {
  synthetic_history <- is.null(history)
  if (is.null(history)) history <- ek_load_dataset("credit_loss_components")
  ek_check_wide(history, c("segment", "lgd", "ead_index"))
  if (!is.character(history$segment) || anyNA(history$segment) ||
        any(!nzchar(trimws(history$segment))) ||
        !is.numeric(history$lgd) || !is.numeric(history$ead_index)) {
    stop("`history` needs character `segment` and numeric `lgd` and `ead_index` columns.",
         call. = FALSE)
  }
  if (anyDuplicated(history[c("date", "segment")])) {
    stop("`history` must have one row per date and segment.", call. = FALSE)
  }
  observed_lgd <- history$lgd[!is.na(history$lgd)]
  observed_ead <- history$ead_index[!is.na(history$ead_index)]
  if (any(!is.finite(observed_lgd)) || any(observed_lgd <= 0 | observed_lgd >= 1)) {
    stop("Observed `lgd` values must be finite and strictly between zero and one.",
         call. = FALSE)
  }
  if (any(!is.finite(observed_ead)) || any(observed_ead <= 0)) {
    stop("Observed `ead_index` values must be positive and finite.", call. = FALSE)
  }
  if (!is.character(drivers) || !length(drivers) || anyNA(drivers) ||
        any(!nzchar(drivers)) || anyDuplicated(drivers) ||
        any(drivers %in% c("date", "segment", "lgd", "ead_index",
                           "logit_lgd", "log_ead_index"))) {
    stop("`drivers` must be distinct macro series names.", call. = FALSE)
  }
  if (is.null(macro)) {
    frequency <- if (ek_is_sparse(unique(history$date))) "quarter" else "month"
    macro <- ek_get_series(drivers, frequency = frequency)
  }
  ek_check_wide(macro, drivers)
  if (!all(vapply(macro[drivers], is.numeric, logical(1))) ||
        any(vapply(macro[drivers], function(x) any(!is.na(x) & !is.finite(x)),
                   logical(1)))) {
    stop("Macro drivers must contain finite values or NA.", call. = FALSE)
  }
  if (anyDuplicated(macro$date)) {
    stop("`macro` must have one row per date.", call. = FALSE)
  }

  segments <- sort(unique(history$segment))
  fits <- lapply(segments, function(segment) {
    rows <- history[history$segment == segment,
                    c("date", "lgd", "ead_index"), drop = FALSE]
    data <- merge(rows, macro[c("date", drivers)], by = "date")
    data$logit_lgd <- stats::qlogis(data$lgd)
    data$log_ead_index <- log(data$ead_index)
    data <- data[stats::complete.cases(data[c("logit_lgd", "log_ead_index", drivers)]),
                 , drop = FALSE]
    if (!nrow(data)) {
      stop("No complete `history` and `macro` observations for segment `",
           segment, "`.", call. = FALSE)
    }
    lgd_model <- tryCatch(
      ek_satellite_fit(data[c("date", "logit_lgd", drivers)],
                       "logit_lgd", drivers),
      error = function(error) {
        stop("Segment `", segment, "` LGD model: ", conditionMessage(error),
             call. = FALSE)
      }
    )
    ead_model <- tryCatch(
      ek_satellite_fit(data[c("date", "log_ead_index", drivers)],
                       "log_ead_index", drivers),
      error = function(error) {
        stop("Segment `", segment, "` EAD model: ", conditionMessage(error),
             call. = FALSE)
      }
    )
    list(
      models = list(lgd = lgd_model, ead = ead_model),
      fit = data.frame(
        segment = segment, lgd_r_squared = lgd_model$r_squared,
        lgd_sigma = lgd_model$sigma, ead_r_squared = ead_model$r_squared,
        ead_sigma = ead_model$sigma, n_obs = min(lgd_model$n_obs, ead_model$n_obs),
        start = min(data$date), end = max(data$date), stringsAsFactors = FALSE
      )
    )
  })
  macro_provenance <- attr(macro, "provenance")
  list(
    models = stats::setNames(lapply(fits, `[[`, "models"), segments),
    fit = do.call(rbind, lapply(fits, `[[`, "fit")),
    segments = segments,
    drivers = drivers,
    last_date = as.Date(
      max(vapply(fits, function(x) as.numeric(x$models$lgd$last_date), numeric(1))),
      origin = "1970-01-01"
    ),
    source = if (is.null(macro_provenance$source)) {
      if (synthetic_history) "sample" else "provided"
    } else {
      macro_provenance$source
    },
    synthetic = synthetic_history || isTRUE(attr(history, "synthetic")) ||
      isTRUE(macro_provenance$synthetic),
    model_version = "credit-lgd-ead-satellite-v1"
  ) |>
    structure(class = "ek_credit_lgd_ead_satellites")
}

#' Project credit LGD and EAD satellites along scenarios
#'
#' Applies fitted segment models to macro paths, converts fitted logit-LGD
#' back to a proportion and expresses EAD as a multiplier relative to the
#' latest observed EAD index.
#'
#' @param fit An `ek_credit_lgd_ead_satellites` object.
#' @param scenarios An `ek_scenario_library` or macro-path data frame.
#' @return A data frame keyed by scenario, version, segment, step and date,
#'   with `lgd`, `ead_multiplier` and transformed model values.
#' @export
#' @examples
#' fit <- ek_credit_lgd_ead_satellite_fit()
#' paths <- ek_credit_lgd_ead_project(fit, ek_scenario_library())
#' head(paths)
ek_credit_lgd_ead_project <- function(fit, scenarios) {
  if (!inherits(fit, "ek_credit_lgd_ead_satellites") ||
        !is.list(fit$models) || !length(fit$models)) {
    stop("`fit` must be an ek_credit_lgd_ead_satellites object.", call. = FALSE)
  }
  if (inherits(scenarios, "ek_scenario_library")) {
    ek_validate_scenario_library(scenarios)
    scenario_paths <- scenarios$paths
    scenario_synthetic <- scenarios$synthetic
  } else {
    scenario_paths <- scenarios
    scenario_synthetic <- FALSE
  }
  required <- c("scenario_id", "version", "step", "date", fit$drivers)
  if (!is.data.frame(scenario_paths) || !all(required %in% names(scenario_paths))) {
    stop("Scenario paths need columns: ", paste(required, collapse = ", "), ".",
         call. = FALSE)
  }
  if (!all(vapply(scenario_paths[fit$drivers], is.numeric, logical(1))) ||
        any(!is.finite(as.matrix(scenario_paths[fit$drivers])))) {
    stop("Scenario drivers must be finite numeric values.", call. = FALSE)
  }
  keys <- c("scenario_id", "version", "step", "date")
  projected <- lapply(names(fit$models), function(segment) {
    models <- fit$models[[segment]]
    lgd <- ek_satellite_project(models$lgd, scenario_paths)
    ead <- ek_satellite_project(models$ead, scenario_paths)
    out <- merge(lgd, ead, by = keys, sort = FALSE)
    out$segment <- segment
    out$lgd <- stats::plogis(out$logit_lgd)
    out$ead_multiplier <- exp(out$log_ead_index - models$ead$last_y)
    out[c(keys, "segment", "lgd", "ead_multiplier", "logit_lgd", "log_ead_index")]
  })
  out <- do.call(rbind, projected)
  rownames(out) <- NULL
  out <- out[order(out$scenario_id, out$version, out$segment, out$step), , drop = FALSE]
  if (any(!is.finite(out$lgd)) || any(out$lgd <= 0 | out$lgd >= 1) ||
        any(!is.finite(out$ead_multiplier)) || any(out$ead_multiplier <= 0)) {
    stop("Credit component projection produced invalid LGD or EAD values.",
         call. = FALSE)
  }
  attr(out, "synthetic") <- fit$synthetic || scenario_synthetic
  attr(out, "model_version") <- fit$model_version
  out
}
