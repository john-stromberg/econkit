#* @apiTitle econkit API
#* @apiDescription Endpoints for econkit macro data, models and scenario workflows. When the
#*   ECONKIT_API_KEYS environment variable is set (comma-separated keys), every route except
#*   /health requires an X-API-Key header with one of the keys.
#* @serializer json list(na = "null", auto_unbox = TRUE)

library(econkit)

#* Require an API key when ECONKIT_API_KEYS is set
#* @filter api_key
function(req, res) {
  keys <- ek_api_keys()
  path <- req$PATH_INFO
  open <- path %in% c("/health", "/openapi.json") || startsWith(path, "/__docs__") ||
    startsWith(path, "/__swagger__")
  if (length(keys) && !open) {
    key <- req$HTTP_X_API_KEY
    if (is.null(key) || !key %in% keys) {
      res$status <- 401L
      return(list(error = "Missing or invalid API key (X-API-Key header)."))
    }
  }
  plumber::forward()
}

#* Health check
#* @get /health
function() {
  list(status = "ok", version = as.character(utils::packageVersion("econkit")),
       auth = length(ek_api_keys()) > 0)
}

#* Current sample macro signals
#* @param as_of Optional snapshot date (YYYY-MM-DD).
#* @get /v1/signals
function(as_of = "") {
  snapshot <- if (!nzchar(as_of)) NULL else as.Date(as_of)
  postgres_settings <- c(
    "ECONKIT_PGHOST", "ECONKIT_PGDATABASE", "ECONKIT_PGUSER"
  )
  if (any(nzchar(Sys.getenv(postgres_settings, unset = "")))) {
    conn <- ek_connect_postgres()
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
    signals <- ek_read_signals_db(conn, as_of = snapshot)
    return(list(source = "postgres", signals = signals))
  }
  data <- ek_get_series(source = "sample")
  signals <- ek_build_signals(data, as_of = snapshot)
  list(source = "synthetic_sample", signals = signals)
}

#* Series registry
#* @serializer unboxedJSON list(na = "null")
#* @get /v1/catalog
function() {
  catalog <- ek_series_catalog()
  list(
    meta = list(
      n_series = nrow(catalog),
      fred_available = nzchar(Sys.getenv("FRED_API_KEY"))
    ),
    data = catalog
  )
}

#* Macro series in long format with provenance
#* @param ids Comma-separated series ids; empty returns the sample series.
#* @param source "sample" (default), "fred" (requires FRED_API_KEY on the server) or a
#*   source registered on the server with ek_register_source().
#* @param start Optional start date (YYYY-MM-DD).
#* @param end Optional end date (YYYY-MM-DD).
#* @param frequency "month" (default) or "quarter".
#* @param vintage Optional as-of date for point-in-time data (YYYY-MM-DD).
#* @serializer unboxedJSON list(na = "null")
#* @get /v1/series
function(res, ids = "", source = "sample", start = "", end = "", frequency = "month",
         vintage = "") {
  tryCatch({
    x <- ek_api_series(ids, source, start, end, frequency, vintage)
    prov <- ek_provenance(x)
    data <- ek_series_long(x)
    data$date <- format(data$date)
    list(
      meta = list(
        source = prov$source,
        synthetic = prov$synthetic,
        frequency = prov$frequency,
        vintage = prov$vintage,
        vintage_method = prov$vintage_method,
        retrieved_at = prov$retrieved_at,
        econkit_version = prov$econkit_version,
        series = prov$series,
        n_obs = nrow(data)
      ),
      data = data
    )
  }, error = function(error) ek_api_error(res, error))
}

#* Series extract as a Parquet or CSV file for BI tools
#* @param ids Comma-separated series ids; empty returns the sample series.
#* @param source Data source, as for /v1/series.
#* @param start Optional start date (YYYY-MM-DD).
#* @param end Optional end date (YYYY-MM-DD).
#* @param frequency "month" (default) or "quarter".
#* @param vintage Optional as-of date for point-in-time data (YYYY-MM-DD).
#* @param format "parquet" (default; needs arrow on the server) or "csv".
#* @get /v1/series/export
function(res, ids = "", source = "sample", start = "", end = "", frequency = "month",
         vintage = "", format = "parquet") {
  tryCatch({
    if (!format %in% c("parquet", "csv")) stop("`format` must be \"parquet\" or \"csv\".")
    x <- ek_api_series(ids, source, start, end, frequency, vintage)
    path <- tempfile(fileext = paste0(".", format))
    on.exit(unlink(c(path, paste0(path, ".provenance.json"))), add = TRUE)
    out <- ek_export_series(x, path, format = format)
    prov <- ek_provenance(x)
    res$setHeader("Content-Type", if (format == "csv") {
      "text/csv; charset=utf-8"
    } else {
      "application/vnd.apache.parquet"
    })
    res$setHeader("Content-Disposition",
                  sprintf("attachment; filename=\"econkit-series.%s\"", format))
    res$setHeader("X-Econkit-Data-MD5", out$data_md5)
    res$setHeader("X-Econkit-Source", prov$source)
    res$setHeader("X-Econkit-Synthetic", tolower(prov$synthetic))
    res$setHeader("X-Econkit-Vintage", prov$vintage)
    res$setHeader("X-Econkit-Vintage-Method", prov$vintage_method)
    res$body <- readBin(path, "raw", file.info(path)$size)
    res
  }, error = function(error) ek_api_error(res, error))
}

#* Registered data sources
#* @serializer unboxedJSON list(na = "null")
#* @get /v1/sources
function() {
  sources <- ek_sources()
  list(meta = list(n_sources = nrow(sources)), data = sources)
}

#* Expected release dates of registry series
#* @param ids Comma-separated series ids; empty returns all registry series.
#* @param from Start of the window (YYYY-MM-DD); defaults to today.
#* @param to End of the window (YYYY-MM-DD); defaults to 90 days after `from`.
#* @param frequency "month" (default) or "quarter".
#* @serializer unboxedJSON list(na = "null")
#* @get /v1/calendar
function(res, ids = "", from = "", to = "", frequency = "month") {
  tryCatch({
    if (!frequency %in% c("month", "quarter")) {
      stop("`frequency` must be \"month\" or \"quarter\".")
    }
    from <- if (nzchar(from)) ek_api_date(from, "from") else Sys.Date()
    to <- if (nzchar(to)) ek_api_date(to, "to") else from + 90
    calendar <- ek_release_calendar(ek_api_ids(ids), from = from, to = to,
                                    frequency = frequency)
    for (col in c("period", "period_end", "expected_release")) {
      calendar[[col]] <- format(calendar[[col]])
    }
    list(
      meta = list(from = format(from), to = format(to), frequency = frequency,
                  n_releases = nrow(calendar),
                  note = "Estimated from registry release lags, not official schedules."),
      data = calendar
    )
  }, error = function(error) ek_api_error(res, error))
}

#* Current-quarter nowcast from monthly indicators (sample demo)
#* @param vintage Optional as-of date (YYYY-MM-DD) for a point-in-time nowcast.
#* @param n_factors Number of factors.
#* @param level Prediction-interval coverage.
#* @serializer unboxedJSON list(na = "null")
#* @get /v1/nowcast
function(res, vintage = "", n_factors = "1", level = "0.8") {
  tryCatch({
    vintage <- if (nzchar(vintage)) ek_api_date(vintage, "vintage") else NULL
    fit <- ek_nowcast(vintage = vintage,
                      n_factors = ek_api_number(n_factors, "n_factors", 1, 3, integer = TRUE),
                      level = ek_api_number(level, "level", 0.5, 0.99))
    nowcast <- fit$nowcast
    nowcast$quarter <- format(nowcast$quarter)
    list(
      meta = list(target = fit$target, indicators = fit$indicators,
                  vintage = if (is.null(vintage)) NA_character_ else format(vintage),
                  r_squared = fit$r_squared, level = fit$level, synthetic = TRUE,
                  model = "ek_nowcast"),
      data = nowcast
    )
  }, error = function(error) ek_api_error(res, error))
}

#* BVAR forecast with optional conditions
#* @param req JSON body: optional `vars`, `source`, `vintage`, `p`, `lambda`, `h`, `draws`,
#*   `level`, `seed` and `conditions` (object of arrays by variable; null leaves a period free).
#* @parser json list(simplifyVector = FALSE)
#* @serializer unboxedJSON list(na = "null")
#* @post /v1/forecast
function(req, res) {
  tryCatch({
    body <- ek_api_body(req, c("vars", "source", "vintage", "p", "lambda", "h", "draws",
                               "level", "seed", "conditions"))
    spec <- ek_api_bvar_spec(body)
    fit <- ek_bvar(spec$data, p = spec$p, lambda = spec$lambda)
    fc <- ek_bvar_forecast(fit, h = spec$h, conditions = ek_api_conditions(body$conditions),
                           draws = spec$draws, level = spec$level, seed = spec$seed)
    summary <- fc$summary
    summary$date <- format(summary$date)
    meta <- c(ek_api_bvar_meta(fit, spec), list(level = fc$level, h = spec$h, draws = spec$draws))
    list(meta = meta, data = summary)
  }, error = function(error) ek_api_error(res, error))
}

#* Build a macro scenario library from BVAR scenarios
#* @param req JSON body: model options as for /v1/forecast (monthly data only), `version`,
#*   `path` ("central", "median", "mean", "lower" or "upper") and `scenarios`, an object
#*   of scenario ids to condition objects (use {} for unconditional). Defaults to
#*   {"base": {}}.
#* @parser json list(simplifyVector = FALSE)
#* @serializer unboxedJSON list(na = "null")
#* @post /v1/scenarios/build
function(req, res) {
  tryCatch({
    body <- ek_api_body(req, c("vars", "source", "vintage", "p", "lambda", "h", "draws",
                               "level", "seed", "version", "path", "scenarios"))
    spec <- ek_api_bvar_spec(body)
    scenarios <- if (is.null(body$scenarios)) list(base = list()) else body$scenarios
    if (!is.list(scenarios) || !length(scenarios) || is.null(names(scenarios)) ||
          length(scenarios) > 10) {
      stop("`scenarios` must be an object of 1 to 10 named condition objects.")
    }
    fit <- ek_bvar(spec$data, p = spec$p, lambda = spec$lambda)
    forecasts <- lapply(scenarios, function(conditions) {
      ek_bvar_forecast(fit, h = spec$h, conditions = ek_api_conditions(conditions),
                       draws = spec$draws, level = spec$level, seed = spec$seed)
    })
    lib <- ek_scenario_from_forecast(
      forecasts, version = ek_api_string(body$version, "version", "bvar-v1"),
      path = ek_api_string(body$path, "path", "central")
    )
    paths <- lib$paths
    for (col in c("as_of", "data_vintage", "date")) paths[[col]] <- format(paths[[col]])
    list(meta = c(ek_api_bvar_meta(fit, spec),
                  list(version = paths$version[1], scenarios = names(forecasts),
                       h = spec$h, synthetic = lib$synthetic, asset_shocks = FALSE)),
         paths = paths)
  }, error = function(error) ek_api_error(res, error))
}

#* Regime-conditioned asset-return playbook
#* @param min_obs Minimum observations for eligibility.
#* @get /v1/positioning/playbook
function(min_obs = 24L) {
  playbook <- ek_positioning_playbook(min_obs = as.integer(min_obs))
  list(source = "synthetic_sample", playbook = playbook)
}

#* Strategic asset-allocation assumptions
#* @param lookback_years Monthly observation lookback in years.
#* @param as_of Latest date to include (YYYY-MM-DD).
#* @get /v1/saa/assumptions
function(lookback_years = 10L, as_of = "") {
  cutoff <- if (!nzchar(as_of)) NULL else as.Date(as_of)
  assumptions <- ek_saa_assumptions(
    lookback_years = as.integer(lookback_years),
    as_of = cutoff
  )
  list(
    source = if (assumptions$synthetic) "synthetic_sample" else "provided_returns",
    assumptions = assumptions$assumptions,
    covariance = assumptions$covariance,
    as_of = assumptions$as_of,
    sample_start = assumptions$sample_start,
    sample_end = assumptions$sample_end,
    lookback_years = assumptions$lookback_years,
    note = "Historical estimates, not forward-looking capital-market forecasts."
  )
}

#* Illustrative macro and asset-return scenarios
#* @get /v1/scenarios
function() {
  scenarios <- ek_scenario_library()
  list(
    source = if (scenarios$synthetic) "synthetic_sample" else "provided",
    paths = scenarios$paths,
    asset_returns = scenarios$asset_returns
  )
}

#* Illustrative credit stress for a sample banking portfolio
#* @get /v1/banking/credit-stress
function() {
  ek_credit_scenario_stress()
}

#* Apply sample macro scenarios to a supplied credit portfolio
#* @param req JSON request body containing a `portfolio` array of segments.
#* @parser json list(simplifyVector = FALSE)
#* @post /v1/banking/credit-stress
function(req, res) {
  tryCatch(
    ek_credit_scenario_stress(portfolio = ek_api_credit_portfolio(req)),
    econkit_credit_input_error = function(error) {
      res$status <- 400L
      list(error = conditionMessage(error))
    }
  )
}

#* Apply a scenario set to a strategic policy mix
#* @param req JSON request body containing a named `weights` object.
#* @post /v1/saa/stress
function(req) {
  scenarios <- ek_scenario_library()
  stress <- ek_saa_scenario_stress(scenarios, ek_api_weights(req))
  list(
    source = if (stress$synthetic) "synthetic_sample" else "provided",
    summary = stress$summary,
    contributions = stress$contributions
  )
}

#* Summarize a strategic policy mix
#* @param req JSON request body containing a named `weights` object.
#* @post /v1/saa/portfolio
function(req) {
  weights <- ek_api_weights(req)
  assumptions <- ek_saa_assumptions()
  portfolio <- ek_saa_portfolio(assumptions, weights)
  list(
    source = if (assumptions$synthetic) "synthetic_sample" else "provided_returns",
    portfolio = portfolio
  )
}

ek_api_weights <- function(req) {
  if (is.null(req$body$weights)) {
    stop("Request JSON must contain a named `weights` object.", call. = FALSE)
  }
  unlist(req$body$weights, use.names = TRUE)
}

ek_api_keys <- function() {
  keys <- trimws(strsplit(Sys.getenv("ECONKIT_API_KEYS"), ",", fixed = TRUE)[[1]])
  keys[nzchar(keys)]
}

ek_api_error <- function(res, error) {
  res$status <- 400L
  list(error = conditionMessage(error))
}

ek_api_ids <- function(ids) {
  if (!nzchar(ids)) return(NULL)
  trimws(strsplit(ids, ",", fixed = TRUE)[[1]])
}

ek_api_date <- function(x, name) {
  date <- tryCatch(as.Date(x), error = function(e) NA)
  if (length(date) != 1L || is.na(date)) stop("`", name, "` must be a date (YYYY-MM-DD).")
  date
}

ek_api_number <- function(x, name, min, max, default = NULL, integer = FALSE) {
  if (is.null(x)) return(default)
  value <- suppressWarnings(as.numeric(if (is.list(x)) unlist(x) else x))
  if (length(value) != 1L || !is.finite(value) || value < min || value > max ||
        (integer && value != round(value))) {
    stop("`", name, "` must be ", if (integer) "an integer" else "a number",
         " between ", min, " and ", max, ".")
  }
  if (integer) as.integer(value) else value
}

ek_api_string <- function(x, name, default) {
  if (is.null(x)) return(default)
  if (!is.character(x) || length(x) != 1L || !nzchar(x)) {
    stop("`", name, "` must be a non-empty string.")
  }
  x
}

ek_api_series <- function(ids, source, start, end, frequency, vintage) {
  empty_null <- function(x) if (!nzchar(x)) NULL else x
  if (!source %in% ek_sources()$name) {
    stop("Unknown `source`. Available: ", paste(ek_sources()$name, collapse = ", "), ".")
  }
  if (!frequency %in% c("month", "quarter")) {
    stop("`frequency` must be \"month\" or \"quarter\".")
  }
  ek_get_series(
    ids = ek_api_ids(ids), source = source, start = empty_null(start), end = empty_null(end),
    frequency = frequency, vintage = empty_null(vintage)
  )
}

ek_api_body <- function(req, allowed) {
  body <- req$body
  if (is.null(body) || (is.list(body) && !length(body))) return(list())
  if (!is.list(body) || is.null(names(body))) stop("Request JSON must be an object.")
  unknown <- setdiff(names(body), allowed)
  if (length(unknown)) stop("Unknown field(s): ", paste(unknown, collapse = ", "), ".")
  body
}

ek_api_bvar_spec <- function(body) {
  vars <- if (is.null(body$vars)) {
    c("FEDFUNDS", "UNRATE", "PCEPILFE", "INDPRO")
  } else {
    unlist(body$vars)
  }
  if (!is.character(vars) || length(vars) < 2L || length(vars) > 8L || anyDuplicated(vars)) {
    stop("`vars` must be 2 to 8 distinct series ids.")
  }
  source <- ek_api_string(body$source, "source", "sample")
  if (!source %in% ek_sources()$name) stop("Unknown `source`.")
  vintage <- if (is.null(body$vintage)) NULL else ek_api_date(body$vintage, "vintage")
  list(
    data = ek_get_series(vars, source = source, vintage = vintage),
    source = source, vintage = vintage,
    p = ek_api_number(body$p, "p", 1, 12, 4L, integer = TRUE),
    lambda = ek_api_number(body$lambda, "lambda", 0.01, 10, 0.2),
    h = ek_api_number(body$h, "h", 1, 60, 12L, integer = TRUE),
    draws = ek_api_number(body$draws, "draws", 1, 2000, 500L, integer = TRUE),
    level = ek_api_number(body$level, "level", 0.5, 0.99, 0.8),
    seed = ek_api_number(body$seed, "seed", 0, .Machine$integer.max, 1L, integer = TRUE)
  )
}

ek_api_bvar_meta <- function(fit, spec) {
  prov <- ek_provenance(spec$data)
  list(model = "ek_bvar", vars = I(fit$vars), log_vars = I(fit$log_vars), p = fit$p,
       lambda = fit$lambda, n_obs = fit$n_obs, last_date = format(fit$last_date),
       source = prov$source, synthetic = prov$synthetic, vintage = prov$vintage,
       vintage_method = prov$vintage_method, seed = spec$seed)
}

ek_api_conditions <- function(conditions) {
  if (is.null(conditions) || !length(conditions)) return(NULL)
  if (!is.list(conditions) || is.null(names(conditions))) {
    stop("`conditions` must be an object of arrays by variable.")
  }
  lapply(conditions, function(values) {
    if (!is.list(values) && !is.numeric(values)) {
      stop("Each condition must be an array of numbers or nulls.")
    }
    vapply(as.list(values), function(v) {
      if (is.null(v)) return(NA_real_)
      if (!is.numeric(v) || length(v) != 1L) {
        stop("Each condition must be an array of numbers or nulls.")
      }
      as.numeric(v)
    }, numeric(1))
  })
}

ek_api_credit_portfolio <- function(req) {
  body <- req$body
  required <- c("segment", "ead", "pd_12m", "lgd", "unemployment_beta")
  invalid <- function() {
    stop(errorCondition(
      "Request JSON must contain a portfolio array with one complete segment per object.",
      class = "econkit_credit_input_error"
    ))
  }
  if (!is.list(body) || !identical(names(body), "portfolio")) invalid()
  portfolio <- body$portfolio
  if (!is.list(portfolio) || !length(portfolio) || !is.null(names(portfolio))) {
    invalid()
  }
  valid <- vapply(portfolio, function(row) {
    if (!is.list(row) || is.null(names(row)) ||
          anyDuplicated(names(row)) || !setequal(names(row), required)) {
      return(FALSE)
    }
    is.character(row$segment) && length(row$segment) == 1L &&
      all(vapply(required[-1], function(column) {
        is.numeric(row[[column]]) && length(row[[column]]) == 1L
      }, logical(1)))
  }, logical(1))
  if (!all(valid)) invalid()
  data.frame(
    segment = vapply(portfolio, `[[`, character(1), "segment"),
    ead = vapply(portfolio, `[[`, numeric(1), "ead"),
    pd_12m = vapply(portfolio, `[[`, numeric(1), "pd_12m"),
    lgd = vapply(portfolio, `[[`, numeric(1), "lgd"),
    unemployment_beta = vapply(portfolio, `[[`, numeric(1), "unemployment_beta"),
    stringsAsFactors = FALSE
  )
}
