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
function(req, res, vintage = "", n_factors = "1", level = "0.8") {
  tryCatch({
    vintage <- if (nzchar(vintage)) ek_api_date(vintage, "vintage") else NULL
    fit <- ek_nowcast(vintage = vintage,
                      n_factors = ek_api_number(n_factors, "n_factors", 1, 3, integer = TRUE),
                      level = ek_api_number(level, "level", 0.5, 0.99))
    nowcast <- fit$nowcast
    nowcast$quarter <- format(nowcast$quarter)
    run_id <- ek_api_record(
      "nowcast_factor_bridge", fit$factors, fit$nowcast,
      list(vintage = if (is.null(vintage)) NULL else format(vintage),
           n_factors = ncol(fit$loadings), level = fit$level),
      req, "GET /v1/nowcast", metrics = c(r_squared = fit$r_squared)
    )
    list(
      meta = list(target = fit$target, indicators = fit$indicators,
                  vintage = if (is.null(vintage)) NA_character_ else format(vintage),
                  r_squared = fit$r_squared, level = fit$level, synthetic = TRUE,
                  model = "ek_nowcast", run_id = run_id),
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
    meta$run_id <- ek_api_record("bvar_minnesota", spec$data, fc$summary,
                                 ek_api_bvar_params(spec, body), req, "POST /v1/forecast")
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
    if (is.null(body$scenarios)) body$scenarios <- list(base = list())
    built <- ek_api_build_scenarios(body)
    spec <- built$spec
    lib <- built$library
    paths <- lib$paths
    for (col in c("as_of", "data_vintage", "date")) paths[[col]] <- format(paths[[col]])
    params <- c(ek_api_bvar_params(spec, body),
                list(version = paths$version[1], scenarios = body$scenarios))
    run_id <- ek_api_record("bvar_minnesota", spec$data, lib$paths, params, req,
                            "POST /v1/scenarios/build")
    list(meta = c(ek_api_bvar_meta(built$fit, spec),
                  list(version = paths$version[1], scenarios = unique(paths$scenario_id),
                       h = spec$h, synthetic = lib$synthetic, asset_shocks = FALSE,
                       run_id = run_id)),
         paths = paths)
  }, error = function(error) ek_api_error(res, error))
}

#* Model inventory with monitoring thresholds
#* @param id Optional model id.
#* @serializer unboxedJSON list(na = "null")
#* @get /v1/models
function(res, id = "") {
  models <- ek_model_registry()
  monitoring <- ek_model_registry(monitoring = TRUE)
  models$last_review <- format(models$last_review)
  if (nzchar(id)) {
    if (!id %in% models$id) {
      res$status <- 404L
      return(list(error = paste0("Unknown model `", id, "`.")))
    }
    models <- models[models$id == id, , drop = FALSE]
    monitoring <- monitoring[monitoring$model_id == id, , drop = FALSE]
  }
  list(meta = list(n_models = nrow(models)), data = models, monitoring = monitoring)
}

#* Model-run lineage log (requires ECONKIT_RUN_LOG on the server)
#* @param model_id Optional model id.
#* @param since Optional date (YYYY-MM-DD); runs at or after it.
#* @serializer unboxedJSON list(na = "null")
#* @get /v1/runs
function(res, model_id = "", since = "") {
  tryCatch({
    log <- Sys.getenv("ECONKIT_RUN_LOG")
    if (!nzchar(log)) stop("Run logging is not enabled on this server (ECONKIT_RUN_LOG).")
    runs <- ek_read_runs(log, model_id = if (nzchar(model_id)) model_id,
                         since = if (nzchar(since)) ek_api_date(since, "since"))
    runs$timestamp <- format(runs$timestamp, "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
    list(meta = list(n_runs = nrow(runs)), data = runs)
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

#* Descriptive Treasury yield-curve factors and Nelson-Siegel fit
#* @param source Data source (`sample` or `fred`).
#* @param start Optional first observation date (YYYY-MM-DD).
#* @param end Optional last observation date (YYYY-MM-DD).
#* @param vintage Optional ALFRED as-of date (YYYY-MM-DD).
#* @param lambda Positive Nelson-Siegel decay parameter.
#* @serializer unboxedJSON list(na = "null")
#* @get /v1/rates/curve
function(req, res, source = "sample", start = "", end = "", vintage = "",
         lambda = "0.6") {
  tryCatch({
    source <- ek_api_string(source, "source", "sample")
    start <- if (nzchar(start)) ek_api_date(start, "start") else NULL
    end <- if (nzchar(end)) ek_api_date(end, "end") else NULL
    vintage <- if (nzchar(vintage)) ek_api_date(vintage, "vintage") else NULL
    lambda <- ek_api_number(lambda, "lambda", 0.01, 5, 0.6)
    series <- ek_get_series(
      c("DGS3MO", "DGS2", "DGS5", "DGS10", "DGS30"),
      source = source, start = start, end = end, vintage = vintage
    )
    curve <- ek_yield_curve_fit(series, lambda = lambda)
    run_id <- ek_api_record(
      "yield_curve_nelson_siegel", series, curve$factors,
      list(lambda = lambda, tenors = curve$tenors,
           vintage = if (is.null(vintage)) NULL else format(vintage)),
      req, "GET /v1/rates/curve",
      metrics = c(rmse = mean(curve$factors$rmse), n_obs = nrow(curve$factors))
    )
    list(
      meta = list(model = "ek_yield_curve_fit", model_version = curve$model_version,
                  source = curve$source, synthetic = curve$synthetic,
                  data_vintage = curve$data_vintage, as_of = max(curve$factors$date),
                  lambda = curve$lambda, run_id = run_id),
      factors = curve$factors,
      fitted = curve$fitted
    )
  }, error = function(error) ek_api_error(res, error))
}

#* Project Treasury yield curves under macro scenarios
#* @param req JSON body: optional `source`, `vintage`, `ar`, `curve_lambda`,
#*   `scenarios` and BVAR options as for `/v1/scenarios/build`. BVAR `vars`
#*   must include `UNRATE` and `NFCI`.
#* @parser json list(simplifyVector = FALSE)
#* @serializer unboxedJSON list(na = "null")
#* @post /v1/rates/scenarios
function(req, res) {
  tryCatch({
    own <- c("source", "vintage", "ar", "curve_lambda")
    body <- ek_api_body(req, c(own, "scenarios", "vars", "p", "lambda", "h", "draws",
                               "level", "seed", "version", "path"))
    source <- ek_api_string(body$source, "source", "sample")
    vintage <- if (is.null(body$vintage)) NULL else ek_api_date(body$vintage, "vintage")
    curve_lambda <- ek_api_number(body$curve_lambda, "curve_lambda", 0.01, 5, 0.6)
    ar <- ek_api_flag(body$ar, "ar", TRUE)
    drivers <- c("UNRATE", "NFCI")
    if (!is.null(body$scenarios) && is.null(body$vars)) {
      body$vars <- list("FEDFUNDS", "UNRATE", "NFCI", "PCEPILFE", "INDPRO")
    }
    curve_data <- ek_get_series(
      c("DGS3MO", "DGS2", "DGS5", "DGS10", "DGS30"),
      source = source, vintage = vintage
    )
    macro <- ek_get_series(drivers, source = source, vintage = vintage)
    fit <- ek_yield_curve_satellite_fit(curve_data, macro, ar = ar, lambda = curve_lambda)
    scenario <- ek_api_credit_scenarios(body, own)
    paths <- scenario$library$paths
    missing <- setdiff(fit$drivers, names(paths))
    if (length(missing)) {
      stop("Scenario paths must include curve drivers: ", paste(missing, collapse = ", "),
           ".", call. = FALSE)
    }
    projection <- ek_yield_curve_project(fit, scenario$library)
    factors <- projection$factors
    yields <- projection$yields
    factors$date <- format(factors$date)
    yields$date <- format(yields$date)
    r_squared <- mean(vapply(fit$models, `[[`, numeric(1), "r_squared"))
    rmse <- mean(vapply(fit$models, `[[`, numeric(1), "sigma"))
    run_id <- ek_api_record(
      "yield_curve_satellite",
      list(curves = curve_data, macro = macro, scenario_inputs = scenario$inputs),
      projection$yields,
      list(source = source, vintage = if (is.null(vintage)) NULL else format(vintage),
           drivers = fit$drivers, ar = ar, curve_lambda = curve_lambda,
           scenario_ids = unique(paths$scenario_id), version = paths$version[1]),
      req, "POST /v1/rates/scenarios", metrics = c(r_squared = r_squared, rmse = rmse)
    )
    list(
      meta = list(model = "ek_yield_curve_satellite_fit",
                  model_version = fit$model_version, source = source,
                  synthetic = fit$synthetic || scenario$library$synthetic,
                  data_vintage = fit$data_vintage, scenario_version = paths$version[1],
                  scenarios = I(unique(paths$scenario_id)), drivers = I(fit$drivers),
                  tenors = fit$tenors, lambda = fit$lambda, ar = fit$ar,
                  r_squared = r_squared, rmse = rmse, run_id = run_id),
      factors = factors,
      yields = yields
    )
  }, error = function(error) ek_api_error(res, error))
}

#* Illustrative scenario NII and economic-value sensitivity for a sample bank
#* @param source Curve and macro data source (default "sample").
#* @param vintage Optional data vintage (YYYY-MM-DD).
#* @param horizon_months Number of projected months to analyze (1 to 60).
#* @param reference_scenario Scenario used to anchor rate changes.
#* @serializer unboxedJSON list(na = "null")
#* @get /v1/banking/alm
function(req, res, source = "sample", vintage = "", horizon_months = "12",
         reference_scenario = "base") {
  tryCatch({
    source <- ek_api_string(source, "source", "sample")
    vintage <- if (nzchar(vintage)) ek_api_date(vintage, "vintage") else NULL
    horizon_months <- ek_api_number(horizon_months, "horizon_months", 1, 60,
                                    integer = TRUE)
    reference_scenario <- ek_api_string(reference_scenario, "reference_scenario", "base")
    curve_data <- ek_get_series(
      c("DGS3MO", "DGS2", "DGS5", "DGS10", "DGS30"),
      source = source, vintage = vintage
    )
    macro <- ek_get_series(c("UNRATE", "NFCI"), source = source, vintage = vintage)
    fit <- ek_yield_curve_satellite_fit(curve_data, macro)
    scenarios <- ek_scenario_library()
    projection <- ek_yield_curve_project(fit, scenarios)
    result <- ek_alm_scenario_analysis(
      projection, reference_scenario = reference_scenario,
      horizon_months = horizon_months
    )
    run_id <- ek_api_record(
      "banking_alm_scenario_analysis",
      list(curves = curve_data, macro = macro, scenario_inputs = scenarios$paths),
      result$summary,
      list(source = source, vintage = vintage, horizon_months = horizon_months,
           reference_scenario = reference_scenario),
      req, "GET /v1/banking/alm",
      metrics = c(
        max_abs_nii_change = max(abs(result$summary$nii_change)),
        max_abs_eve_change = max(abs(result$summary$terminal_eve_change))
      )
    )
    ek_api_alm_response(result, run_id)
  }, error = function(error) ek_api_error(res, error))
}

#* Scenario NII and economic-value sensitivity for a bank balance sheet
#* @param req JSON body: optional `portfolio`, `source`, `vintage`, `horizon_months`,
#*   `reference_scenario`, `ar`, `curve_lambda`, `scenarios` and BVAR options as
#*   for `/v1/scenarios/build`. Portfolio rows require `position`, `side`, `balance`,
#*   `rate`, `tenor`, `repricing`, `duration`, `reset_months` and `deposit_beta`.
#* @parser json list(simplifyVector = FALSE)
#* @serializer unboxedJSON list(na = "null")
#* @post /v1/banking/alm
function(req, res) {
  tryCatch({
    own <- c("portfolio", "source", "vintage", "horizon_months",
             "reference_scenario", "ar", "curve_lambda")
    body <- ek_api_body(
      req, c(own, "scenarios", "vars", "p", "lambda", "h", "draws", "level",
             "seed", "version", "path")
    )
    source <- ek_api_string(body$source, "source", "sample")
    vintage <- if (is.null(body$vintage)) NULL else ek_api_date(body$vintage, "vintage")
    horizon_months <- ek_api_number(body$horizon_months, "horizon_months", 1, 60,
                                    12L, integer = TRUE)
    reference_scenario <- ek_api_string(body$reference_scenario, "reference_scenario", "base")
    curve_lambda <- ek_api_number(body$curve_lambda, "curve_lambda", 0.01, 5, 0.6)
    ar <- ek_api_flag(body$ar, "ar", TRUE)
    portfolio <- if (is.null(body$portfolio)) {
      NULL
    } else {
      ek_api_alm_portfolio(body$portfolio)
    }
    if (!is.null(body$scenarios) && is.null(body$vars)) {
      body$vars <- list("FEDFUNDS", "UNRATE", "NFCI", "PCEPILFE", "INDPRO")
    }
    curve_data <- ek_get_series(
      c("DGS3MO", "DGS2", "DGS5", "DGS10", "DGS30"),
      source = source, vintage = vintage
    )
    macro <- ek_get_series(c("UNRATE", "NFCI"), source = source, vintage = vintage)
    fit <- ek_yield_curve_satellite_fit(
      curve_data, macro, ar = ar, lambda = curve_lambda
    )
    scenario <- ek_api_credit_scenarios(body, own)
    missing <- setdiff(fit$drivers, names(scenario$library$paths))
    if (length(missing)) {
      stop("Scenario paths must include curve drivers: ", paste(missing, collapse = ", "),
           ".", call. = FALSE)
    }
    projection <- ek_yield_curve_project(fit, scenario$library)
    result <- ek_alm_scenario_analysis(
      projection, portfolio = portfolio, reference_scenario = reference_scenario,
      horizon_months = horizon_months
    )
    run_id <- ek_api_record(
      "banking_alm_scenario_analysis",
      list(curves = curve_data, macro = macro, scenario_inputs = scenario$inputs),
      result$summary,
      list(source = source, vintage = if (is.null(vintage)) NULL else format(vintage),
           curve_lambda = curve_lambda, ar = ar, horizon_months = horizon_months,
           reference_scenario = reference_scenario,
           portfolio_source = result$portfolio_source),
      req, "POST /v1/banking/alm",
      metrics = c(
        max_abs_nii_change = max(abs(result$summary$nii_change)),
        max_abs_eve_change = max(abs(result$summary$terminal_eve_change))
      )
    )
    ek_api_alm_response(result, run_id)
  }, error = function(error) ek_api_error(res, error))
}

#* Illustrative credit stress for a sample banking portfolio
#* @get /v1/banking/credit-stress
function() {
  ek_credit_scenario_stress()
}

#* Segment default-hazard sensitivities calibrated on the synthetic sample history
#* @param drivers Comma-separated macro drivers (default UNRATE).
#* @serializer unboxedJSON list(na = "null")
#* @get /v1/banking/pd-sensitivity
function(res, drivers = "UNRATE") {
  tryCatch({
    sens <- ek_credit_pd_sensitivity(drivers = ek_api_ids(drivers))
    sens$fit$start <- format(sens$fit$start)
    sens$fit$end <- format(sens$fit$end)
    list(meta = list(model = "ek_credit_pd_sensitivity", drivers = I(sens$drivers),
                     scale = "log annual default hazard", synthetic = sens$synthetic),
         coefficients = sens$coefficients, fit = sens$fit)
  }, error = function(error) ek_api_error(res, error))
}

#* Apply macro scenarios to a supplied or sample credit portfolio
#* @param req JSON body: optional `portfolio` array of segments, `reference_scenario`
#*   (default "base") and `scenarios`, BVAR condition sets as for /v1/scenarios/build
#*   (with the same model options; `vars` must include UNRATE). Without `scenarios`,
#*   the sample scenario library is used.
#* @parser json list(simplifyVector = FALSE)
#* @post /v1/banking/credit-stress
function(req, res) {
  tryCatch({
    body <- ek_api_body(req, c("portfolio", "reference_scenario", "scenarios", "vars",
                               "source", "vintage", "p", "lambda", "h", "draws", "level",
                               "seed", "version", "path"))
    portfolio <- if (is.null(body$portfolio)) {
      NULL
    } else {
      ek_api_portfolio(body$portfolio, c("ead", "pd_12m", "lgd", "unemployment_beta"))
    }
    reference <- ek_api_string(body$reference_scenario, "reference_scenario", "base")
    source <- ek_api_credit_scenarios(body, c("portfolio", "reference_scenario"))
    scenarios <- source$library
    inputs <- c(list(portfolio = portfolio), source$inputs)
    result <- ek_credit_scenario_stress(scenarios, portfolio, reference)
    result$scenario_version <- unique(scenarios$paths$version)
    run_id <- ek_api_record(
      "credit_scenario_stress", inputs, result$summary,
      list(reference_scenario = reference, scenario_version = result$scenario_version,
           scenarios = body$scenarios, portfolio_source = result$portfolio_source),
      req, "POST /v1/banking/credit-stress",
      metrics = c(loss_rate = max(result$summary$loss_rate))
    )
    if (!is.na(run_id)) result$run_id <- run_id
    result
  }, error = function(error) ek_api_error(res, error))
}

#* Lifetime expected credit losses (CECL or IFRS 9) for the synthetic sample portfolio
#* @param standard "cecl" (default) or "ifrs9".
#* @serializer unboxedJSON list(na = "null", null = "null")
#* @get /v1/banking/ecl
function(res, standard = "cecl") {
  tryCatch({
    if (!standard %in% c("cecl", "ifrs9")) stop("`standard` must be \"cecl\" or \"ifrs9\".")
    ek_api_ecl_response(ek_credit_lifetime_ecl(standard = standard), FALSE, NA_character_)
  }, error = function(error) ek_api_error(res, error))
}

#* Lifetime expected credit losses for a supplied or sample portfolio
#* @param req JSON body: optional `portfolio` array (credit-stress fields plus `term_months`,
#*   `rate` and optional `amortization` and `stage`), `weights` (object of scenario
#*   probabilities), `standard`, `anchor`, `reversion_months`, `discount`,
#*   `reference_scenario`, `include_paths` and `scenarios` with BVAR model options as for
#*   /v1/scenarios/build.
#* @parser json list(simplifyVector = FALSE)
#* @serializer unboxedJSON list(na = "null", null = "null")
#* @post /v1/banking/ecl
function(req, res) {
  tryCatch({
    own <- c("portfolio", "weights", "standard", "anchor", "reversion_months", "discount",
             "reference_scenario", "include_paths")
    body <- ek_api_body(req, c(own, "scenarios", "vars", "source", "vintage", "p", "lambda",
                               "h", "draws", "level", "seed", "version", "path"))
    portfolio <- if (is.null(body$portfolio)) {
      NULL
    } else {
      ek_api_portfolio(body$portfolio,
                       c("ead", "pd_12m", "lgd", "unemployment_beta", "term_months", "rate"),
                       optional = c("amortization", "stage"), text = "amortization")
    }
    weights <- if (is.null(body$weights)) NULL else ek_api_weight_object(body$weights)
    standard <- ek_api_string(body$standard, "standard", "cecl")
    if (!standard %in% c("cecl", "ifrs9")) stop("`standard` must be \"cecl\" or \"ifrs9\".")
    settings <- list(
      reference_scenario = ek_api_string(body$reference_scenario, "reference_scenario", "base"),
      anchor = ek_api_number(body$anchor, "anchor", 0, 50),
      reversion_months = ek_api_number(body$reversion_months, "reversion_months", 0, 120, 12L,
                                       integer = TRUE),
      discount = ek_api_flag(body$discount, "discount", NULL)
    )
    source <- ek_api_credit_scenarios(body, own)
    result <- ek_credit_lifetime_ecl(
      source$library, portfolio, weights = weights, standard = standard,
      reference_scenario = settings$reference_scenario, anchor = settings$anchor,
      reversion_months = settings$reversion_months, discount = settings$discount
    )
    rates <- if (is.null(result$weighted)) result$summary$ecl_rate else result$weighted$ecl_rate
    run_id <- ek_api_record(
      "credit_lifetime_ecl", c(list(portfolio = portfolio), source$inputs), result$summary,
      c(settings, list(standard = standard, discount = result$discount,
                       weights = body$weights, scenarios = body$scenarios,
                       portfolio_source = result$portfolio_source)),
      req, "POST /v1/banking/ecl", metrics = c(ecl_rate = max(rates))
    )
    ek_api_ecl_response(result, ek_api_flag(body$include_paths, "include_paths", FALSE), run_id)
  }, error = function(error) ek_api_error(res, error))
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

ek_api_bvar_params <- function(spec, body) {
  list(vars = names(spec$data)[-1], source = spec$source,
       vintage = if (is.null(spec$vintage)) NULL else format(spec$vintage), p = spec$p,
       lambda = spec$lambda, h = spec$h, draws = spec$draws, level = spec$level,
       seed = spec$seed, conditions = body$conditions)
}

# Records lineage when the server sets ECONKIT_RUN_LOG; the user comes from an
# optional X-User header set by an authenticating gateway.
ek_api_record <- function(model_id, inputs, outputs, params, req, note, metrics = NULL) {
  log <- Sys.getenv("ECONKIT_RUN_LOG")
  if (!nzchar(log)) return(NA_character_)
  ek_record_run(model_id, inputs = inputs, outputs = outputs, params = params,
                metrics = metrics, user = req$HTTP_X_USER, note = note, log = log)$run_id
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

ek_api_build_scenarios <- function(body) {
  scenarios <- body$scenarios
  if (!is.list(scenarios) || !length(scenarios) || is.null(names(scenarios)) ||
        length(scenarios) > 10) {
    stop("`scenarios` must be an object of 1 to 10 named condition objects.")
  }
  spec <- ek_api_bvar_spec(body)
  fit <- ek_bvar(spec$data, p = spec$p, lambda = spec$lambda)
  forecasts <- lapply(scenarios, function(conditions) {
    ek_bvar_forecast(fit, h = spec$h, conditions = ek_api_conditions(conditions),
                     draws = spec$draws, level = spec$level, seed = spec$seed)
  })
  library <- ek_scenario_from_forecast(
    forecasts, version = ek_api_string(body$version, "version", "bvar-v1"),
    path = ek_api_string(body$path, "path", "central")
  )
  list(spec = spec, fit = fit, library = library)
}

# Scenario library for banking routes: BVAR-built when `scenarios` is given,
# otherwise the sample library (model options are then rejected).
ek_api_credit_scenarios <- function(body, own_fields) {
  if (is.null(body$scenarios)) {
    if (length(setdiff(names(body), own_fields))) stop("Model options require `scenarios`.")
    library <- ek_scenario_library()
    return(list(library = library, inputs = list(paths = library$paths)))
  }
  built <- ek_api_build_scenarios(body)
  list(library = built$library, inputs = list(data = built$spec$data))
}

ek_api_weight_object <- function(weights) {
  if (!is.list(weights) || !length(weights) || is.null(names(weights))) {
    stop("`weights` must be an object of scenario probabilities.")
  }
  vapply(weights, function(w) {
    if (!is.numeric(w) || length(w) != 1L) stop("Each weight must be a number.")
    as.numeric(w)
  }, numeric(1))
}

ek_api_flag <- function(x, name, default) {
  if (is.null(x)) return(default)
  if (!is.logical(x) || length(x) != 1L || is.na(x)) stop("`", name, "` must be true or false.")
  x
}

ek_api_ecl_response <- function(result, include_paths, run_id) {
  dates <- function(df) {
    if (is.null(df)) return(NULL)
    for (col in names(df)) if (inherits(df[[col]], "Date")) df[[col]] <- format(df[[col]])
    df
  }
  list(
    meta = list(
      model = "ek_credit_lifetime_ecl", model_version = result$model_version,
      standard = result$standard, discount = result$discount,
      reference_scenario = result$reference_scenario, anchor = result$anchor,
      reversion_months = result$reversion_months,
      scenario_version = I(unique(result$summary$version)),
      scenario_synthetic = result$scenario_synthetic,
      portfolio_source = result$portfolio_source,
      weights = if (is.null(result$weights)) NULL else as.list(result$weights),
      run_id = run_id
    ),
    summary = dates(result$summary),
    segments = dates(result$segments),
    weighted = result$weighted,
    weighted_segments = result$weighted_segments,
    paths = if (include_paths) dates(result$paths)
  )
}

ek_api_alm_response <- function(result, run_id) {
  dates <- function(df) {
    for (col in names(df)) {
      if (inherits(df[[col]], "Date")) df[[col]] <- format(df[[col]])
    }
    df
  }
  list(
    meta = list(
      model = "ek_alm_scenario_analysis", model_version = result$model_version,
      reference_scenario = result$reference_scenario,
      horizon_months = result$horizon_months,
      scenario_version = I(unique(result$summary$version)),
      scenario_synthetic = result$scenario_synthetic,
      portfolio_source = result$portfolio_source,
      run_id = run_id
    ),
    summary = dates(result$summary),
    paths = dates(result$paths),
    positions = dates(result$positions)
  )
}

# Converts a JSON array of segment objects to a data frame. `segment` and any
# `text` columns are strings; other columns are numbers. Optional columns
# must appear in every object or in none.
ek_api_portfolio <- function(portfolio, numeric, optional = NULL, text = NULL) {
  required <- c("segment", numeric)
  invalid <- function() {
    stop("`portfolio` must be an array with one complete segment object per element.")
  }
  if (!is.list(portfolio) || !length(portfolio) || !is.null(names(portfolio))) invalid()
  fields <- lapply(portfolio, function(row) {
    if (!is.list(row) || is.null(names(row)) || anyDuplicated(names(row))) invalid()
    names(row)
  })
  present <- intersect(optional, fields[[1]])
  columns <- c(required, present)
  if (!all(vapply(fields, setequal, logical(1), columns))) invalid()
  out <- lapply(columns, function(column) {
    values <- lapply(portfolio, `[[`, column)
    is_text <- column == "segment" || column %in% text
    ok <- vapply(values, function(v) {
      length(v) == 1L && (if (is_text) is.character(v) else is.numeric(v))
    }, logical(1))
    if (!all(ok)) {
      stop("Portfolio field `", column, "` must be ", if (is_text) "a string" else "a number",
           " in every segment.")
    }
    if (is_text) as.character(unlist(values)) else as.numeric(unlist(values))
  })
  names(out) <- columns
  as.data.frame(out, stringsAsFactors = FALSE)
}

ek_api_alm_portfolio <- function(portfolio) {
  required <- c("position", "side", "balance", "rate", "tenor", "repricing",
                "duration", "reset_months", "deposit_beta")
  invalid <- function() {
    stop("`portfolio` must be an array with one complete ALM position per element.")
  }
  if (!is.list(portfolio) || !length(portfolio) || !is.null(names(portfolio))) invalid()
  fields <- lapply(portfolio, function(row) {
    if (!is.list(row) || is.null(names(row)) || anyDuplicated(names(row))) invalid()
    names(row)
  })
  if (!all(vapply(fields, setequal, logical(1), required))) invalid()
  text <- c("position", "side", "tenor", "repricing")
  out <- lapply(required, function(column) {
    values <- lapply(portfolio, `[[`, column)
    is_text <- column %in% text
    valid <- vapply(values, function(value) {
      length(value) == 1L && (if (is_text) is.character(value) else is.numeric(value))
    }, logical(1))
    if (!all(valid)) {
      stop("Portfolio field `", column, "` must be ",
           if (is_text) "a string" else "a number", " in every position.")
    }
    if (is_text) as.character(unlist(values)) else as.numeric(unlist(values))
  })
  names(out) <- required
  as.data.frame(out, stringsAsFactors = FALSE)
}
