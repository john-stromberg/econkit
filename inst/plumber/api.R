#* @apiTitle econkit API
#* @apiDescription Read-only endpoints for bundled sample-data workflows.
#* @serializer json list(na = "null", auto_unbox = TRUE)

library(econkit)

#* Health check
#* @get /health
function() {
  list(status = "ok", version = as.character(utils::packageVersion("econkit")))
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
