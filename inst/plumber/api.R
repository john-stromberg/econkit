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
  empty_null <- function(x) if (!nzchar(x)) NULL else x
  id_list <- empty_null(ids)
  if (!is.null(id_list)) id_list <- trimws(strsplit(id_list, ",", fixed = TRUE)[[1]])
  tryCatch({
    if (!source %in% ek_sources()$name) {
      stop("Unknown `source`. Available: ", paste(ek_sources()$name, collapse = ", "), ".")
    }
    if (!frequency %in% c("month", "quarter")) {
      stop("`frequency` must be \"month\" or \"quarter\".")
    }
    x <- ek_get_series(
      ids = id_list, source = source, start = empty_null(start), end = empty_null(end),
      frequency = frequency, vintage = empty_null(vintage)
    )
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
  }, error = function(error) {
    res$status <- 400L
    list(error = conditionMessage(error))
  })
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
