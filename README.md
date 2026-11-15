# econkit

<!-- badges: start -->
[![R-CMD-check](https://github.com/john-stromberg/econkit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/john-stromberg/econkit/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**econkit** is an R toolkit for applied economic analysis in **asset management** and **banking**. It turns macro and market data into outputs a business can make decisions from: recession risk, regime signals, stress scenarios, rate and credit risk, and reports. Model governance follows the expectations of SR 11-7.

> **Status:** Phase 0 (foundation), Phase 1 (macro core) and the first Phase 1b positioning-service slice are complete. See the [roadmap](#roadmap).

## Installation

```r
# install.packages("remotes")
remotes::install_github("john-stromberg/econkit")
```

## Quick start

```r
library(econkit)

d <- ek_get_series()                         # bundled synthetic sample, works offline
m <- ek_recession_model(d, horizon = 12)     # yield-curve probit
tail(predict(m))

bt <- ek_backtest_recession(d, horizon = 12, label_lag = 6)  # leakage-free backtest
attr(bt, "metrics")                          # AUC, Brier score, log loss

ek_sahm_rule(d$UNRATE)                       # real-time recession signal
ek_growth_inflation_regime(ek_transform(d$INDPRO, "yoy"),
                           ek_transform(d$CPIAUCSL, "yoy"))
ek_markov_switching(ek_transform(d$INDPRO, "log_diff")[-1])
ek_output_gap(d$INDPRO)                      # Hamilton (2018) filter

pb <- ek_positioning_playbook()               # regime-conditional, next-month returns
b <- c(global_equity = 0.4, government_bonds = 0.25,
       investment_grade_credit = 0.1, high_yield_credit = 0.05,
       commodities = 0.08, gold = 0.05, us_dollar = 0.02, cash = 0.05)
t <- ek_positioning_tilts(pb, "Recovery", b)  # transparent, constrained heuristic
ek_positioning_gap(t, b)                      # compare a current book with the tilts

signals <- ek_build_signals()                  # point-in-time macro snapshot
ek_store_signals(signals, "output/signals.rds")
ek_read_signals("output/signals.rds", as_of = "2025-01-31")

assumptions <- ek_saa_assumptions()             # historical SAA input estimates
weights <- stats::setNames(
  rep(1 / nrow(assumptions$assumptions), nrow(assumptions$assumptions)),
  assumptions$assumptions$asset_class
)
ek_saa_portfolio(assumptions, weights)           # summarize a policy mix

scenarios <- ek_scenario_library()                 # illustrative 12-month paths
ek_saa_scenario_stress(scenarios, b)               # shocked policy returns
ek_credit_scenario_stress(scenarios)$summary       # sample banking credit stress

ek_render_macro_dashboard("macro-dashboard.html")  # Quarto HTML report
```

### Read-only API

The optional `{plumber}` API serves the bundled sample workflows. Install
the current checkout and `{plumber}`, then start it from the package root:

```r
remotes::install_local(".")
install.packages("plumber")
plumber::pr("inst/plumber/api.R") |>
  plumber::pr_run(host = "127.0.0.1", port = 8000)
```

Available endpoints are `/health`, `/v1/catalog`, `/v1/series`, `/v1/signals`,
`/v1/positioning/playbook`,
`/v1/saa/assumptions`, `/v1/scenarios`, and compute-only `POST` routes
`/v1/saa/portfolio` and `/v1/saa/stress`, each with a JSON `weights` object.
`GET /v1/series?ids=UNRATE,PAYEMS&frequency=quarter&vintage=2020-06-30`
returns `{meta, data}`: provenance metadata and long-format observations.
`source=fred` works only when the server has `FRED_API_KEY` set.
`GET /v1/banking/credit-stress` runs a synthetic sample portfolio, while
`POST /v1/banking/credit-stress` accepts a JSON `portfolio` array and applies
the same synthetic scenarios without persisting the portfolio:

```json
{
  "portfolio": [
    {"segment": "CRE", "ead": 1000000, "pd_12m": 0.025,
     "lgd": 0.4, "unemployment_beta": 0.35}
  ]
}
```

The sample credit stress adjusts each segment's default hazard using monthly
unemployment differences from the base path. It assumes constant exposure and
LGD and does not model scenario probabilities, lifetime terms, discounting or
accounting stages. Inputs and sensitivities are illustrative; results are not
CECL/IFRS 9 allowances, regulatory stress results or calibrated forecasts.
Amounts must use common units; no currency conversion is performed. Output
as-of dates and vintages describe the scenarios, not the supplied portfolio.
Only send non-sensitive demonstration portfolios to the unauthenticated API.
The POST routes do not persist data or recommend allocations. Scenario paths
and asset shocks are synthetic illustrations, not forecasts or calibrated
market estimates. Sample endpoints identify synthetic data. This
prototype is stateless and has no authentication; keep it on localhost or
behind an appropriately secured gateway. PostgreSQL is optional for shared
signal storage. Without database settings, `/v1/signals` uses sample data. If
any required PostgreSQL setting is present, the API connects to the configured
store and reports connection or schema errors rather than silently falling
back to sample data.

Install `{DBI}` and `{RPostgres}`, then set `ECONKIT_PGHOST`,
`ECONKIT_PGDATABASE` and `ECONKIT_PGUSER`; optionally set `ECONKIT_PGPORT`,
`ECONKIT_PGPASSWORD` and `ECONKIT_PGSSLMODE` in the process environment or
`.Renviron`. Never commit credentials. Initialize and publish snapshots from a
controlled R job. Run the API with a database role granted `SELECT` only; the
HTTP API intentionally has no signal-write endpoint.

```r
conn <- ek_connect_postgres()
ek_init_signal_store_db(conn)
ek_store_signals_db(ek_build_signals(), conn)
ek_read_signals_db(conn)
DBI::dbDisconnect(conn)
```

### Data

| Source | How | Notes |
|---|---|---|
| `"sample"` (default) | Bundled `macro_monthly` | **Synthetic**, calibrated to US dynamics. For demos, tests and CI only. Rebuild it with `data-raw/macro_monthly.R`. |
| Asset returns | Bundled `asset_returns` | **Synthetic and deliberately regime-conditioned**, for workflow tests only; not market history, performance evidence or suitable for investment decisions. Rebuild with `data-raw/asset_returns.R`. |
| `"fred"` | `ek_get_series(source = "fred")` | Requires `fredr` and `FRED_API_KEY` in `~/.Renviron` (never commit it). Downloads are cached in `ek_cache_dir()` and aggregated to monthly or quarterly frequency. |
| Vendor or internal data | `ek_register_source()` | Adapter interface for licensed or in-house feeds. `ek_csv_fetcher()` serves long-format extracts (`source_id`, `date`, `value`, optional `realtime_start` for vintages). List adapters with `ek_sources()`. |

The series registry is in [`inst/config/series.yml`](inst/config/series.yml).
Each entry records the provider id, native frequency, aggregation rule
(`mean`, `last`, `sum` or `max`), seasonal adjustment, a conservative
publication lag, licensing notes and whether the series is in the sample data.
`ek_get_series()` without `ids` returns the sample series; credit spreads, bank
lending and delinquency series, GDP, house prices and VIX are available from
FRED. Check provider terms before redistributing licensed series.

```r
q <- ek_get_series(c("UNRATE", "PAYEMS"), frequency = "quarter", vintage = "2020-06-30")
ek_provenance(q)   # source, vintage method, retrieval time, series metadata
ek_series_long(q)  # one row per date and series, for BI tools and feeds
ek_export_series(q, "exports/macro.parquet")  # Parquet + provenance sidecar (needs arrow)
ek_release_calendar(from = Sys.Date(), to = Sys.Date() + 30)  # expected data arrivals
```

With `source = "fred"`, `vintage` queries ALFRED, so values are those
published on that date, including later revisions. The synthetic sample has no
vintages: it approximates point-in-time data by masking each value until the
period end plus the series' release lag, which does not model revisions.
`ek_release_calendar()` applies the same lag rule, so it is an estimate rather
than an official release schedule.

Vendor series are added to the registry with a YAML file in the same layout as
`series.yml`, listed in `options(econkit.registry = )` or the
`ECONKIT_REGISTRY` environment variable, with `source` set to the adapter name:

```r
options(econkit.registry = "config/vendor-series.yml")
ek_register_source("vendor", ek_csv_fetcher("extracts/vendor.csv"), supports_vintage = TRUE)
ek_get_series("VENDOR_HY_OAS", source = "vendor", vintage = "2024-03-05")
```

Exports are written atomically, in long format, and their provenance includes a
`data_md5` checksum of the rows that is the same for Parquet and CSV.

## What's included (v0.1)

| Area | Functions |
|---|---|
| Data layer | `ek_get_series()`, `ek_series_catalog()`, `ek_provenance()`, `ek_series_long()`, `ek_export_series()`, `ek_release_calendar()`, `ek_register_source()`, `ek_sources()`, `ek_csv_fetcher()`, `ek_align_frequency()`, `ek_floor_date()`, `ek_transform()`, `ek_lag()`, `ek_cache_dir()` |
| Recession risk | `ek_recession_model()`, `predict()`, `ek_backtest_recession()`, `ek_classification_metrics()`, `ek_sahm_rule()` |
| Cycle and regimes | `ek_output_gap()` (Hamilton or HP), `ek_growth_inflation_regime()`, `ek_markov_switching()` |
| Multi-asset positioning | `ek_positioning_playbook()` (signals at month t, return observations at t+1), `ek_positioning_tilts()` (neutral, bounded heuristic), `ek_positioning_gap()` |
| Strategic asset allocation | `ek_saa_assumptions()` (historical return/risk/covariance inputs), `ek_saa_portfolio()` (policy-mix summary; no optimizer) |
| Scenario analysis | `ek_scenario_library()` (versioned macro paths and asset shocks), `ek_validate_scenario_library()`, `ek_saa_scenario_stress()` |
| Banking credit stress | `ek_credit_scenario_stress()` (scenario-specific segment default probabilities and expected credit losses) |
| API | Read-only/compute-only Plumber endpoints for sample or database-backed signals, positioning playbook and SAA |
| PM signal service | `ek_build_signals()` (delayed labels), local RDS storage via `ek_store_signals()` / `ek_read_signals()`, or shared DBI/PostgreSQL storage via `ek_init_signal_store_db()`, `ek_store_signals_db()` and `ek_read_signals_db()` |
| Reporting | `ek_render_macro_dashboard()` and the template in `inst/templates/macro-dashboard.qmd` |
| Pipelines | `pipelines/macro/_targets.R` (`targets::tar_make()`) |

The positioning playbook is descriptive, not a return forecast. Tilt scores are shrunk, volatility-scaled historical differences subject to active-weight and one-way-turnover caps; they are not an optimizer or a recommendation to trade. SAA return and risk inputs are historical estimates, not forward-looking capital-market assumptions. Review sample size and eligibility alongside outputs. The sample returns are simulated with regime-dependent means to exercise the workflow and cannot support investment conclusions.

Both signal stores preserve the immutable `(as_of, signal, model_version)` key and as-of reads. The RDS implementation remains a single-writer local option; PostgreSQL is the shared-service option. `data_vintage` records the latest observation date. Signals are built from current data; `ek_get_series(vintage = )` now provides ALFRED vintages for FRED, but signal backtests do not yet rebuild each snapshot from its vintage. The `override`, `override_reason` and `user` fields are reserved for downstream approval workflows.

Model cards (purpose, assumptions, limitations and monitoring) are in the help pages for each model, for example `?ek_recession_model`, `?ek_markov_switching` and `?ek_positioning_tilts`.

## Development

```r
roxygen2::roxygenise()          # regenerate docs and NAMESPACE
testthat::test_local()          # unit and validation tests
rcmdcheck::rcmdcheck(args = "--no-manual")
lintr::lint_package()
```

CI (GitHub Actions) runs `R CMD check` on Ubuntu and Windows, lintr, and a coverage gate of at least 80%. A monthly workflow renders the dashboard, using FRED data when the `FRED_API_KEY` repository secret is set.

## Roadmap

| Phase | Scope | Status |
|---|---|---|
| 0. Foundation | Package skeleton, CI, data layer, sample data | Done |
| 1. Macro core | Recession model and backtest, Sahm rule, regimes, output gap, dashboard | Done |
| 1b. PM positioning and signal store | Regime playbook, constrained tilts, portfolio gap, versioned signal snapshots and targets outputs | Done (initial local-store slice) |
| 2. Service and SAA | Plumber API, sample-backed SAA inputs and optional PostgreSQL signal store | Initial local implementation |
| 3. Scenario analysis | Versioned macro paths, SAA stress and illustrative banking credit stress with API routes; follow with CECL/stress integration | Sample-backed initial slice (banking work paused for core enhancements) |
| Core A. Data foundation | Series registry, point-in-time vintages, provenance, source adapters, Parquet/CSV export, release calendar | Done |
| Core B–D. Analysis core, service contract, governance | Indicators, nowcast, BVAR and scenario builder; export/calendar/forecast API routes and API keys; model registry, run lineage and monitoring | In progress |
| 4. Asset management portfolio analytics | Factor exposures, VaR/ES with backtests, scenario P&L | Planned |
| 5. Commercial banking portfolio analytics | Concentration, credit portfolio model and economic capital, RWA, migration and vintages, RAROC/FTP pricing, NII/EVE, CRE stress | Planned |
| 6. Forecasting and nowcasting | BVAR, conditional forecasts, DFM nowcast | Planned |
| 7. Delivery | Shiny app, pkgdown site | Planned |

## License

MIT
