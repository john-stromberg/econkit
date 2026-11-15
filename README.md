# econkit

<!-- badges: start -->
[![R-CMD-check](https://github.com/john-stromberg/econkit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/john-stromberg/econkit/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**econkit** is an R toolkit for applied economic analysis in **asset management** and **banking**. It turns macro and market data into outputs a business can make decisions from: recession risk, regime signals, stress scenarios, rate and credit risk, and reports. Model governance follows the expectations of SR 11-7.

> **Status:** The foundation, macro core, positioning service, core economics service (data, analysis, API and governance) and scenario library are complete; rates, credit, monitoring and strategic modules are in progress or planned. All bundled data is synthetic. See the [roadmap](#roadmap).

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
ek_credit_lifetime_ecl(scenarios,                  # probability-weighted lifetime ECL
                       weights = c(base = 0.6, adverse = 0.3, severe = 0.1))$weighted
curve_fit <- ek_yield_curve_satellite_fit()
curves <- ek_yield_curve_project(curve_fit, scenarios)
ek_alm_scenario_analysis(curves)$summary            # sample NII/EVE sensitivity
ek_ftp_analysis(curves)$summary                     # matched-tenor FTP contribution
ek_curve_pricing_inputs(curves)$dcf                 # DCF rates and WACC inputs

ek_apply_transforms(ek_get_series(c("PAYEMS", "UNRATE")))  # registry transforms
ek_taylor_rule()                                   # policy-rate benchmark
ek_nowcast()                                       # factor-bridge nowcast, ragged edge
fit <- ek_bvar(p = 2)                              # Minnesota-prior BVAR
base <- ek_bvar_forecast(fit, h = 12, seed = 1)
hike <- ek_bvar_forecast(fit, h = 12, seed = 1,
                         conditions = list(FEDFUNDS = rep(6, 12)))
bvar_lib <- ek_scenario_from_forecast(list(base = base, hike = hike))
ek_credit_scenario_stress(bvar_lib)$summary        # credit stress on model scenarios
sat <- ek_satellite_fit(ek_get_series(c("DGS10", "UNRATE", "NFCI")),
                        "DGS10", c("UNRATE", "NFCI"))
ek_satellite_project(sat, bvar_lib)                # satellite variable along paths

ek_render_macro_dashboard("macro-dashboard.html")  # Quarto HTML report
```

### API

The optional `{plumber}` API serves the econkit workflows. Install
the current checkout and `{plumber}`, then start it from the package root:

```r
remotes::install_local(".")
install.packages("plumber")
plumber::pr("inst/plumber/api.R") |>
  plumber::pr_run(host = "127.0.0.1", port = 8000)
```

Available endpoints are `/health`, `/v1/catalog`, `/v1/sources`, `/v1/series`,
`/v1/series/export`, `/v1/calendar`, `/v1/nowcast`, `/v1/signals`,
`/v1/positioning/playbook`,
`/v1/saa/assumptions`, `/v1/scenarios`, and compute-only `POST` routes
`/v1/saa/portfolio` and `/v1/saa/stress`, each with a JSON `weights` object,
plus `/v1/forecast`, `/v1/scenarios/build`, `/v1/rates/curve` and
`POST /v1/rates/scenarios`. Banking routes are
`/v1/banking/credit-stress` and `/v1/banking/ecl` (each `GET` and `POST`) and
`/v1/banking/pd-sensitivity` and `/v1/banking/alm` (each `GET` and `POST`).
`/v1/rates/ftp` and `/v1/rates/pricing-inputs` also provide sample `GET` and
custom-assumption `POST` workflows.
Governance routes are
`/v1/models` and `/v1/runs`.
`GET /v1/series?ids=UNRATE,PAYEMS&frequency=quarter&vintage=2020-06-30`
returns `{meta, data}`: provenance metadata and long-format observations.
`GET /v1/series/export` takes the same parameters plus `format=parquet|csv`
and returns the file, with provenance in `X-Econkit-*` response headers
(`Data-MD5`, `Source`, `Synthetic`, `Vintage`, `Vintage-Method`), for BI tools
that read Parquet or CSV over HTTP. `GET /v1/calendar?from=&to=&ids=` lists
estimated releases, and `GET /v1/nowcast?vintage=` runs the sample nowcast.
`source=fred` works only when the server has `FRED_API_KEY` set.
`GET /v1/rates/curve?source=sample&lambda=0.6` fits a descriptive
Nelson-Siegel curve to five Treasury maturities and returns level, slope,
curvature, fit RMSE and fitted yields. Use `source=fred` for FRED/ALFRED data.
The bundled curve is synthetic; FRED constant-maturity yields are par yields,
not zero-coupon discount rates, so these fitted values are not for pricing.
`POST /v1/rates/scenarios` fits macro satellites to the curve's level, slope
and curvature, then projects factors and maturity yields along the sample
scenario paths or BVAR-built scenarios. The BVAR `vars` must include `UNRATE`
and `NFCI`; the request defaults to the sample library. The projections are
illustrative scenario-implied par yields, not valuation curves.

`GET /v1/banking/alm` runs a synthetic bank balance sheet against the sample
curve scenarios. `POST /v1/banking/alm` accepts the same portfolio schema and
scenario-building options; each position supplies `position`, `side`, `balance`,
`rate`, `tenor`, `repricing` (`fixed`, `floating` or `deposit`), `duration`,
`reset_months` and `deposit_beta`. NII changes apply floating-rate pass-through
at reset dates and monthly deposit beta to three-month yields. EVE changes use
a linear modified-duration approximation. Results compare each path with
`reference_scenario` (default `base`) on a static balance sheet; they are not
regulatory NII/EVE measures. The yield projections are Treasury par yields,
not discount curves, and the sample portfolio and assumptions are synthetic.

`GET /v1/rates/ftp` returns matched-tenor Treasury benchmarks plus synthetic
funding spreads and position contributions. `POST /v1/rates/ftp` accepts a
portfolio with `position`, `side`, `balance`, customer `rate`, curve `tenor`
and `funding_spread_bps`; provided funding spreads should reflect the bank's
own liquidity and funding costs.

`GET /v1/rates/pricing-inputs` returns illustrative scenario-based DCF and DCM
rate inputs. `POST /v1/rates/pricing-inputs` accepts `dcf` assumptions
(`project`, `tenor`, `beta`, `equity_risk_premium_bps`, `debt_spread_bps`,
`equity_weight`, `debt_weight`, `tax_rate`) and `dcm` assumptions (`issue`,
`tenor`, `spread_bps`). The outputs include cost of equity, cost of debt, WACC
and indicative DCM yields. These are inputs only: the sample Treasury par
yields are not risk-free zero rates, and the API does not calculate enterprise
values or bond prices. Replace sample premia and spreads with dated, governed
market inputs before professional use.

`POST /v1/forecast` fits a BVAR and returns forecast bands; `POST
/v1/scenarios/build` turns named condition sets into a macro scenario library.
Both accept `vars`, `source`, `vintage`, `p`, `lambda`, `h` (up to 60),
`draws` (up to 2000), `level` and `seed` (default 1, so identical requests
return identical results); conditions are arrays by variable, with `null`
leaving a period free:

```json
{
  "vars": ["FEDFUNDS", "UNRATE", "INDPRO"], "h": 6, "version": "rates-v1",
  "scenarios": {"base": {}, "hike": {"FEDFUNDS": [6, 6, 6, null, null, null]}}
}
```

`GET /v1/banking/credit-stress` runs a synthetic sample portfolio, while
`POST /v1/banking/credit-stress` accepts a JSON `portfolio` array and applies
the same synthetic scenarios without persisting the portfolio. Add
`scenarios` (condition sets, with the `/v1/scenarios/build` model options) to
stress against BVAR conditional scenarios instead; `vars` must include
`UNRATE` and the scenarios must include the `reference_scenario` (default
`"base"`). Add `"include_components": true` to a POST request or
`?include_components=true` to the GET request to apply the bundled synthetic
macro-linked LGD/EAD satellites. Component projections are illustrative and
require institution-specific loss and exposure histories plus independent
validation before professional use. `GET /v1/banking/pd-sensitivity?drivers=UNRATE` returns segment
sensitivities calibrated on the synthetic default-rate history:

```json
{
  "portfolio": [
    {"segment": "CRE", "ead": 1000000, "pd_12m": 0.025,
     "lgd": 0.4, "unemployment_beta": 0.35}
  ],
  "vars": ["FEDFUNDS", "UNRATE", "INDPRO"], "h": 12,
  "scenarios": {"base": {}, "downturn": {"UNRATE": [10.5, 11, 11.5, 12]}}
}
```

The sample credit stress adjusts each segment's default hazard using monthly
unemployment differences from the base path over the scenario horizon only.
Without component satellites, exposure and LGD remain constant. Credit stress
does not model scenario probabilities, lifetime terms, discounting or accounting
stages; use the lifetime ECL below for those. Inputs and sensitivities are illustrative; results are not
CECL/IFRS 9 allowances, regulatory stress results or calibrated forecasts.
Amounts must use common units; no currency conversion is performed. Output
as-of dates and vintages describe the scenarios, not the supplied portfolio.
Only send non-sensitive demonstration portfolios unless the API runs behind
authentication and TLS.
The POST routes do not persist data (run lineage stores only hashes and
parameters) or recommend allocations. Scenario paths
and asset shocks are synthetic illustrations, not forecasts or calibrated
market estimates. Sample endpoints identify synthetic data.

In R, calibrate sensitivities from default-rate and LGD/EAD histories and stress
against BVAR conditional scenarios:

```r
sens <- ek_credit_pd_sensitivity()   # or your own history and macro data
beta <- sens$coefficients[sens$coefficients$term == "UNRATE", ]
portfolio <- data.frame(segment = beta$segment, ead = c(200e6, 250e6, 100e6),
                        pd_12m = c(0.02, 0.025, 0.04), lgd = c(0.45, 0.4, 0.6),
                        unemployment_beta = beta$estimate)
fit <- ek_bvar(ek_get_series(c("FEDFUNDS", "UNRATE", "INDPRO")), p = 2)
scenarios <- ek_scenario_from_forecast(list(
  base = ek_bvar_forecast(fit, h = 12, seed = 1),
  hike = ek_bvar_forecast(fit, h = 12, conditions = list(FEDFUNDS = rep(7, 6)), seed = 1)
))
components <- ek_credit_lgd_ead_satellite_fit()
ek_credit_scenario_stress(scenarios, portfolio,
                          component_fit = components)$summary
```

Conditioning on the policy rate alone gives reduced-form co-movements; in the
synthetic sample, higher rates coincide with lower unemployment and lower
losses. Condition `UNRATE` directly, for example
`list(UNRATE = c(10.5, 11, 11.5, 12))`, for an explicit labor-market downturn.

`ek_credit_lifetime_ecl()` extends the same hazard model to contractual lives.
Within the scenario horizon the unemployment gap scales each segment's hazard;
afterwards the gap reverts linearly to zero over `reversion_months` (default
12), a straight-line reversion to the long-run PD. Exposure is bullet or linear
amortization, and losses are discounted at the loan `rate` (default on for IFRS
9, off for CECL). Under `standard = "ifrs9"`, stage 1 loans use a 12-month
horizon, stage 2 the full term, and stage 3 is treated as defaulted (`ead * lgd`).
`weights` give scenario probabilities and add `weighted` and `weighted_segments`
results. Gaps are measured against `reference_scenario`, or against a fixed
long-run unemployment rate via `anchor`:

```r
ecl_portfolio <- data.frame(
  segment = c("cre", "cards"), ead = c(5e6, 1e6), pd_12m = c(0.02, 0.05),
  lgd = c(0.4, 0.8), unemployment_beta = c(0.3, 0.4), term_months = c(60, 36),
  rate = c(0.06, 0.18), amortization = c("bullet", "linear"), stage = c(1, 2)
)
ecl <- ek_credit_lifetime_ecl(ek_scenario_library(), ecl_portfolio, standard = "ifrs9",
                              weights = c(base = 0.6, adverse = 0.3, severe = 0.1))
ecl$weighted_segments
ek_credit_lifetime_ecl(bvar_lib, ecl_portfolio, anchor = 5.5)$summary  # anchored gaps
```

`GET /v1/banking/ecl?standard=cecl|ifrs9` runs the sample portfolio and
`POST /v1/banking/ecl` accepts the same `portfolio` fields plus `term_months`,
`rate`, optional `amortization` and `stage`, and `weights`, `standard`,
`anchor`, `reversion_months`, `discount`, `reference_scenario`,
`include_paths` (monthly marginal PDs, survival and losses) and `scenarios`
with the BVAR options. Weights must cover exactly the scenarios used:

```json
{
  "standard": "ifrs9", "anchor": 5.5,
  "weights": {"base": 0.7, "downturn": 0.3},
  "vars": ["FEDFUNDS", "UNRATE"], "h": 12,
  "scenarios": {"base": {}, "downturn": {"UNRATE": [10.5, 11, 11.5, 12]}}
}
```

This is a simplified, illustrative ECL: it does not model prepayment, undrawn
commitments, significant-increase-in-credit-risk tests, collateral or
qualitative overlays, and its results are not accounting allowances.

Set `ECONKIT_API_KEYS` (comma-separated) to require an `X-API-Key` header on
every route except `/health` and the API docs; `/health` reports whether keys
are enforced. Keys are read on each request, so they can be rotated by
updating the environment. The API serves plain HTTP: terminate TLS at a
gateway or reverse proxy and keep keys out of source control.

`GET /v1/models?id=` returns the model inventory and monitoring thresholds
(404 for an unknown id). Set `ECONKIT_RUN_LOG` to a JSONL file path to record
lineage for `/v1/nowcast`, `/v1/forecast`, `/v1/scenarios/build`, `GET
/v1/rates/curve`, `POST /v1/rates/scenarios`, `POST /v1/banking/credit-stress`,
`POST /v1/banking/ecl`, `/v1/banking/alm`, `/v1/rates/ftp` and
`/v1/rates/pricing-inputs`: their
responses then include a `run_id` (in `meta` except for credit stress), the optional `X-User` header is stored
with the run, and `GET /v1/runs?model_id=&since=` lists recorded runs. Without
`ECONKIT_RUN_LOG`, nothing is recorded and `/v1/runs` returns 400. The log is
append-only from a single API process; use one log per process.

PostgreSQL is optional for shared
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
| `"sample"` (default) | Bundled `macro_monthly` | **Synthetic**, calibrated to US dynamics; includes multi-tenor Treasury yield curve points for curve-fit demonstrations. For demos, tests and CI only. Rebuild it with `data-raw/macro_monthly.R`. |
| Asset returns | Bundled `asset_returns` | **Synthetic and deliberately regime-conditioned**, for workflow tests only; not market history, performance evidence or suitable for investment decisions. Rebuild with `data-raw/asset_returns.R`. |
| Credit default rates | Bundled `credit_default_rates` | **Synthetic** quarterly default rates for three segments, driven by the sample unemployment rate with known sensitivities. For calibration demos and tests only; not bank or industry loss data. Rebuild with `data-raw/credit_default_rates.R`. |
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
| Indicators | `ek_apply_transforms()` (registry transforms), `ek_taylor_rule()` (policy-rate benchmark with inertia and lower bound), `ek_surprise_index()` (standardized, decaying data-surprise index) |
| Nowcasting and forecasting | `ek_nowcast()` (principal-component factor bridge for the current quarter, ragged edge), `ek_bvar()` and `ek_bvar_forecast()` (Minnesota-prior BVAR with bands and conditional scenarios) |
| Multi-asset positioning | `ek_positioning_playbook()` (signals at month t, return observations at t+1), `ek_positioning_tilts()` (neutral, bounded heuristic), `ek_positioning_gap()` |
| Strategic asset allocation | `ek_saa_assumptions()` (historical return/risk/covariance inputs), `ek_saa_portfolio()` (policy-mix summary; no optimizer) |
| Rates and curves | `ek_yield_curve_fit()` (fixed-decay Nelson-Siegel level, slope, curvature and fitted yields), `ek_yield_curve_satellite_fit()` and `ek_yield_curve_project()` (macro-scenario projections; descriptive par yields), `ek_alm_scenario_analysis()` (static-balance-sheet NII and duration-based EVE changes with deposit betas), `ek_ftp_analysis()` (matched-tenor FTP contribution), `ek_curve_pricing_inputs()` (scenario-based DCF/WACC and DCM yield inputs, not valuations) |
| Scenario analysis | `ek_scenario_library()` (versioned macro paths, with optional asset shocks), `ek_scenario_from_forecast()` (libraries from BVAR forecasts), `ek_satellite_fit()` and `ek_satellite_project()` (macro-to-business-variable models), `ek_validate_scenario_library()`, `ek_saa_scenario_stress()` |
| Banking credit stress | `ek_credit_scenario_stress()` (scenario-specific segment default probabilities and expected credit losses), `ek_credit_pd_sensitivity()` (segment unemployment sensitivities calibrated on default-rate history), `ek_credit_lifetime_ecl()` (scenario-weighted CECL/IFRS 9-style lifetime ECL with reversion, amortization, discounting and stages) |
| Model governance | `ek_model_registry()` (inventory, risk tiers, owners, review dates, monitoring thresholds), `ek_record_run()` and `ek_read_runs()` (JSONL run lineage with input/output hashes and provenance), `ek_monitor_metrics()` (threshold breaches and drift) |
| API | Plumber endpoints for series, Parquet/CSV export, sources, release calendar, nowcast, BVAR forecasts and scenario building, yield curve fitting/projections, FTP, DCF/DCM rate inputs, signals, positioning, SAA, credit stress, lifetime ECL, ALM NII/EVE, model inventory and run lineage, with optional API keys |
| PM signal service | `ek_build_signals()` (delayed labels), local RDS storage via `ek_store_signals()` / `ek_read_signals()`, or shared DBI/PostgreSQL storage via `ek_init_signal_store_db()`, `ek_store_signals_db()` and `ek_read_signals_db()` |
| Reporting | `ek_render_macro_dashboard()` and the template in `inst/templates/macro-dashboard.qmd` |
| Pipelines | `pipelines/macro/_targets.R` (`targets::tar_make()`) |

The positioning playbook is descriptive, not a return forecast. Tilt scores are shrunk, volatility-scaled historical differences subject to active-weight and one-way-turnover caps; they are not an optimizer or a recommendation to trade. SAA return and risk inputs are historical estimates, not forward-looking capital-market assumptions. Review sample size and eligibility alongside outputs. The sample returns are simulated with regime-dependent means to exercise the workflow and cannot support investment conclusions.

Both signal stores preserve the immutable `(as_of, signal, model_version)` key and as-of reads. The RDS implementation remains a single-writer local option; PostgreSQL is the shared-service option. `data_vintage` records the latest observation date. Signals are built from current data; `ek_get_series(vintage = )` now provides ALFRED vintages for FRED, but signal backtests do not yet rebuild each snapshot from its vintage. The `override`, `override_reason` and `user` fields are reserved for downstream approval workflows.

Model cards (purpose, assumptions, limitations and monitoring) are in the help pages for each model, for example `?ek_recession_model`, `?ek_markov_switching`, `?ek_positioning_tilts`, `?ek_nowcast`, `?ek_bvar`, `?ek_satellite_fit`, `?ek_yield_curve_fit`, `?ek_yield_curve_satellite_fit`, `?ek_alm_scenario_analysis`, `?ek_ftp_analysis`, `?ek_curve_pricing_inputs`, `?ek_credit_pd_sensitivity` and `?ek_credit_lifetime_ecl`. BVAR conditional forecasts use reduced-form shocks, so they are consistent scenarios rather than identified policy experiments, and the BVAR does not impose a lower bound on rates unless the policy rate is conditioned. Scenario libraries built from forecasts are macro-only (no asset shocks), so they feed credit stress and satellites but not SAA stress.

### Model governance

The model inventory in [`inst/config/models.yml`](inst/config/models.yml)
records each model's entry point, version, risk tier, validation status, owner,
intended use, last review date and monitoring thresholds. Add or override
models (by `id`) with YAML files in the same layout, listed in
`options(econkit.model_registry = )` or `ECONKIT_MODEL_REGISTRY`.

```r
ek_model_registry()                    # inventory
ek_model_registry(monitoring = TRUE)   # metric thresholds
log <- "governance/runs.jsonl"         # or options(econkit.run_log = ) / ECONKIT_RUN_LOG
data <- ek_get_series(c("FEDFUNDS", "UNRATE"))
fc <- ek_bvar_forecast(ek_bvar(data, p = 2), h = 6, seed = 1)
ek_record_run("bvar_minnesota", inputs = data, outputs = fc$summary,
              params = list(p = 2, h = 6), metrics = c(rmse = 0.4), log = log)
ek_monitor_metrics(ek_read_runs(log))  # breach, drift or ok per model and metric
```

Each run record stores a run id, UTC timestamp, model and package versions,
user, MD5 hashes of inputs and outputs, a data-provenance summary (source,
vintage, synthetic flag), parameters, metrics and a note. Hashes ignore
retrieval times, so re-fetching identical data gives the same hash; they
identify changes within one installation and are not comparable across R
versions. The log is append-only JSONL for a single writer; it is an audit aid,
not a substitute for an institution's model risk inventory or approval process.
`ek_monitor_metrics()` flags values outside the registry thresholds and
z-score drift against each metric's recent history (default: previous 12
values, at least 3).

## Development

```r
roxygen2::roxygenise()          # regenerate docs and NAMESPACE
testthat::test_local()          # unit and validation tests
rcmdcheck::rcmdcheck(args = "--no-manual")
lintr::lint_package()
```

CI (GitHub Actions) runs `R CMD check` on Ubuntu and Windows, lintr, and a coverage gate of at least 80%. A monthly workflow renders the dashboard, using FRED data when the `FRED_API_KEY` repository secret is set.

## Roadmap

One roadmap covers asset management (AM), commercial banking (CB) and
investment banking (IB). Workflows are grouped by the shared engines they use,
so each engine serves all three business lines.

| Shared engine | Asset management | Commercial banking | Investment banking |
|---|---|---|---|
| Regime and playbook | Tactical allocation, factor and sector rotation | Through-the-cycle risk appetite | Sector screening, fee forecasting |
| Signal store | Rebalance triggers, risk overlays | Early-warning triggers | DCM/ECM market-window signal |
| Scenario library | SAA/CMA scenarios, stress overlays | CECL/IFRS 9, CCAR/DFAST/ICAAP, FP&A | Scenario valuations, leveraged finance stress |
| Rates and curves | Duration and curve positioning | ALM, NII/EVE, FTP, deposit betas | DCF discount rates, DCM pricing |
| Credit and stress | Credit allocation, hedge triggers | PD/LGD/EAD, migration, concentration, economic capital, RAROC, CRE | Covenant tests, securitization collateral, counterparty risk |

**Real-data readiness** shows what each phase needs beyond the bundled
synthetic data: *Public* runs on FRED/ALFRED and other free public sources;
*Vendor* needs licensed or internal data such as curves, spreads, returns,
loan tapes or deal data.

| Phase | Scope | Builds on | Status | Real-data readiness |
|---|---|---|---|---|
| 0–1. Foundation and macro core | Package, CI, data layer, recession model and backtest, Sahm rule, regimes, output gap, dashboard | — | Done | Public |
| 1b. Positioning and signal store | Regime playbook, constrained tilts, portfolio gap, versioned signal snapshots | Macro core | Done | Public; Vendor for asset returns |
| C. Core economics service | **A** series registry, vintages, provenance, adapters, release calendar, Parquet/CSV export; **B** transforms, Taylor rule, surprise index, nowcast, BVAR with conditional forecasts, scenario builder, satellites; **C** API contract and keys; **D** model inventory, run lineage, monitoring | — | Done | Public; vendor adapters are interfaces only |
| 2. Service platform | Done: Plumber API, optional PostgreSQL store, Parquet/CSV export, API keys. Next: Excel/CSV feed, async jobs and webhooks, alerting on monitoring breaches, maker/checker overrides on top of run lineage | Core C, D | Partial | Not data-dependent |
| 3. Scenario library | Sample and BVAR-built versioned scenarios, vintages, SAA stress | Core B | Done | Public; Vendor for asset shocks |
| 4. Rates and curves | Synthetic multi-tenor data, Nelson-Siegel fit, macro satellites and scenario-implied curves; static-balance-sheet NII/EVE with deposit betas; matched-tenor FTP; DCF/WACC and DCM rate inputs | BVAR, satellites, scenarios | Done (illustrative) | Public FRED/ALFRED par yields; vendor for internal funding curves, credit spreads and balance sheets |
| 5. Credit and stress | Done: PD sensitivity calibration, scenario credit stress, scenario-weighted CECL/IFRS 9-style lifetime ECL, synthetic LGD/EAD satellites integrated into scenario stress. Next: rating migration, concentration, economic capital and RWA, ICAAP-style capital, RAROC, CRE stress, leveraged-finance covenant stress, securitization collateral | Satellites, scenarios | Partial | Vendor (loan tapes, default and recovery history) |
| 6. Monitoring and signals | Early-warning watchlist, CRE monitor, DCM/ECM market-window signal, tail-risk overlays | Signal store, monitoring | Planned | Public and Vendor |
| 7. Strategic and commercial | Done: SAA. Next: factor exposures, VaR/ES with backtests, scenario P&L, factor rotation, manager due diligence, fee and pipeline forecasts, loan pricing, client reporting | All engines | Partial | Vendor (returns, holdings, deal pipeline) |
| 8. Forecast quality | Vintage-based forecast evaluation, nowcast and BVAR backtests | Vintages, BVAR, nowcast | Planned | Public (ALFRED) |
| 9. Delivery | Shiny app, pkgdown site, Python client, Quarto committee packs (IC, ALCO, underwriting) | API | Planned | Not data-dependent |
| Controls | Done: versioning and lineage, model inventory, monitoring. Next: maker/checker approvals, information barriers for deal-tagged runs, research compliance (disclaimers, approval, archiving) | Core D | Partial | Not data-dependent |

## License

MIT
