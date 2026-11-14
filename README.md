# econkit

<!-- badges: start -->
[![R-CMD-check](https://github.com/john-stromberg/econkit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/john-stromberg/econkit/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**econkit** is an R toolkit for applied economic analysis in **asset management** and **banking**. It turns macro and market data into outputs a business can make decisions from: recession risk, regime signals, stress scenarios, rate and credit risk, and reports. Model governance follows the expectations of SR 11-7.

> **Status:** Phase 0 (foundation) and Phase 1 (macro core) are complete. See the [roadmap](#roadmap).

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

ek_render_macro_dashboard("macro-dashboard.html")  # Quarto HTML report
```

### Data

| Source | How | Notes |
|---|---|---|
| `"sample"` (default) | Bundled `macro_monthly` | **Synthetic**, calibrated to US dynamics. For demos, tests and CI only. Rebuild it with `data-raw/macro_monthly.R`. |
| `"fred"` | `ek_get_series(source = "fred")` | Requires `fredr` and `FRED_API_KEY` in `~/.Renviron` (never commit it). Downloads are cached in `ek_cache_dir()` and aggregated to monthly. |

The series catalog is in [`inst/config/series.yml`](inst/config/series.yml).

## What's included (v0.1)

| Area | Functions |
|---|---|
| Data layer | `ek_get_series()`, `ek_series_catalog()`, `ek_align_frequency()`, `ek_floor_date()`, `ek_transform()`, `ek_lag()`, `ek_cache_dir()` |
| Recession risk | `ek_recession_model()`, `predict()`, `ek_backtest_recession()`, `ek_classification_metrics()`, `ek_sahm_rule()` |
| Cycle and regimes | `ek_output_gap()` (Hamilton or HP), `ek_growth_inflation_regime()`, `ek_markov_switching()` |
| Reporting | `ek_render_macro_dashboard()` and the template in `inst/templates/macro-dashboard.qmd` |
| Pipelines | `pipelines/macro/_targets.R` (`targets::tar_make()`) |

Model cards (purpose, assumptions, limitations and monitoring) are in the help pages for each model, for example `?ek_recession_model` and `?ek_markov_switching`.

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
| 2. Rates | Nelson-Siegel-Svensson curve fitting, PCA, duration/DV01/key-rate duration, IRRBB shocks | Planned |
| 3. Banking stress and credit | Scenario engine, PD satellite models, ECL/CECL, stress test report | Planned |
| 4. Asset management portfolio analytics | Factor exposures, VaR/ES with backtests, scenario P&L | Planned |
| 5. Commercial banking portfolio analytics | Concentration, credit portfolio model and economic capital, RWA, migration and vintages, RAROC/FTP pricing, NII/EVE, CRE stress | Planned |
| 6. Forecasting and nowcasting | BVAR, conditional forecasts, DFM nowcast | Planned |
| 7. Delivery | Shiny app, pkgdown site | Planned |

## License

MIT
