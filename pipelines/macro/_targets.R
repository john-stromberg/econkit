# Macro pipeline (Phase 1), built with {targets}.
#
# Run from this directory:
#   Rscript -e "targets::tar_make()"
# Use real data by setting ECONKIT_SOURCE=fred and FRED_API_KEY.

library(targets)

tar_option_set(packages = "econkit")

source_name <- Sys.getenv("ECONKIT_SOURCE", "sample")
horizon <- 12L

list(
  tar_target(raw, ek_get_series(source = source_name), cue = tar_cue(mode = "always")),
  tar_target(recession_model, ek_recession_model(raw, horizon = horizon)),
  tar_target(recession_backtest, ek_backtest_recession(raw, horizon = horizon, label_lag = 6)),
  tar_target(
    indicators,
    data.frame(
      date = raw$date,
      recession_prob = predict(recession_model, newdata = raw)$probability,
      sahm = ek_sahm_rule(raw$UNRATE),
      regime = ek_growth_inflation_regime(
        ek_transform(raw$INDPRO, "yoy"),
        ek_transform(raw$CPIAUCSL, "yoy")
      )
    )
  ),
  tar_target(
    indicators_csv,
    {
      path <- "output/macro_indicators.csv"
      dir.create(dirname(path), showWarnings = FALSE)
      utils::write.csv(indicators, path, row.names = FALSE)
      path
    },
    format = "file"
  ),
  tar_target(
    dashboard,
    ek_render_macro_dashboard("output/macro-dashboard.html", source = source_name,
                              horizon = horizon),
    format = "file"
  )
)
