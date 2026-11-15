# Builds the bundled `asset_returns` sample dataset.
#
# The returns are SYNTHETIC and deliberately regime-conditioned to exercise
# the positioning tools. They are not historical market data or evidence of
# strategy performance. Run `Rscript data-raw/asset_returns.R` from the root.

library(econkit)
utils::data("macro_monthly", package = "econkit")
set.seed(20261115)

growth <- ek_growth_inflation_regime(
  ek_transform(macro_monthly$INDPRO, "yoy"),
  ek_transform(macro_monthly$CPIAUCSL, "yoy")
)

assets <- c(
  "global_equity", "government_bonds", "investment_grade_credit",
  "high_yield_credit", "commodities", "gold", "us_dollar", "cash"
)
regimes <- c("Reflation", "Recovery", "Overheat", "Stagflation")

# Illustrative arithmetic monthly returns, not fitted historical estimates.
conditional_mean <- rbind(
  Reflation = c(0.010, 0.006, 0.004, 0.007, 0.006, 0.004, -0.001, 0.002),
  Recovery = c(0.013, 0.002, 0.006, 0.010, 0.008, 0.001, -0.002, 0.002),
  Overheat = c(0.006, -0.003, 0.004, 0.006, 0.013, 0.002, -0.004, 0.002),
  Stagflation = c(-0.006, 0.003, -0.002, -0.010, 0.006, 0.008, 0.006, 0.002)
)
vol <- c(0.04, 0.02, 0.012, 0.03, 0.045, 0.04, 0.025, 0.001)
correlation <- matrix(0.2, length(assets), length(assets))
diag(correlation) <- 1
dimnames(conditional_mean) <- list(regimes, assets)

z <- matrix(stats::rnorm(nrow(macro_monthly) * length(assets)),
            nrow = nrow(macro_monthly), ncol = length(assets)) %*% chol(correlation)
z <- sweep(z, 2, vol, "*")
means <- matrix(0, nrow(macro_monthly), length(assets),
                dimnames = list(NULL, assets))
valid <- !is.na(growth)
means[valid, ] <- conditional_mean[as.character(growth[valid]), , drop = FALSE]
asset_returns <- data.frame(
  date = macro_monthly$date,
  z + means,
  check.names = FALSE
)
names(asset_returns)[-1] <- assets
attr(asset_returns, "synthetic") <- TRUE

save(asset_returns, file = "data/asset_returns.rda", compress = "xz")
