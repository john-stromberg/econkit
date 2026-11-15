# Builds the bundled `credit_default_rates` sample dataset.
#
# The default rates are SYNTHETIC. Each segment's annual default hazard is
# its long-run level scaled by exp(beta * (unemployment - long-run mean)),
# with persistent noise, so calibration tests can recover the known betas.
# They are not bank, regulatory or industry loss data. Run
# `Rscript data-raw/credit_default_rates.R` from the package root.

library(econkit)
set.seed(20261116)

unemployment <- ek_get_series("UNRATE", frequency = "quarter")
anchor <- mean(unemployment$UNRATE)
segments <- data.frame(
  segment = c("commercial_real_estate", "commercial_industrial", "consumer"),
  pd_long_run = c(0.025, 0.02, 0.04),
  beta = c(0.20, 0.15, 0.25),
  stringsAsFactors = FALSE
)

credit_default_rates <- do.call(rbind, lapply(seq_len(nrow(segments)), function(i) {
  noise <- stats::filter(stats::rnorm(nrow(unemployment), sd = 0.1), 0.5,
                         method = "recursive")
  hazard <- -log1p(-segments$pd_long_run[i]) *
    exp(segments$beta[i] * (unemployment$UNRATE - anchor) + as.numeric(noise))
  data.frame(
    date = unemployment$date,
    segment = segments$segment[i],
    default_rate = round(-expm1(-hazard), 6),
    stringsAsFactors = FALSE
  )
}))
rownames(credit_default_rates) <- NULL
attr(credit_default_rates, "synthetic") <- TRUE

save(credit_default_rates, file = "data/credit_default_rates.rda", compress = "xz")
