# Builds the bundled `credit_loss_components` sample dataset.
#
# The LGD and EAD index paths are SYNTHETIC illustrations driven by
# unemployment and financial conditions. They are not bank, regulatory or
# industry loss data. Run `Rscript data-raw/credit_loss_components.R` from
# the package root.

library(econkit)
set.seed(20261117)

macro <- ek_get_series(c("UNRATE", "NFCI"))
anchor_unrate <- mean(macro$UNRATE)
segments <- data.frame(
  segment = c("commercial_real_estate", "commercial_industrial", "consumer"),
  lgd = c(0.40, 0.45, 0.60),
  lgd_unrate_beta = c(0.08, 0.06, 0.04),
  ead_unrate_beta = c(0.025, 0.02, 0.035),
  ead_nfci_beta = c(0.06, 0.045, 0.035),
  stringsAsFactors = FALSE
)

ar_noise <- function(n, sd) {
  as.numeric(stats::filter(stats::rnorm(n, sd = sd), 0.55, method = "recursive"))
}

credit_loss_components <- do.call(rbind, lapply(seq_len(nrow(segments)), function(i) {
  lgd_logit <- qlogis(segments$lgd[i]) +
    segments$lgd_unrate_beta[i] * (macro$UNRATE - anchor_unrate) +
    0.08 * macro$NFCI + ar_noise(nrow(macro), 0.07)
  log_ead_index <- segments$ead_unrate_beta[i] * (macro$UNRATE - anchor_unrate) +
    segments$ead_nfci_beta[i] * macro$NFCI + ar_noise(nrow(macro), 0.04)
  data.frame(
    date = macro$date,
    segment = segments$segment[i],
    lgd = pmin(0.95, pmax(0.05, plogis(lgd_logit))),
    ead_index = exp(log_ead_index),
    stringsAsFactors = FALSE
  )
}))
rownames(credit_loss_components) <- NULL
attr(credit_loss_components, "synthetic") <- TRUE

save(credit_loss_components, file = "data/credit_loss_components.rda", compress = "xz")
