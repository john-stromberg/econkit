#' Synthetic monthly US-style macro dataset
#'
#' A **synthetic** monthly panel that mimics the FRED series in the econkit
#' catalog. It is simulated from a regime-switching model calibrated to
#' stylised US dynamics: about 12% of months in recession, a term spread that
#' inverts 6-18 months before recessions, counter-cyclical unemployment and
#' financial conditions. Use it for demos, tests and CI. It is not real data.
#' Use `ek_get_series(source = "fred")` for real data. The generator lives in
#' `data-raw/macro_monthly.R`.
#'
#' @format A data frame with 672 rows (January 1970 to December 2025) and 16 columns:
#' \describe{
#'   \item{date}{First day of the month.}
#'   \item{USREC}{Recession indicator (1 = recession).}
#'   \item{T10Y3M}{10-year minus 3-month Treasury spread, percentage points.}
#'   \item{DGS10}{10-year Treasury yield, percent.}
#'   \item{DGS3MO, DGS2, DGS5, DGS30}{3-month, 2-year, 5-year and 30-year
#'   Treasury constant maturity yields, percent; fitted synthetic curve points.}
#'   \item{TB3MS}{3-month Treasury bill rate, percent.}
#'   \item{FEDFUNDS}{Effective federal funds rate, percent.}
#'   \item{UNRATE}{Unemployment rate, percent.}
#'   \item{PAYEMS}{Nonfarm payrolls, thousands.}
#'   \item{INDPRO}{Industrial production index.}
#'   \item{CPIAUCSL}{Consumer price index.}
#'   \item{PCEPILFE}{Core PCE price index.}
#'   \item{NFCI}{Financial conditions index (positive = tight).}
#' }
#' @source Simulated; see `data-raw/macro_monthly.R`.
"macro_monthly"

#' Synthetic monthly asset-class returns
#'
#' Simulated monthly total returns aligned to [macro_monthly]. Returns have
#' deliberately stylised conditional means by growth/inflation regime and
#' correlated noise. This makes the positioning workflow reproducible and
#' testable; these data are not market observations and must not be used for
#' investment decisions or performance claims. Rebuild them with
#' `data-raw/asset_returns.R`.
#'
#' @format A data frame with 672 rows and 9 columns: `date` and eight numeric
#'   return columns (`global_equity`, `government_bonds`,
#'   `investment_grade_credit`, `high_yield_credit`, `commodities`, `gold`,
#'   `us_dollar` and `cash`). Returns are decimal monthly total returns.
#' @source Simulated; see `data-raw/asset_returns.R`.
"asset_returns"

#' Synthetic quarterly default rates by credit segment
#'
#' Simulated annualized default rates for three illustrative segments
#' (`commercial_real_estate`, `commercial_industrial` and `consumer`), aligned
#' to quarterly averages of the synthetic unemployment rate in
#' [macro_monthly]. Each segment's default hazard rises with unemployment
#' relative to its long-run mean (log-hazard sensitivities of 0.20, 0.15 and
#' 0.25 per percentage point) with persistent noise. Use it to demonstrate
#' and test [ek_credit_pd_sensitivity()]; these are not bank, regulatory or
#' industry loss data. Rebuild with `data-raw/credit_default_rates.R`.
#'
#' @format A long data frame with 672 rows and 3 columns:
#' \describe{
#'   \item{date}{First day of the quarter (1970 Q1 to 2025 Q4).}
#'   \item{segment}{Segment name.}
#'   \item{default_rate}{Annualized default rate, decimal.}
#' }
#' @source Simulated; see `data-raw/credit_default_rates.R`.
"credit_default_rates"

#' Synthetic monthly LGD and EAD component history by credit segment
#'
#' Simulated loss-given-default rates and exposure indices for
#' `commercial_real_estate`, `commercial_industrial` and `consumer`, aligned to
#' [macro_monthly]. LGD and EAD respond to unemployment and financial
#' conditions with persistent noise. The EAD index is dimensionless and is
#' used only to estimate relative exposure changes from the latest observation.
#' These are synthetic inputs for workflow demonstrations, not bank,
#' regulatory or industry loss data. Rebuild with
#' `data-raw/credit_loss_components.R`.
#'
#' @format A long data frame with 2016 rows and 4 columns:
#' \describe{
#'   \item{date}{First day of the month.}
#'   \item{segment}{Illustrative credit segment.}
#'   \item{lgd}{Synthetic loss-given-default proportion in `(0, 1)`.}
#'   \item{ead_index}{Positive dimensionless synthetic exposure index.}
#' }
#' @source Simulated; see `data-raw/credit_loss_components.R`.
"credit_loss_components"
