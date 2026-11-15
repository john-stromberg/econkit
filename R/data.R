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
#' @format A data frame with 672 rows (January 1970 to December 2025) and 12 columns:
#' \describe{
#'   \item{date}{First day of the month.}
#'   \item{USREC}{Recession indicator (1 = recession).}
#'   \item{T10Y3M}{10-year minus 3-month Treasury spread, percentage points.}
#'   \item{DGS10}{10-year Treasury yield, percent.}
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
