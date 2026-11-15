library(testthat)
library(econkit)
utils::data("macro_monthly", package = "econkit")
utils::data("asset_returns", package = "econkit")

test_check("econkit")
