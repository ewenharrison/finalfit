library(testthat)
library(openssl)

# The CRAN OSX Mavericks server has a firewall that blocks IPv6
if(isTRUE(grepl("Mavericks", sessionInfo()$running)))
  options(ipv4_only = TRUE)

test_check("openssl")
