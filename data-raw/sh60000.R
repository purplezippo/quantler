strategy_sh600000 <- read.csv(
  'data-raw/strategy_sh600000.csv', header = TRUE, stringsAsFactors = FALSE
  )
stockprice_sh600000 <- read.csv(
  'data-raw/sh600000.csv', header = TRUE, stringsAsFactors = FALSE
)
usethis::use_data(strategy_sh600000, overwrite = TRUE)
usethis::use_data(stockprice_sh600000, overwrite = TRUE)
