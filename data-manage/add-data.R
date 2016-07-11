### add RData files to data/ directory
# run this script before building package

library(gogamer)
saikoyo <- read_sgf("inst/extdata/saikoyo.sgf")
devtools::use_data(saikoyo)


