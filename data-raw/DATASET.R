## code to prepare `DATASET` dataset goes here


toxdata <- read.csv("data-raw/ToxCast/Toxdata.csv")

usethis::use_data(toxdata, overwrite = TRUE)
