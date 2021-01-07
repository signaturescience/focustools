## code to prepare internal datasets goes here
library(readr)

locations <- read_csv("data-raw/locations.csv")

usethis::use_data(locations,
                  internal = TRUE,
                  overwrite = TRUE)
