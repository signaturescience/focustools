## code to prepare internal datasets goes here
library(readr)

locations <- read_csv("data-raw/locations.csv")

## exclude DC county code because DC will be a state/territory
locations <-
  locations %>%
  dplyr::filter(location != "11001")

usethis::use_data(locations,
                  internal = TRUE,
                  overwrite = TRUE)
