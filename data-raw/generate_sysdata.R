## code to prepare internal datasets goes here
library(dplyr)
library(readr)
library(tidyr)

locations <- read_csv("data-raw/locations.csv")

## exclude DC county code because DC will be a state/territory
locations <-
  locations %>%
  dplyr::filter(location != "11001")


# quantiles needed
q <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
# Figure out what the interval you need to get those quantiles
qi <-
  tibble(lower=q[q<.5], upper=rev(q[q>.5])) %>%
  mutate(interval=round((upper-lower)*100))
qi
# The quidk (say: "quiddick") tibble: QUantile, Interval, Direction, Key
quidk <-
  qi %>%
  gather(direction, quantile, lower, upper) %>%
  mutate(key=paste0(interval, "%_", direction)) %>%
  arrange(quantile) %>%
  select(quantile, interval, direction, key)
quidk

usethis::use_data(locations, quidk,
                  internal = TRUE,
                  overwrite = TRUE)
