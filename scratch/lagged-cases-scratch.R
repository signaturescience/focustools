## this script explores using lagged incident cases to predict incident deaths
## linear regression time series

library(tidyverse)
library(lubridate)
library(fable)
library(tsibble)
# library(fable.prophet) # install from source worked for me
theme_set(theme_classic())

# Set up national data ----------------------------------------------------

# Get national data for cases and deaths (incident and cumulative)

source(here::here("utils/get_data.R"))
usac <-  get_cases(source="nyt",  granularity = "national")
usad <- get_deaths(source="nyt",  granularity = "national")
usa <-  inner_join(usac, usad, by = c("epiyear", "epiweek"))

# Add sunday date, and yearweek based on that sunday, convert to tsibble
# see package?tsibble for more
usa <-
  usa %>%
  # get the date that starts the MMWRweek
  mutate(day=MMWRweek::MMWRweek2Date(epiyear, epiweek), .after="epiweek") %>%
  # Remove the incomplete week
  filter(week(day)!=week(today())) %>%
  # convert represent as yearweek (see package?tsibble)
  mutate(yweek=yearweek(day), .after="day") %>%
  # convert to tsibble
  as_tsibble(index=yweek)


# when later forecasting or limiting to training/testing, how many periods?
horizon <- 4

## evaluate how well the model fits
## create separate training / test sets

## test on n week horizon
usa_test <-
  usa %>%
  slice(tail(row_number(), horizon))

## train on everything *except* n week horizon
usa <-
  usa %>%
  slice(-tail(row_number(), horizon))

fit <-
  usa %>%
  model(
    # Linear trend
    # https://otexts.com/fpp3/useful-predictors.html
    linear = TSLM(ideaths ~ trend()),
    linear_caselag1 = TSLM(ideaths ~ lag(icases, 1)),
    linear_caselag2 = TSLM(ideaths ~ lag(icases, 2)),
    linear_caselag3 = TSLM(ideaths ~ lag(icases, 3)),
    linear_caselag4 = TSLM(ideaths ~ lag(icases, 4))
  )


## ok now we need to get the cases to use in future data
## lets get the "best" model from fable-accuracy-scratch.R
source(here::here("utils/get_data.R"))

best_guess <-
  metrics %>%
  filter(.outcome == "icases") %>%
  filter(RMSE == min(RMSE)) %>%
  gather(week, icases, `2020 W44`:`2020 W47`) %>%
  pull(icases)


case_future <-
  new_data(usa, 4) %>%
  mutate(icases = best_guess)

myforecast <-
  fit %>%
  forecast(case_future)

myforecast %>%
  autoplot(usa)
