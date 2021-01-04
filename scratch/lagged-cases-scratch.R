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
source(here::here("R/utils.R"))
usac <-  get_cases(source="nyt",  granularity = "national")
usad <- get_deaths(source="nyt",  granularity = "national")
usa <-  inner_join(usac, usad, by = c("epiyear", "epiweek"))

# Add sunday date, and yearweek based on that sunday, convert to tsibble
# see package?tsibble for more
usa <- usa %>% make_tsibble(., chop = TRUE)

# when later forecasting or limiting to training/testing, how many periods?
horizon <- 4

# ## evaluate how well the model fits
# ## create separate training / test sets
#
# ## test on n week horizon
# usa_test <-
#   usa %>%
#   slice(tail(row_number(), horizon))
#
# ## train on everything *except* n week horizon
# usa <-
#   usa %>%
#   slice(-tail(row_number(), horizon))

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


# ## ok now we need to get the cases to use in future data
# ## lets get the "best" model from fable-accuracy-scratch.R
# source(here::here("scratch/fable-accuracy-scratch.R"))
#
# best_guess <-
#   metrics %>%
#   filter(.outcome == "icases") %>%
#   filter(RMSE == min(RMSE)) %>%
#   gather(week, icases, `2020 W45`:`2020 W48`) %>%
#   pull(icases)
#
# tail(usa)

fit.icases <-
  usa %>%
  model(arima = ARIMA(icases, stepwise=FALSE, approximation=FALSE))

best_guess <-
  forecast(fit.icases, h=horizon, new_data = NULL) %>%
  pull(.mean)

case_future <-
  new_data(usa, 4) %>%
  mutate(icases = best_guess)

myforecast <-  forecast(fit, h = horizon)

myforecast

myforecast %>%
  autoplot(usa)

# bootstrap a model
boots <-
  fit %>%
  generate(h=horizon, times=1000, bootstrap=TRUE, new_data = case_future)

boots
