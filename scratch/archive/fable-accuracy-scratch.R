## variation of the fable-scratch script to look at how we can assess accuracy
# https://fable.tidyverts.org/index.html
# https://otexts.com/fpp3/

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

## function to evaluate accuracy
## to add new models include another line in model() in this function
evaluate_accuracy <- function(.data, outcome, horizon = 4) {

  ## test on n week horizon
  test_data <-
    .data %>%
    dplyr::slice(tail(dplyr::row_number(), horizon))

  ## train on everything *except* n week horizon
  train_data <-
    .data %>%
    dplyr::slice(-tail(dplyr::row_number(), horizon))

  .outcome <-
    train_data %>%
    dplyr::pull(outcome)

  fit <-
    train_data %>%
    model(
      # Linear trend
      # https://otexts.com/fpp3/useful-predictors.html
      linear = TSLM(.outcome ~ trend()),
      # Exponential smoothing methods
      # https://otexts.com/fpp3/ets.html
      ## ETS(A,N,N): simple exponential smoothing with additive errors
      ses_additive = ETS(.outcome ~ error("A") + trend("N") + season("N")),
      ## ETS(M,N,N): simple exponential smoothing with multiplicative errors
      ses_multiplicative = ETS(.outcome ~ error("M") + trend("N") + season("N")),
      ## ETS(A,A,N): Holt’s linear method with additive errors
      holt_additive = ETS(.outcome ~ error("A") + trend("A") + season("N")),
      ## ETS(M,A,N): Holt’s linear method with multiplicative errors
      holt_multiplicative = ETS(.outcome ~ error("M") + trend("A") + season("N")),
      # Auto ARIMA
      # https://otexts.com/fpp3/arima-r.html
      arima = ARIMA(.outcome, stepwise=FALSE, approximation=FALSE)
    )

  myforecast <-
    fit %>%
    forecast(h=horizon)

  ## the forecast obj is carrying along the column name 'y'
  ## need to set that input value for the outcome param
  names(test_data)[which(names(test_data) == outcome)] <- ".outcome"

  mean_forecasts <-
    myforecast %>%
    as_tibble() %>%
    select(-.outcome) %>%
    mutate(yweek = as.character(yweek)) %>%
    spread(yweek, .mean)

  accuracy(myforecast, test_data) %>%
    mutate(.outcome = outcome) %>%
    left_join(mean_forecasts)

}

## calculate and combine model metrics for different endpoints
metrics <-
  c("icases","ccases","ideaths","cdeaths") %>%
  map_df(., .f = evaluate_accuracy, horizon = horizon, .data = usa)

## plot results
metrics %>%
  gather(Metric, Value, ME:MAPE) %>%
  ggplot(aes(.model,Value)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ Metric + .outcome, scale = "free_x", ncol = 4)

