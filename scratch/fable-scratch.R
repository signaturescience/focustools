# https://fable.tidyverts.org/index.html
# https://otexts.com/fpp3/

library(tidyverse)
library(lubridate)
library(fable)
library(tsibble)
# library(fable.prophet) # install from source worked for me
theme_set(theme_classic())


# Set up national data ----------------------------------------------------

# Get national data
source(here::here("utils/get_data.R"))
usa <- get_cases(source="nyt",  granularity = "national")

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


# Create some models ------------------------------------------------------

# The linear trend is dumb, but use as a benchmark.
# I've done no other tuning of anything in the exponential model or arima model (which are roughly equivalent)
# For differences on ETS and ARIMA methods, see:
# https://otexts.com/fpp3/arima-ets.html

# Fit some models!
fit <-
  usa %>%
  model(
    # Linear trend
    # https://otexts.com/fpp3/useful-predictors.html
    linear = TSLM(icases ~ trend()),
    # Exponential smoothing
    # https://otexts.com/fpp3/estimation-and-model-selection.html
    exponential = ETS(icases ~ error("A") + trend("A") + season("N")),
    # Auto ARIMA
    # https://otexts.com/fpp3/arima-r.html
    arima = ARIMA(icases, stepwise=FALSE, approximation=FALSE),
  )

# Combine those forecasts! Which makes no sense at all given the linear forecast
# https://otexts.com/fpp3/combinations.html
fit <- fit %>%
  mutate(comb = (linear+exponential+arima)/3)

# plot actual data versus fitted values
augment(fit) %>%
  mutate(date=as_date(yweek)) %>%
  ggplot(aes(x=date)) +
  geom_line(aes(y=icases), lty=3, lwd=1) +
  geom_line(aes(y=.fitted, col=.model))

# forecast four period (four weeks)
myforecast <- fit %>%
  forecast(h=horizon)

# plot forecast against actual data observed to date
myforecast %>%
  autoplot(usa, level=c(80, 95), alpha=.5)


# Distributional forecasts ------------------------------------------------

# forecast distributions
# https://otexts.com/fpp3/prediction-intervals.html

# prediction intervals (good luck getting this into something tidier)
myforecast %>%
  hilo(level=c(97.5, 90, 75, 50))

# do it with boostrapping

# first you need a function to return a tibble of quantiles and the value
# requires dplyr 1.0.0 so summarise can return a tibble
# make a quantile tibble for quantiles required for N wk ahead inc case target
quibble <- function(x, q = c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)) {
  tibble(q = q, x = quantile(x, q))
}
# quibble(mtcars$mpg)
# A tibble: 7 x 2
#        q     x
#    <dbl> <dbl>
# 1  0.025  10.4
# 2  0.1    14.3
# 3  0.25   15.4
# 4  0.5    19.2
# 5  0.75   22.8
# 6  0.9    30.1
# 7  0.975  32.7

# bootstrap a model
boots <- fit %>%
  generate(h=horizon, times=1000, bootstrap=TRUE)

# get the quantiles
myquibbles <- boots %>%
  as_tibble() %>%
  group_by(.model, yweek) %>%
  summarize(quibble(.sim), .groups="drop")

# join back to the forecast and spread to take a look
myforecast %>%
  as_tibble() %>%
  inner_join(myquibbles) %>%
  spread(q, x)

# TODO: now need a way to turn this back into epiweeks!

# Other stuff -------------------------------------------------------------

# TODO: forecasting on training and testing sets
# https://otexts.com/fpp3/forecasting-on-training-and-test-sets.html

# TODO: check accuracy
# https://otexts.com/fpp3/distaccuracy.html
