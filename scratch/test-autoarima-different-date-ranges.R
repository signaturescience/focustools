suppressPackageStartupMessages(suppressWarnings(library(dplyr)))
suppressPackageStartupMessages(suppressWarnings(library(fable)))
suppressPackageStartupMessages(suppressWarnings(library(focustools)))

# Get the data as of 2021-02-08 and save it to work with later
# usa <- inner_join(
#   get_cases( source="jhu", granularity="national"),
#   get_deaths(source="jhu", granularity="national"),
#   by = c("location", "epiyear", "epiweek")) %>%
#   make_tsibble() %>%
#   filter(monday>"2020-03-01")
# saveRDS(usa, here::here("scratch/2021-02-08-usa-data.rds"))

horizon <- 4
usa <- readRDS(here::here("scratch/2021-02-08-usa-data.rds"))

# What's our model and forecast look like today, without log transform?
# 5,1,0
# terrible forecast
usa %>%
  model(arima = ARIMA(icases, stepwise=FALSE, approximation=FALSE)) %>%
  print() %>%
  forecast(h=horizon)

# What's the model look like if we use log+1?
# 2,1,0
# reasonable forecasts, but low to start with, followed by a rise
usa %>%
  model(arima = ARIMA(log(icases+1), stepwise=FALSE, approximation=FALSE)) %>%
  print() %>%
  forecast(h=horizon)

# What's the model look like without log transform if we did this without the most recent week?
# 5,1,0
usa %>%
  head(-1) %>%
  model(arima = ARIMA(icases, stepwise=FALSE, approximation=FALSE)) %>%
  print() %>%
  forecast(h=horizon)

# What's the model look like without log transform if we did this without the most 2 weeks?
# 2,1,3
usa %>%
  head(-2) %>%
  model(arima = ARIMA(icases, stepwise=FALSE, approximation=FALSE)) %>%
  print() %>%
  forecast(h=horizon)

# What's the model look like without log transform if we did this without the most 3 weeks?
# 4,1,0
usa %>%
  head(-3) %>%
  model(arima = ARIMA(icases, stepwise=FALSE, approximation=FALSE)) %>%
  print() %>%
  forecast(h=horizon)

# What's the model look like without log transform if we did this without the most 4 weeks?
# 2,1,4
usa %>%
  head(-4) %>%
  model(arima = ARIMA(icases, stepwise=FALSE, approximation=FALSE)) %>%
  print() %>%
  forecast(h=horizon)

# Non seasonal, restrict AR terms to 0:2, differencing to 0:2, and 0 order moving average
usa %>%
  model(arima = ARIMA(icases~PDQ(0,0,0)+pdq(0:2,0:2,0), stepwise=FALSE, approximation=FALSE)) %>%
  print() %>%
  forecast(h=horizon)

