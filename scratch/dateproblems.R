# Load libs and set up data -----------------------------------------------

suppressWarnings(suppressPackageStartupMessages(library(tidyverse)))
suppressWarnings(suppressPackageStartupMessages(library(lubridate)))
suppressWarnings(suppressPackageStartupMessages(library(fable)))
suppressWarnings(suppressPackageStartupMessages(library(tsibble)))

source(here::here("utils/get_data.R"))

horizon <- 4

usatibble <- inner_join(
  get_cases(source="nyt",  granularity = "national"),
  get_deaths(source="nyt",  granularity = "national"),
  by = c("epiyear", "epiweek"))
tail(usatibble)


# Default lubridate options -----------------------------------------------

# Make the tsibble: note that "day" here is SUNDAY of the epiweek.
usa <- usatibble %>%
  # get the date that starts the MMWRweek
  # GET THE MONDAY that starts the week so yearweek ... XX document somewhere else.
  mutate(day=MMWRweek::MMWRweek2Date(epiyear, epiweek, MMWRday=2), .after="epiweek") %>%
  # Remove the incomplete week
  filter(week(day)!=week(today())) %>%
  # convert represent as yearweek (see package?tsibble)
  mutate(yweek=yearweek(day), .after="day") %>%
  # convert to tsibble
  as_tsibble(index=yweek)
tail(usa)



# You're doing the modeling on the yweek variable here as the index
# So what comes out of the modeling side you have to use that yweek to get back to dates.
# date is six days behind the sunday of yweek. epiweek is one off.
tail(usa) %>%
  select(epiyear:yweek) %>%
  mutate(date_from_yweek=as_date(yweek)) %>%
  mutate(epiweek_from_yweek=epiweek(yweek))

# fit ARIMA and generate a forecast
fit <- usa %>% model(arima = ARIMA(icases, stepwise=FALSE, approximation=FALSE))
fc <- forecast(fit, h=horizon)

# Forecast dates are a week off.
fc %>%
  mutate(date_from_yweek=as_date(yweek)) %>%
  mutate(epiweek_from_yweek=epiweek(yweek))

# But bootstrapping works fine.
boots <- fit %>% generate(h=horizon, times=1000, bootstrap=TRUE)


# Same code, different lubridate options ----------------------------------

options(lubridate.week.start = 7)

# Make the tsibble: note that "day" here is still SUNDAY of the epiweek.
usa <- usatibble %>%
  # get the date that starts the MMWRweek
  mutate(day=MMWRweek::MMWRweek2Date(epiyear, epiweek), .after="epiweek") %>%
  # Remove the incomplete week
  filter(week(day)!=week(today())) %>%
  # convert represent as yearweek (see package?tsibble)
  mutate(yweek=yearweek(day), .after="day") %>%
  # convert to tsibble
  as_tsibble(index=yweek)
tail(usa)

# You're doing the modeling on the yweek variable here as the index
# So what comes out of the modeling side you have to use that yweek to get back to dates.
# DATE IS NOW CORRECT, AS IS EPIWEEK
tail(usa) %>%
  select(epiyear:yweek) %>%
  mutate(date_from_yweek=as_date(yweek)) %>%
  mutate(epiweek_from_yweek=epiweek(yweek))

# fit ARIMA and generate a forecast
fit <- usa %>% model(arima = ARIMA(icases, stepwise=FALSE, approximation=FALSE))
fc <- forecast(fit, h=horizon)

# Forecast dates are CORRECTLY 4 weeks in advance
fc %>%
  mutate(date_from_yweek=as_date(yweek)) %>%
  mutate(epiweek_from_yweek=epiweek(yweek))

# But bootstrapping fails with some cryptic error that I can't crack.
boots <- fit %>% generate(h=horizon, times=1000, bootstrap=TRUE)
