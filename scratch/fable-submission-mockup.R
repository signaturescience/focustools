# https://fable.tidyverts.org/index.html
# https://otexts.com/fpp3/

library(tidyverse)
library(lubridate)
library(fable)
library(tsibble)
theme_set(theme_classic())

# TODO: This doesn't seem to fix the TODO noted below, and further messes up the bootstrapping bit
# options(lubridate.week.start = 7)


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


# Model -------------------------------------------------------------------

fit <-
  usa %>%
  model(
    # Auto ARIMA
    # https://otexts.com/fpp3/arima-r.html
    arima = ARIMA(icases, stepwise=FALSE, approximation=FALSE),
  )

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

# forecast distributions with bootstrapping
# https://otexts.com/fpp3/prediction-intervals.html

# make a quantile tibble for quantiles required for N wk ahead inc case target
quibble <- function(x, q = c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)) {
  tibble(q = q, x = quantile(x, q))
}

# bootstrap a model
boots <- fit %>%
  generate(h=horizon, times=1000, bootstrap=TRUE)

# get the quantiles
myquibbles <- boots %>%
  as_tibble() %>%
  group_by(.model, yweek) %>%
  summarize(quibble(.sim), .groups="drop")


submission.us <-
  bind_rows(
    myquibbles %>%
      mutate(type="quantile") %>%
      rename(quantile=q, value=x),
    myforecast %>%
      as_tibble() %>%
      mutate(quantile=NA_real_, .after=yweek) %>%
      select(-icases) %>%
      mutate(type="point") %>%
      rename(value=.mean)
  ) %>%
  arrange(type, quantile, yweek) %>%
  group_by(yweek) %>%
  mutate(N=cur_group_id()) %>%
  ungroup() %>%
  mutate(target=paste(N, "wk ahead inc case")) %>%
  select(-N) %>%
  # TODO: problem here, as_date starts on a monday... easier to explain via phone
  mutate(target_end_date=as_date(yweek)+5) %>%
  mutate(location="US", forecast_date=today()) %>%
  select(forecast_date, target, target_end_date, location, type, quantile, value)

# submission.us %>% knitr::kable() %>% clipr::write_clip()
