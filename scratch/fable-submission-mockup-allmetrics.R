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


# Model -------------------------------------------------------------------

fit.icases <- usa %>% model(arima = ARIMA(icases, stepwise=FALSE, approximation=FALSE))
fit.ideaths <- usa %>% model(arima = ARIMA(ideaths, stepwise=FALSE, approximation=FALSE))
fit.cdeaths <- usa %>% model(arima = ARIMA(cdeaths, stepwise=FALSE, approximation=FALSE))

plotforecast <- function(fit) fit %>% forecast(h=horizon) %>% autoplot(usa)
plotforecast(fit.icases)
plotforecast(fit.ideaths) #uh-oh
plotforecast(fit.cdeaths)

# all different arima parameters
report(fit.icases)
report(fit.ideaths)
report(fit.cdeaths)


# Format all for submission -----------------------------------------------

# make a quantile tibble for quantiles required for N wk ahead inc case target
# For N wk ahead inc case target, filter to: c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)
quibble <- function(x, q = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)) {
  tibble(q = q, x = quantile(x, q))
}


fit <- fit.icases

format_fit_for_submission <- function(mable, horizon) {

  # get target name
  targetname <-
    substitute(mable) %>%
    as.character() %>%
    gsub(".*icases.*", "inc case", .) %>%
    gsub(".*ideaths.*", "inc death", .) %>%
    gsub(".*cdeaths.*", "cum death", .)

  # forecast
  myforecast <- forecast(fit, h=horizon)

  # bootstrap a model
  boots <-
    fit %>%
    generate(h=horizon, times=1000, bootstrap=TRUE)

  # get the quantiles
  myquibbles <-
    boots %>%
    as_tibble() %>%
    group_by(.model, yweek) %>%
    summarize(quibble(.sim), .groups="drop")

  bound <-
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
    mutate(target=glue::glue("{N} wk ahead {targetname}")) %>%
    select(-N) %>%
    # TODO: problem here, as_date starts on a monday... easier to explain via phone
    mutate(target_end_date=as_date(yweek)+5) %>%
    mutate(location="US", forecast_date=today()) %>%
    select(forecast_date, target, target_end_date, location, type, quantile, value)

  # restrict inc case quantiles to c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)
  if (targetname=="inc case") {
    bound <- bound %>%
      filter(type=="point" | quantile  %in% c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975))
  }

  return(bound)
}

submission <-
  list(format_fit_for_submission(fit.icases, horizon=horizon),
       format_fit_for_submission(fit.ideaths, horizon=horizon),
       format_fit_for_submission(fit.cdeaths, horizon=horizon)) %>%
  reduce(bind_rows)

submission %>% write_csv(here::here("scratch/fable-submission-mockup-allmetrics.csv"))

# submission %>% knitr::kable() %>% clipr::write_clip()
