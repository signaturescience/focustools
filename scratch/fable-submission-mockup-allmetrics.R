# https://fable.tidyverts.org/index.html
# https://otexts.com/fpp3/

library(tidyverse)
library(lubridate)
library(fable)
library(tsibble)
theme_set(theme_classic())

# This doesn't seem to fix the TODO noted below, and further messes up the bootstrapping bit
# options(lubridate.week.start = 7)


# Set up national data ----------------------------------------------------

# Get national data
source(here::here("utils/get_data.R"))
usac <-  get_cases(source="nyt",  granularity = "national")
usad <- get_deaths(source="nyt",  granularity = "national")

# Turn into a tsibble (see function definition in utils/get_data.R)
usa <-  inner_join(usac, usad, by = c("epiyear", "epiweek")) %>% make_tsibble(chop=TRUE)
tail(usa)

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

format_fit_for_submission <- function(mable, horizon, target_name) {

  # Check for the correct target type
  stopifnot(target_name %in% c("inc cases", "inc deaths", "cum deaths"))

  ## bailing on the substitute()!
  ## just pass in an argument to this function for target_name ?

  # forecast
  myforecast <- forecast(mable, h=horizon)

  # bootstrap a model
  boots <-
    mable %>%
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
        mutate(type="point") %>%
        rename(value=.mean)
    ) %>%
    ## instead of selecting *out* outcome name ('ideaths','icases',etc) ...
    ## select *in* variables besides outcome
    select(.model:type) %>%
    arrange(type, quantile, yweek) %>%
    group_by(yweek) %>%
    mutate(N=cur_group_id()) %>%
    ungroup() %>%
    mutate(target=glue::glue("{N} wk ahead {target_name}")) %>%
    select(-N) %>%
    # Fix dates: as_date(yweek) returns the MONDAY that starts that week. add 5 days to get the Saturday date.
    mutate(target_end_date=as_date(yweek)+days(5)) %>%
    ## think i've fixed this ...
    ## some sleight of hand to get the "year week" format to epiyear, epiweek separately ...
    ## then use MMRweek to convert that to the *first* day of the epiweek and add 6 to get the last date
    # mutate(target_end_year = lubridate::epiyear(yweek),
    #        target_end_week = lubridate::epiweek(yweek),
    #        target_end_date = MMWRweek::MMWRweek2Date(target_end_year, target_end_week) + 6) %>%
    mutate(location="US", forecast_date=today()) %>%
    select(forecast_date, target, target_end_date, location, type, quantile, value)

  # restrict inc case quantiles to c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)
  if (target_name=="inc case") {
    bound <- bound %>%
      filter(type=="point" | quantile  %in% c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975))
  }

  return(bound)
}

submission <-
  list(format_fit_for_submission(fit.icases, horizon=horizon, target_name = "inc cases"),
       format_fit_for_submission(fit.ideaths, horizon=horizon, target_name = "inc deaths"),
       format_fit_for_submission(fit.cdeaths, horizon=horizon, target_name = "cum deaths")) %>%
  reduce(bind_rows) %>%
  arrange(target)

# submission %>%
#   distinct(target, target_end_date)

submission %>% write_csv(here::here("scratch/fable-submission-mockup-allmetrics.csv"))

# submission %>% knitr::kable() %>% clipr::write_clip()
