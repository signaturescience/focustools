# https://fable.tidyverts.org/index.html
# https://otexts.com/fpp3/

library(tidyverse)
library(lubridate)
library(fable)
library(tsibble)
theme_set(theme_classic())
source(here::here("utils/get_data.R"))
source(here::here("R/utils.R"))

# define forecasting functions

## COMING SOON: generic wrapper to fable::model (this would let us pass in a list of arbitrary functions to the functions)
# ts_fit <- function(.data, outcome, model) {
#
# }

ts_forecast <- function(mable, horizon = 4, new_data = NULL, seed = 1863) {

  # forecast
  myforecast <- forecast(mable, h=horizon, new_data = new_data)

  # bootstrap a model
  boots <-
    mable %>%
    generate(h=horizon, times=1000, bootstrap=TRUE, new_data = new_data, seed = seed)

  # get the quantiles
  myquibbles <-
    boots %>%
    as_tibble() %>%
    group_by(.model, yweek) %>%
    summarize(quibble(.sim), .groups="drop")

  .forecast <-
    bind_rows(
      myquibbles %>%
        mutate(type="quantile") %>%
        rename(quantile=q, value=x),
      myforecast %>%
        as_tibble() %>%
        mutate(quantile=NA_real_, .after=yweek) %>%
        mutate(type="point") %>%
        rename(value=.mean)
    )

  ## return named list with forecast AND quibbles
  return(.forecast)

}

# Format all for submission -----------------------------------------------

format_fit_for_submission <- function(.forecast, target_name) {

  # Check for the correct target type
  stopifnot(target_name %in% c("inc case", "inc death", "cum death"))

  bound <-
    .forecast %>%
    select(.model:type) %>%
    arrange(type, quantile, yweek) %>%
    group_by(yweek) %>%
    mutate(N=cur_group_id()) %>%
    ungroup() %>%
    mutate(target=glue::glue("{N} wk ahead {target_name}")) %>%
    select(-N) %>%
    # Fix dates: as_date(yweek) returns the MONDAY that starts that week. add 5 days to get the Saturday date.
    mutate(target_end_date=as_date(yweek)+days(5)) %>%
    mutate(location="US", forecast_date=today()) %>%
    select(forecast_date, target, target_end_date, location, type, quantile, value)

  # restrict inc case quantiles to c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)
  if (target_name=="inc case") {
    bound <- bound %>%
      filter(type=="point" | quantile  %in% c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975))
  }

  bound <- bound %>%
    mutate(quantile=round(quantile, 4)) %>%
    mutate(value=as.integer(round(value)))

  return(bound)
}


# Set up national data ----------------------------------------------------

# Get national data
usac <-  get_cases(source="nyt",  granularity = "national")
usad <- get_deaths(source="nyt",  granularity = "national")

# Turn into a tsibble (see function definition in utils/get_data.R)
usa <-  inner_join(usac, usad, by = c("epiyear", "epiweek")) %>% make_tsibble(chop=TRUE)
tail(usa)

# when later forecasting or limiting to training/testing, how many periods?
horizon <- 4

# Model -------------------------------------------------------------------

fit.icases <- usa %>% model(arima = ARIMA(icases, stepwise=FALSE, approximation=FALSE))
## NOTE: using the lagged cases (3 weeks) as predictor ... maybe eventually some way to combine this with ARIMA via xreg formulat (see ?ARIMA)
fit.ideaths <- usa %>% model(linear_caselag3 = TSLM(ideaths ~ lag(icases, 3)))
# fit.ideaths <- usa %>% model(arima = ARIMA(ideaths, stepwise=FALSE, approximation=FALSE))
## NOTE: for now we are just getting the
fit.cdeaths <- usa %>% model(arima = ARIMA(cdeaths, stepwise=FALSE, approximation=FALSE))

icases_forecast <- ts_forecast(fit.icases, horizon = 4)

## best guess for incident cases in the future
## need this to forecast deaths
best_guess <-
  icases_forecast %>%
  filter(type == "point") %>%
  arrange(yweek) %>%
  pull(value)

future_cases <-
  new_data(usa, 4) %>%
  mutate(icases = best_guess)

## NOTE: you will probably see a WARNING here about horizon being ignored ...
## ... not an issue given that new_data object passes along the horizon info
ideaths_forecast <- ts_forecast(fit.ideaths, new_data = future_cases)

## if modeled the "old" (ARIMA) way ...
# ideaths_forecast <- ts_forecast(fit.ideaths, horizon = 4)

## NOTE: we need to figure out how to get the cumulative deaths from incident deaths model
cdeaths_forecast <- ts_forecast(fit.cdeaths, horizon = 4)

format_fit_for_submission(icases_forecast, target_name = "inc case")

submission <-
  list(format_fit_for_submission(icases_forecast, target_name = "inc case"),
       format_fit_for_submission(ideaths_forecast, target_name = "inc death"),
       format_fit_for_submission(cdeaths_forecast, target_name = "cum death")) %>%
  reduce(bind_rows) %>%
  arrange(target)

# Ensure quantiles for cum aren't below current ---------------------------

# What's the most recent week's cumulative deaths?
most_recent_cdeaths <- tail(usa$cdeaths, 1)

# Ensure ANY value (not just 10th quantile) for cumulative isn't below current
submission <-
  submission %>%
  # indicator column whether any value for cumulative deaths is below the most recent cumulative death
  mutate(tmp_is_cdeaths_below_current = grepl("cum deaths", target) & value<most_recent_cdeaths) %>%
  # if so, set that value to the most recent cumulative deaths
  mutate(value = ifelse(tmp_is_cdeaths_below_current, most_recent_cdeaths, value)) %>%
  # get rid of that indicator
  select(-starts_with("tmp_"))

# write out ---------------------------------------------------------------

forecast_filename <- here::here("scratch/fable-submission-mockup-allmetrics-forecasts/2021-01-04-sigsci-ts.csv")
submission %>% write_csv(forecast_filename)

# submission %>% knitr::kable() %>% clipr::write_clip()

# validate ----------------------------------------------------------------

# wget https://raw.githubusercontent.com/signaturescience/covid19-forecast-hub/master/code/validation/R-scripts/functions_plausibility.R
source(here::here("utils/functions_plausibility.R"))
validate_file(forecast_filename)
