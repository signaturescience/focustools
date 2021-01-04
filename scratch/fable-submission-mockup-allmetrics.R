# https://fable.tidyverts.org/index.html
# https://otexts.com/fpp3/

library(tidyverse)
library(lubridate)
library(fable)
library(tsibble)
theme_set(theme_classic())
source(here::here("utils/get_data.R"))
source(here::here("R/utils.R"))

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
fit.ccases <- usa %>% model(arima = ARIMA(ccases, stepwise=FALSE, approximation=FALSE))
fit.ideaths <- usa %>% model(arima = ARIMA(ideaths, stepwise=FALSE, approximation=FALSE))
fit.cdeaths <- usa %>% model(arima = ARIMA(cdeaths, stepwise=FALSE, approximation=FALSE))

plotforecast <- function(fit) fit %>% forecast(h=horizon) %>% autoplot(usa)
p1 <- plotforecast(fit.icases) + labs(title="Incident Cases")
p2 <- plotforecast(fit.ccases) + labs(title="Cumulative Cases")
p3 <- plotforecast(fit.ideaths)  + labs(title="Incident Deaths") #uh-oh
p4 <- plotforecast(fit.cdeaths) + labs(title="Cumulative Deaths")
library(patchwork)
(p1+p2)/(p3+p4)

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

  bound <- bound %>%
    mutate(quantile=round(quantile, 4)) %>%
    mutate(value=as.integer(round(value)))

  return(bound)
}

submission <-
  list(format_fit_for_submission(fit.icases, horizon=horizon, target_name = "inc cases"),
       format_fit_for_submission(fit.ideaths, horizon=horizon, target_name = "inc deaths"),
       format_fit_for_submission(fit.cdeaths, horizon=horizon, target_name = "cum deaths")) %>%
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

forecast_filename <- here::here("scratch/fable-submission-mockup-allmetrics-forecasts/2021-01-04-sigsci-arima.csv")
submission %>% write_csv(forecast_filename)

# submission %>% knitr::kable() %>% clipr::write_clip()


# validate ----------------------------------------------------------------

# wget https://raw.githubusercontent.com/signaturescience/covid19-forecast-hub/master/code/validation/R-scripts/functions_plausibility.R
source(here::here("utils/functions_plausibility.R"))
validate_file(forecast_filename)
