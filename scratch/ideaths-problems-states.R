library(dplyr)
library(fable)
library(focustools)

## Later make this a command line option or set to TRUE if no CL options given?
USonly <- TRUE

## Get national data
national <- inner_join(
  get_cases( source="jhu", granularity="national"),
  get_deaths(source="jhu", granularity="national"),
  by = c("location", "epiyear", "epiweek"))
## Get state data
state <- inner_join(
  get_cases( source="jhu", granularity="state"),
  get_deaths(source="jhu", granularity="state"),
  by = c("location", "epiyear", "epiweek"))
## combine US and state data
usafull <-
  bind_rows(national, state) %>%
  filter(location %in% c("US", stringr::str_pad(1:56, width=2, pad="0"))) %>%
  make_tsibble() %>%
  filter(monday>"2020-03-01")
stopifnot(length(unique(usafull$location))==52L)



# National only -----------------------------------------------------------

# Make usa object US-only
usa <- usafull %>% filter(location=="US")

## Create models and forecasts
horizon <- 4
fit.icases <-  usa %>% model(arima = ARIMA(icases, stepwise=FALSE, approximation=FALSE))
fit.ideaths <- usa %>% model(linear_caselag3 = TSLM(ideaths ~ lag(icases, 3)))
icases_forecast <- ts_forecast(fit.icases, outcome = "icases", horizon = horizon)
future_cases <- ts_futurecases(usa, icases_forecast, horizon = horizon)
ideaths_forecast <- ts_forecast(fit.ideaths,  outcome = "ideaths", new_data = future_cases)
cdeaths_forecast <- ts_forecast(outcome = "cdeaths", .data = usa, inc_forecast = ideaths_forecast)

# Format for submission
submission <-
  list(format_for_submission(icases_forecast, target_name = "inc case"),
       format_for_submission(ideaths_forecast, target_name = "inc death"),
       format_for_submission(cdeaths_forecast, target_name = "cum death")) %>%
  purrr::reduce(dplyr::bind_rows) %>%
  dplyr::arrange(target)

# Plot if interactive
if (interactive()) plot_forecast(.data=usa, submission = submission, location="US", pi=FALSE)
ggsave("~/Downloads/01-national-only-forecast.png", width=15, height=5)


# Virginia only -----------------------------------------------------------

# Make usa object virginia-only
usa <- usafull %>% filter(location=="51")

## Create models and forecasts
horizon <- 4
fit.icases <-  usa %>% model(arima = ARIMA(icases, stepwise=FALSE, approximation=FALSE))
fit.ideaths <- usa %>% model(linear_caselag3 = TSLM(ideaths ~ lag(icases, 3)))
icases_forecast <- ts_forecast(fit.icases, outcome = "icases", horizon = horizon)
future_cases <- ts_futurecases(usa, icases_forecast, horizon = horizon)
ideaths_forecast <- ts_forecast(fit.ideaths,  outcome = "ideaths", new_data = future_cases)
cdeaths_forecast <- ts_forecast(outcome = "cdeaths", .data = usa, inc_forecast = ideaths_forecast)

# Format for submission
submission <-
  list(format_for_submission(icases_forecast, target_name = "inc case"),
       format_for_submission(ideaths_forecast, target_name = "inc death"),
       format_for_submission(cdeaths_forecast, target_name = "cum death")) %>%
  purrr::reduce(dplyr::bind_rows) %>%
  dplyr::arrange(target)

# Plot if interactive
if (interactive()) plot_forecast(.data=usa, submission = submission, location="51", pi=FALSE)
ggsave("~/Downloads/02-virginia-only-forecast.png", width=15, height=5)




# US plus one state -------------------------------------------------------

# Make usa object US or virginia
usa <- usafull %>% filter(location=="US" | location=="51")

## Create models and forecasts
horizon <- 4
fit.icases <-  usa %>% model(arima = ARIMA(icases, stepwise=FALSE, approximation=FALSE))
fit.ideaths <- usa %>% model(linear_caselag3 = TSLM(ideaths ~ lag(icases, 3)))
icases_forecast <- ts_forecast(fit.icases, outcome = "icases", horizon = horizon)
future_cases <- ts_futurecases(usa, icases_forecast, horizon = horizon)
ideaths_forecast <- ts_forecast(fit.ideaths,  outcome = "ideaths", new_data = future_cases)
cdeaths_forecast <- ts_forecast(outcome = "cdeaths", .data = usa, inc_forecast = ideaths_forecast)

# Format for submission
submission <-
  list(format_for_submission(icases_forecast, target_name = "inc case"),
       format_for_submission(ideaths_forecast, target_name = "inc death"),
       format_for_submission(cdeaths_forecast, target_name = "cum death")) %>%
  purrr::reduce(dplyr::bind_rows) %>%
  dplyr::arrange(target)

# Plot if interactive
if (interactive()) plot_forecast(.data=usa, submission = submission, location="US", pi=FALSE)
ggsave("~/Downloads/03-national-forecast-plus-virginia.png", width=15, height=5)
if (interactive()) plot_forecast(.data=usa, submission = submission, location="51", pi=FALSE)
ggsave("~/Downloads/04-virginia-forecast-plus-national.png", width=15, height=5)




# Two states --------------------------------------------------------------

# Make usa object Virginia or Texas
usa <- usafull %>% filter(location=="48" | location=="51")

## Create models and forecasts
horizon <- 4
fit.icases <-  usa %>% model(arima = ARIMA(icases, stepwise=FALSE, approximation=FALSE))
fit.ideaths <- usa %>% model(linear_caselag3 = TSLM(ideaths ~ lag(icases, 3)))
icases_forecast <- ts_forecast(fit.icases, outcome = "icases", horizon = horizon)
future_cases <- ts_futurecases(usa, icases_forecast, horizon = horizon)
ideaths_forecast <- ts_forecast(fit.ideaths,  outcome = "ideaths", new_data = future_cases)
cdeaths_forecast <- ts_forecast(outcome = "cdeaths", .data = usa, inc_forecast = ideaths_forecast)

# Format for submission
submission <-
  list(format_for_submission(icases_forecast, target_name = "inc case"),
       format_for_submission(ideaths_forecast, target_name = "inc death"),
       format_for_submission(cdeaths_forecast, target_name = "cum death")) %>%
  purrr::reduce(dplyr::bind_rows) %>%
  dplyr::arrange(target)

# Plot if interactive
if (interactive()) plot_forecast(.data=usa, submission = submission, location="51", pi=FALSE)
ggsave("~/Downloads/05-virginia-forecast-plus-texas.png", width=15, height=5)




