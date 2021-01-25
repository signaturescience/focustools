library(dplyr)
library(fable)
library(focustools)

# Get national data
national <- inner_join(
  get_cases( source="jhu", granularity="national"),
  get_deaths(source="jhu", granularity="national"),
  by = c("location", "epiyear", "epiweek"))
# Get state data
state <- inner_join(
  get_cases( source="jhu", granularity="state"),
  get_deaths(source="jhu", granularity="state"),
  by = c("location", "epiyear", "epiweek"))
# combine US and state data
usa <-
  bind_rows(national, state) %>%
  filter(location %in% c("US", stringr::str_pad(1:56, width=2, pad="0"))) %>%
  make_tsibble() %>%
  filter(monday>"2020-03-01")
stopifnot(length(unique(usa$location))==52L)

## Limit to just national level data?
usa <- usa %>% filter(location=="US")

## Limit to just national level data plus one state
# usa <- usa %>% filter(location=="US" | location=="11")


horizon <- 4
fit.icases <-  usa %>% model(arima = ARIMA(icases, stepwise=FALSE, approximation=FALSE))
fit.ideaths <- usa %>% model(linear_caselag3 = TSLM(ideaths ~ lag(icases, 3)))
icases_forecast <- ts_forecast(fit.icases, outcome = "icases", horizon = horizon)
future_cases <- ts_futurecases(usa, icases_forecast, horizon = horizon)
ideaths_forecast <- ts_forecast(fit.ideaths,  outcome = "ideaths", new_data = future_cases)
cdeaths_forecast <- ts_forecast(outcome = "cdeaths", .data = usa, inc_forecast = ideaths_forecast)

submission <-
  list(format_for_submission(icases_forecast, target_name = "inc case"),
       format_for_submission(ideaths_forecast, target_name = "inc death"),
       format_for_submission(cdeaths_forecast, target_name = "cum death")) %>%
  purrr::reduce(dplyr::bind_rows) %>%
  dplyr::arrange(target)

if (interactive()) plot_forecast(.data=usa, submission = submission, location="US", pi=FALSE)

submission_filename <- here::here("submission", "SigSci-TS", paste0(Sys.Date(), "-SigSci-TS.csv"))
readr::write_csv(submission, file=submission_filename)
validate_forecast(submission_filename)
