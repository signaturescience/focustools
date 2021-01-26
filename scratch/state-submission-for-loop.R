library(tidyverse)
library(fable)
library(focustools)

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
  filter(location %in% c("US", str_pad(1:56, width=2, pad="0"))) %>%
  make_tsibble() %>%
  filter(monday>"2020-03-01")
stopifnot(length(unique(usafull$location))==52L)
rm(national, state)

# Set forecasting horizon in weeks
horizon <- 4

# Use bootstrapping?
bootstrap <- FALSE

mylocs <- unique(usafull$location)
mylocs <- c("US", "48", "51", "19", "06")

submission_list <- list()
for (loc in mylocs) {
  message(loc)
  usa <- filter(usafull, location==loc)
  fits.icases <-  usa %>% model(arima = ARIMA(icases, stepwise=FALSE, approximation=FALSE))
  fits.ideaths <- usa %>% model(linear_caselag3 = TSLM(ideaths ~ lag(icases, 3)))
  forc.icases <- ts_forecast(fits.icases, outcome = "icases", horizon = horizon, bootstrap=bootstrap)
  futr.icases <- ts_futurecases(usa, forc.icases, horizon = horizon)
  forc.ideaths <- ts_forecast(fits.ideaths,  outcome = "ideaths", new_data = futr.icases, bootstrap = bootstrap)
  forc.cdeaths <- ts_forecast(outcome = "cdeaths", .data = usa, inc_forecast = forc.ideaths)
  submission_list[[loc]] <-
    list(format_for_submission(forc.icases,  target_name = "inc case"),
         format_for_submission(forc.ideaths, target_name = "inc death"),
         format_for_submission(forc.cdeaths, target_name = "cum death")) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    dplyr::arrange(target)
}
submission <- bind_rows(submission_list)
rm(usa, fits.icases, fits.ideaths, forc.icases, forc.ideaths, futr.icases, forc.cdeaths, submission_list)

# Write/validate submission
submission_filename  <-  here::here("submission", "SigSci-TS", paste0(Sys.Date(), "-SigSci-TS.csv"))
if (interactive()) submission_filename <- file.path(tempdir(), paste0(Sys.Date(), "-SigSci-TS.csv"))
write_csv(submission, file=submission_filename)
validate_forecast(submission_filename)

# Plot if interactive
if (interactive()) p <- plot_forecast(.data=usafull, submission = submission, location="US", pi=FALSE); print(p)
if (interactive()) p <- plot_forecast(.data=usafull, submission = submission, location=unique(submission$location), pi=TRUE)
if (interactive()) ggplot2::ggsave(plot=p, filename="~/Downloads/us-and-states.pdf", width=10, height=10, limitsize=FALSE)
if (interactive()) ggplot2::ggsave(plot=p, filename="~/Downloads/us-and-states.png", width=10, height=10, limitsize=FALSE)


