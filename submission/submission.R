suppressPackageStartupMessages(suppressWarnings(library(dplyr)))
suppressPackageStartupMessages(suppressWarnings(library(readr)))
suppressPackageStartupMessages(suppressWarnings(library(fable)))
suppressPackageStartupMessages(suppressWarnings(library(focustools)))

## These could later be set by command line options.
## Use US only?
USonly <- FALSE
## Set forecasting horizon in weeks
horizon <- 4
## Use bootstrapping?
bootstrap <- FALSE

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
  filter(monday>"2020-03-09")
stopifnot(length(unique(usafull$location))==52L)
# Clean up
rm(national, state)

## Get all the locations in the data provided
mylocs <- unique(usafull$location)
## Limit to US only if option set at top
if (USonly) mylocs <- "US"
## Limit to US Plus a few states for testing
# mylocs <- c("US", "48", "51", "19", "06")

## Create models and forecasts
submission_list <- list()
arima_params_list <- list()
for (loc in mylocs) {
  message(loc)
  usa <- filter(usafull, location==loc)
  # fits.icases <-  usa %>% model(arima = ARIMA(log(icases+1), stepwise=FALSE, approximation=FALSE))
  fits.icases <-  usa %>% model(arima = ARIMA(icases~PDQ(0,0,0)+pdq(1:2,0:2,0), stepwise=FALSE, approximation=FALSE))
  arima_params_list[[loc]] <- extract_arima_params(fits.icases)
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
arima_params <- bind_rows(arima_params_list)
rm(usa, fits.icases, fits.ideaths, forc.icases, forc.ideaths, futr.icases, forc.cdeaths, loc)

## NOTE: if running interactively make sure that point estimates and intervals seem plausible ...
## see `?plot_forecast` or `?focus_explorer`

# Create submission in submission directory if it's Monday.
if (!is_monday()) warning("Forecasts should be created on Mondays.")
submission_filename <- here::here("submission", "SigSci-TS", paste0(Sys.Date(), "-SigSci-TS.csv"))
readr::write_csv(submission, submission_filename)
validation <- validate_forecast(submission_filename, install = !interactive())
write_lines(validation, here::here("submission", "SigSci-TS", paste0(Sys.Date(), "-validation.txt")))
readr::write_csv(arima_params, gsub("-SigSci-TS\\.csv$", "-params.csv", submission_filename))


# Plot if interactive
if (interactive()) plot_forecast(.data=usafull, submission = submission, location="US", pi=TRUE)
if (interactive()) plot_forecast(.data=usafull, submission = submission, location="06", pi=TRUE)
if (interactive()) plot_forecast(.data=usafull, submission = submission, location="48", pi=TRUE)
if (interactive()) plot_forecast(.data=usafull, submission = submission, location="51", pi=TRUE)

p <- plot_forecast(.data=usafull, submission = submission, location=unique(submission$location), pi=TRUE)
ggplot2::ggsave(plot=p, filename= here::here("submission", "SigSci-TS", paste0(Sys.Date(),"-us-and-states.pdf")), width=12, height=150, limitsize=FALSE, scale=.9)

# Interactively create submission in temp directory (forcing forecast_date to this monday)
if (interactive()) submission_filename <- file.path(tempdir(), paste0(Sys.Date(), "-SigSci-TS.csv"))
if (interactive()) readr::write_csv(submission %>% mutate(forecast_date=this_monday()), submission_filename)
if (interactive()) validate_forecast(submission_filename)

