
<!-- README.md is generated from README.Rmd. Please edit that file -->

# focustools

<!-- badges: start -->

[![R CMD Check
(stable)](https://github.com/signaturescience/focustools/workflows/R-CMD-check-stable/badge.svg)](https://github.com/signaturescience/focustools/actions)

[![R CMD Check
(dev)](https://github.com/signaturescience/focustools/workflows/R-CMD-check-dev/badge.svg)](https://github.com/signaturescience/focustools/actions)

<!-- badges: end -->

## Installation

Install from GitHub as follows:

``` r
# install.packages("devtools")
devtools::install_github("signaturescience/focustools")
```

## Usage

### Generating forecasts and formatting submission

Here we provide an example of how to use `focustools` to generate
forecasts for COVID-19 Forecast Hub targets via time series approaches.

First, load ancillary packages for data analysis and retrieve COVID-19
data from the JHU source. Note that the `get_cases()` and `get_deaths()`
functions are executed separately, then joined and converted to a
`tsibble`:

``` r
library(focustools)
library(dplyr)
library(readr)
library(purrr)
library(fabletools)
library(fable)

## get data at the national scale from jhu source
usac <-  get_cases(source="jhu",  granularity = "national")
usad <- get_deaths(source="jhu",  granularity = "national")

## use the focustools helper to prep the tsibble format
usa <-  
  dplyr::inner_join(usac, usad, by = c("location", "epiyear", "epiweek")) %>% 
  make_tsibble(chop=TRUE)
```

With the data prepped, we can now fit the models. In this case, we’ll
fit two models. One will be an ARIMA of incident cases, with parameters
automatically optimized. For more details see `?fable::ARIMA`. The other
model will use the three week lagged incident case counts to predict
incident deaths:

``` r
fit.icases <- usa %>% model(arima = ARIMA(icases, stepwise=FALSE, approximation=FALSE))
fit.ideaths <- usa %>% model(linear_caselag3 = TSLM(ideaths ~ lag(icases, 3)))
```

Note that the code above uses the `fabletools::model()` function
directly. The `focustools` package does include `ts_fit()`, which is a
wrapper for modeling multiple outcomes and/or using a list of model
specifications. However, currently `ts_fit()` is incompatible with the
`TSLM` model used above for the incident deaths. For an example of
`ts_fit()` in action see the “Evaluating model accuracy” section below.

With the model fit objects created, we can generate forecasts (including
point and quantile estimates) for the outcomes of interest at an
arbitrary horizon. Note that given the model we have chosen for the
incident deaths outcome, we do need to make sure to generate the
incident case forecast *first* so that we can retrieve point estimates
for future cases, which will be passed into the incident deaths
forecast:

``` r
## generate incident case forecast
icases_forecast <- ts_forecast(fit.icases, outcome = "icases", horizon = 4)
icases_forecast

## need to get future cases to pass to ideaths forecast
future_cases <- ts_futurecases(usa, icases_forecast, horizon = 4)
# Forecast incident deaths based on best guess for cases
ideaths_forecast <- ts_forecast(fit.ideaths,  outcome = "ideaths", new_data = future_cases)
ideaths_forecast
```

We can use the forecasts of incidence to infer cumulative targets. The
`ts_forecast()` function is flexible enough to switch methods
internally, requiring a “inc\_forecast” parameter if either “cdeaths” or
“ccases” is passed as the “outcome” for the forecast:

``` r
## generate cumulative forecasts
cdeaths_forecast <- ts_forecast(outcome = "cdeaths", .data = usa, inc_forecast = ideaths_forecast)
cdeaths_forecast
```

Prior to submitting these forecasts to the COVID-19 Forecast Hub we need
to prepare them in the canonical submission format. Data prep in
`format_for_submission` includes naming targets appropriately and
ensuring that target dates adhere to epidemiological week format:

``` r
## create submission object
submission <-
  list(format_for_submission(icases_forecast, target_name = "inc case"),
       format_for_submission(ideaths_forecast, target_name = "inc death"),
       format_for_submission(cdeaths_forecast, target_name = "cum death")) %>%
  reduce(bind_rows) %>%
  arrange(target)

submission
```

Last of all, we can validate the submission. The object must be written
to a file and then validated with `validate_forecast()`, which wraps a
python function developed and maintained by the COVID-19 Forecast Hub
organizers. For more information on validating the entry see the
“Submission validation” section below:

``` r
## set up file path for submission file
## forcing the date in the file name and submission contents to be this monday
ffile <- file.path(tempdir(), paste0(this_monday(), "-sigsci-arima.csv"))
submission %>%
  mutate(forecast_date = this_monday()) %>%
  write_csv(ffile)

validate_forecast(ffile)
```

## Submission validation

Submissions to the COVID-19 Forecast Hub must comply to the entry format
for the forecast file:

<https://github.com/reichlab/covid19-forecast-hub/blob/master/data-processed/README.md#forecast-file-format>

The organizers have provided code to validate entry formats. We have
included some of those methods in this repo, such that entry format can
be checked as follows (uncomment the first line if you’ve installed
dependencies to a conda environment called `focus`):

    # conda activate focus
    python3 inst/validation/validate_single_forecast_file.py scratch/fable-submission-mockup-allmetrics-forecasts/2021-01-04-sigsci-ts.csv

Note that the following Python modules should be installed:

    pandas
    requests
    pymmwr
    click
    urllib3
    selenium
    webdriver-manager
    pyyaml
    PyGithub
    git+https://github.com/reichlab/zoltpy/
    https://github.com/hannanabdul55/pykwalify/archive/master.zip

You can install these dependencies by running the following. Optionally
create and activate a conda environment prior to installing these
dependencies.

    # conda create -n focus
    # conda activate focus
    pip3 install -r inst/validation/requirements.txt

In order to run a very similar check using R, we have developed a
wrapper to the [`validate_quantile_csv_file()` function from the
`zoltpy`
module](https://github.com/reichlab/zoltpy/blob/master/zoltpy/covid19.py#L75-L93),
which drives the `validate_single_forecast_file.py` script above. To use
the wrapper function in R:

    validate_forecast("submission/SigSci-TS/2021-01-11-SigSci-TS.csv")

## Script to create submission-ready forecasts

Run the script in [submission/submission.R](submission/submission.R) to
do the following.

1.  Get data (national level from JHU by default)
2.  Fit incident case and incident death models (ARIMA and lagged TSLM
    respectively)
3.  Get future case data to create the incident death forecast
4.  Create the incident death forecast based on this new data
5.  Generate cumulative death forecast by summarizing incident death
    forecast
6.  Prepare submission format
7.  Save the submission to the
    `submission/TEAM-MODEL/YYYY-MM-DD-TEAM-MODEL.csv`
8.  Validates the submission
