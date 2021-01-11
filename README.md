
<!-- README.md is generated from README.Rmd. Please edit that file -->

# focustools

<!-- badges: start -->
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
data from the NYT source. Note that the `get_cases()` and `get_deaths()`
functions are executed separately, then joined and converted to a
`tsibble`:

``` r
library(focustools)
library(dplyr)
library(readr)
library(purrr)
library(fabletools)
library(fable)

## get data at the national scale from nyt source
usac <-  get_cases(source="nyt",  granularity = "national")
usad <- get_deaths(source="nyt",  granularity = "national")

## use the focustools helper to prep the tsibble format
usa <-  
  dplyr::inner_join(usac, usad, by = c("epiyear", "epiweek")) %>% 
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
#> # A tibble: 96 x 6
#>    .model    yweek quantile    value type     icases
#>    <chr>    <week>    <dbl>    <dbl> <chr>    <dist>
#>  1 arima  2021 W02    0.01  1704326. quantile      ?
#>  2 arima  2021 W02    0.025 1711439. quantile      ?
#>  3 arima  2021 W02    0.05  1719276. quantile      ?
#>  4 arima  2021 W02    0.1   1789882. quantile      ?
#>  5 arima  2021 W02    0.15  1797630. quantile      ?
#>  6 arima  2021 W02    0.2   1831974. quantile      ?
#>  7 arima  2021 W02    0.25  1850293. quantile      ?
#>  8 arima  2021 W02    0.3   1855465. quantile      ?
#>  9 arima  2021 W02    0.35  1863925. quantile      ?
#> 10 arima  2021 W02    0.4   1867684. quantile      ?
#> # … with 86 more rows

## need to get future cases to pass to ideaths forecast
future_cases <- ts_futurecases(usa, icases_forecast, horizon = 4)
# Forecast incident deaths based on best guess for cases
ideaths_forecast <- ts_forecast(fit.ideaths,  outcome = "ideaths", new_data = future_cases)
ideaths_forecast
#> # A tibble: 96 x 7
#>    .model             yweek quantile  value type     ideaths icases
#>    <chr>             <week>    <dbl>  <dbl> <chr>     <dist>  <dbl>
#>  1 linear_caselag3 2021 W02    0.01  18197. quantile       ?     NA
#>  2 linear_caselag3 2021 W02    0.025 18197. quantile       ?     NA
#>  3 linear_caselag3 2021 W02    0.05  18198. quantile       ?     NA
#>  4 linear_caselag3 2021 W02    0.1   18238. quantile       ?     NA
#>  5 linear_caselag3 2021 W02    0.15  19924. quantile       ?     NA
#>  6 linear_caselag3 2021 W02    0.2   20134. quantile       ?     NA
#>  7 linear_caselag3 2021 W02    0.25  20181. quantile       ?     NA
#>  8 linear_caselag3 2021 W02    0.3   20406. quantile       ?     NA
#>  9 linear_caselag3 2021 W02    0.35  20629. quantile       ?     NA
#> 10 linear_caselag3 2021 W02    0.4   20634. quantile       ?     NA
#> # … with 86 more rows
```

We can use the forecasts of incidence to infer cumulative targets. The
`ts_forecast()` function is flexible enough to switch methods
internally, requiring a “inc\_forecast” parameter if either “cdeaths” or
“ccases” is passed as the “outcome” for the forecast:

``` r
## generate cumulative forecasts
cdeaths_forecast <- ts_forecast(outcome = "cdeaths", .data = usa, inc_forecast = ideaths_forecast)
cdeaths_forecast
#> # A tibble: 96 x 8
#>    .model    yweek quantile  value type            ideaths  icases
#>    <chr>    <week>    <dbl>  <dbl> <chr>            <dist>   <dbl>
#>  1 linea… 2021 W02   NA     3.90e5 point N(17632, 1.4e+07)  1.87e6
#>  2 linea… 2021 W02    0.01  3.91e5 quan…                 ? NA     
#>  3 linea… 2021 W02    0.025 3.91e5 quan…                 ? NA     
#>  4 linea… 2021 W02    0.05  3.91e5 quan…                 ? NA     
#>  5 linea… 2021 W02    0.1   3.91e5 quan…                 ? NA     
#>  6 linea… 2021 W02    0.15  3.93e5 quan…                 ? NA     
#>  7 linea… 2021 W02    0.2   3.93e5 quan…                 ? NA     
#>  8 linea… 2021 W02    0.25  3.93e5 quan…                 ? NA     
#>  9 linea… 2021 W02    0.3   3.93e5 quan…                 ? NA     
#> 10 linea… 2021 W02    0.35  3.93e5 quan…                 ? NA     
#> # … with 86 more rows, and 1 more variable: `ifelse(value < recorded_so_far,
#> #   recorded_so_far, value)` <dbl>
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
#> # A tibble: 216 x 7
#>    forecast_date target          target_end_date location type   quantile  value
#>    <date>        <glue>          <date>          <chr>    <chr>     <dbl>  <int>
#>  1 2021-01-11    1 wk ahead cum… 2021-01-16      US       point    NA     390283
#>  2 2021-01-11    1 wk ahead cum… 2021-01-16      US       quant…    0.01  390848
#>  3 2021-01-11    1 wk ahead cum… 2021-01-16      US       quant…    0.025 390848
#>  4 2021-01-11    1 wk ahead cum… 2021-01-16      US       quant…    0.05  390849
#>  5 2021-01-11    1 wk ahead cum… 2021-01-16      US       quant…    0.1   390889
#>  6 2021-01-11    1 wk ahead cum… 2021-01-16      US       quant…    0.15  392575
#>  7 2021-01-11    1 wk ahead cum… 2021-01-16      US       quant…    0.2   392785
#>  8 2021-01-11    1 wk ahead cum… 2021-01-16      US       quant…    0.25  392832
#>  9 2021-01-11    1 wk ahead cum… 2021-01-16      US       quant…    0.3   393057
#> 10 2021-01-11    1 wk ahead cum… 2021-01-16      US       quant…    0.35  393280
#> # … with 206 more rows
```

Last of all, we can validate the submission. The object must be written
to a file and then validated with `validate_forecast()`, which wraps a
python function developed and maintained by the COVID-19 Forecast Hub
organizers. For more information on validating the entry see the
“Submission validation” section below:

``` r
## set up file path for submission file
## forcing the date in the file name and submission conents to be this monday
ffile <- file.path(tempdir(), paste0(this_monday(), "-sigsci-arima.csv"))
submission %>%
  mutate(forecast_date = this_monday()) %>%
  write_csv(ffile)

validate_forecast(ffile)
#> $valid
#> [1] TRUE
#> 
#> $message
#> [1] "no errors"
```

### Evaluating model accuracy

In order to facilitate model selection and evaluation, we included a
wrapper function to generate model accuracy measures for multiple
outcomes and models simultaneously. The `ts_accuracy()` function relies
heavily on `ts_fit()`. As noted above, in basic usage it may not be
necessary to use `ts_fit()` to generate forecasts / submissions.
Furthermore, there are some models for which `ts_fit()` will not work.
Those limitations will apply to `ts_accuracy()` as well.

However, `ts_fit()` can be useful for iterating over outcomes and
arbitrary compatible models:

``` r
ts_funs <- 
  list(
    ARIMA = function(x, .data) model(.data, ARIMA = ARIMA(x, stepwise=FALSE, approximation=FALSE)),
    SES_Additive = function(x, .data) model(.data, SES_additive = ETS(x ~ error("A") + trend("N") + season("N"))),
    SES_Multiplicative = function(x, .data) model(.data, SES_multiplicative = ETS(x ~ error("M") + trend("N") + season("N"))),
    Holt_Additive = function(x, .data) model(.data, Holt_Additive = ETS(x ~ error("A") + trend("A") + season("N"))),
    Holt_Multiplicative = function(x, .data) model(.data, Holt_Multiplicative = ETS(x ~ error("M") + trend("A") + season("N")))
  )

ts_fit(usa, outcomes = "icases", .fun = ts_funs, single = FALSE)
#> $icases
#> $icases$ARIMA
#> # A mable: 1 x 1
#>            ARIMA
#>          <model>
#> 1 <ARIMA(3,2,0)>
#> 
#> $icases$SES_Additive
#> # A mable: 1 x 1
#>   SES_additive
#>        <model>
#> 1 <ETS(A,N,N)>
#> 
#> $icases$SES_Multiplicative
#> # A mable: 1 x 1
#>   SES_multiplicative
#>              <model>
#> 1       <ETS(M,N,N)>
#> 
#> $icases$Holt_Additive
#> # A mable: 1 x 1
#>   Holt_Additive
#>         <model>
#> 1  <ETS(A,A,N)>
#> 
#> $icases$Holt_Multiplicative
#> # A mable: 1 x 1
#>   Holt_Multiplicative
#>               <model>
#> 1        <ETS(M,A,N)>
```

For convenience, the function can return a single `mable` (model tables)
instead of a list if `single = TRUE` and there is only one outcome and
model:

``` r
ts_funs <- 
  list(
    ARIMA = function(x, .data) model(.data, ARIMA = ARIMA(x, stepwise=FALSE, approximation=FALSE))
  )

ts_fit(usa, outcomes = "icases", .fun = ts_funs, single = TRUE)
#> # A mable: 1 x 1
#>            ARIMA
#>          <model>
#> 1 <ARIMA(3,2,0)>
```

The `ts_accuracy()` function accepts a list of models and returns
metrics for each model and outcome(s):

``` r
ts_funs <- 
  list(
    ARIMA = function(x, .data) model(.data, ARIMA = ARIMA(x, stepwise=FALSE, approximation=FALSE)),
    SES_Additive = function(x, .data) model(.data, SES_additive = ETS(x ~ error("A") + trend("N") + season("N"))),
    SES_Multiplicative = function(x, .data) model(.data, SES_multiplicative = ETS(x ~ error("M") + trend("N") + season("N"))),
    Holt_Additive = function(x, .data) model(.data, Holt_Additive = ETS(x ~ error("A") + trend("A") + season("N"))),
    Holt_Multiplicative = function(x, .data) model(.data, Holt_Multiplicative = ETS(x ~ error("M") + trend("A") + season("N")))
  )

ts_accuracy(.data = usa, horizon = 4, outcomes = c("icases","ideaths"), .fun = ts_funs)
#> # A tibble: 10 x 10
#>    .model       .type       ME   RMSE    MAE    MPE  MAPE  MASE     ACF1 outcome
#>    <chr>        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl>    <dbl> <chr>  
#>  1 ARIMA        Test  -280675. 3.15e5 2.81e5 -19.3  19.3    NaN -0.191   icases 
#>  2 SES_additive Test   -14606. 1.68e5 1.39e5  -2.14  9.13   NaN -0.0511  icases 
#>  3 SES_multipl… Test   -14591. 1.68e5 1.39e5  -2.14  9.13   NaN -0.0511  icases 
#>  4 Holt_Additi… Test  -405021. 4.33e5 4.05e5 -27.5  27.5    NaN -0.152   icases 
#>  5 Holt_Multip… Test   189928. 2.49e5 1.90e5  11.6  11.6    NaN -0.0754  icases 
#>  6 ARIMA        Test     2810. 4.79e3 3.36e3  12.9  16.4    NaN  0.208   ideaths
#>  7 SES_additive Test     1901. 3.05e3 2.47e3   8.75 12.4    NaN  0.0263  ideaths
#>  8 SES_multipl… Test     3321. 4.09e3 3.32e3  16.5  16.5    NaN  0.0263  ideaths
#>  9 Holt_Additi… Test    -3672. 4.16e3 3.67e3 -20.5  20.5    NaN -0.230   ideaths
#> 10 Holt_Multip… Test     1494. 2.71e3 2.22e3   6.61 11.3    NaN -0.00602 ideaths
```

> **CAUTION**: The list passed to “.fun” in either `ts_accuracy()` or
> `ts_fit()` *must* include functions formatted exactly as the example
> above. These functions should all wrap `fable::model` with the outcome
> as “x” and data passed in as “.data”.

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

    validate_forecast("scratch/fable-submission-mockup-allmetrics-forecasts/2021-01-04-sigsci-ts.csv")
    validate_forecast("scratch/fable-submission-mockup-allmetrics-forecasts/2021-01-05-sigsci-ts.csv")

# Experimental: One-step pipeline

The **experimental** `forecast_pipeline()` function will run everything
above, and return a list with models, forecasts, the formatted
submission, and a suggested filename relative to the project directory
to save the submission. See the examples in `?forecast_pipeline()`. For
now this function only works on Mondays!

1.  Get data (national level from NYT by default)
2.  Fit incident case and incident death models (ARIMA and lagged TSLM
    respectively)
3.  Get future case data to create the incident death forecast
4.  Create the incident death forecast based on this new data
5.  Prepare submission format
6.  Suggest a submission filename
7.  Return all resulting objects to a list.

``` r
# Run the forecast pipeline. See ?forecast_pipeline
myforecast <- forecast_pipeline()

# Look at the submission and the suggested filename.
myforecast$submission
myforecast$submission_filename

# Write submission to file
readr::write_csv(myforecast$submission, file=myforecast$submission_filename)

# Validate submission
validate_forecast(myforecast$submission_filename)
```
