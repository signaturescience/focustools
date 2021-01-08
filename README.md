
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
#>  1 arima  2021 W02    0.01   908628. quantile      ?
#>  2 arima  2021 W02    0.025  971523. quantile      ?
#>  3 arima  2021 W02    0.05  1006538. quantile      ?
#>  4 arima  2021 W02    0.1   1064968. quantile      ?
#>  5 arima  2021 W02    0.15  1074068. quantile      ?
#>  6 arima  2021 W02    0.2   1088479. quantile      ?
#>  7 arima  2021 W02    0.25  1112987. quantile      ?
#>  8 arima  2021 W02    0.3   1115117. quantile      ?
#>  9 arima  2021 W02    0.35  1119236. quantile      ?
#> 10 arima  2021 W02    0.4   1122745. quantile      ?
#> # … with 86 more rows

## need to get future cases to pass to ideaths forecast
future_cases <- ts_futurecases(usa, icases_forecast, horizon = 4)
# Forecast incident deaths based on best guess for cases
ideaths_forecast <- ts_forecast(fit.ideaths,  outcome = "ideaths", new_data = future_cases)
ideaths_forecast
#> # A tibble: 96 x 7
#>    .model             yweek quantile  value type     ideaths icases
#>    <chr>             <week>    <dbl>  <dbl> <chr>     <dist>  <dbl>
#>  1 linear_caselag3 2021 W02    0.01   9464. quantile       ?     NA
#>  2 linear_caselag3 2021 W02    0.025  9464. quantile       ?     NA
#>  3 linear_caselag3 2021 W02    0.05   9465. quantile       ?     NA
#>  4 linear_caselag3 2021 W02    0.1    9505. quantile       ?     NA
#>  5 linear_caselag3 2021 W02    0.15  11401. quantile       ?     NA
#>  6 linear_caselag3 2021 W02    0.2   11751. quantile       ?     NA
#>  7 linear_caselag3 2021 W02    0.25  11829. quantile       ?     NA
#>  8 linear_caselag3 2021 W02    0.3   12168. quantile       ?     NA
#>  9 linear_caselag3 2021 W02    0.35  12228. quantile       ?     NA
#> 10 linear_caselag3 2021 W02    0.4   12288. quantile       ?     NA
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
#>  1 linea… 2021 W02   NA     3.82e5 point N(16412, 1.4e+07)  1.13e6
#>  2 linea… 2021 W02    0.01  3.75e5 quan…                 ? NA     
#>  3 linea… 2021 W02    0.025 3.75e5 quan…                 ? NA     
#>  4 linea… 2021 W02    0.05  3.75e5 quan…                 ? NA     
#>  5 linea… 2021 W02    0.1   3.75e5 quan…                 ? NA     
#>  6 linea… 2021 W02    0.15  3.77e5 quan…                 ? NA     
#>  7 linea… 2021 W02    0.2   3.77e5 quan…                 ? NA     
#>  8 linea… 2021 W02    0.25  3.77e5 quan…                 ? NA     
#>  9 linea… 2021 W02    0.3   3.78e5 quan…                 ? NA     
#> 10 linea… 2021 W02    0.35  3.78e5 quan…                 ? NA     
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
#>  1 2021-01-08    1 wk ahead cum… 2021-01-16      US       point    NA     381907
#>  2 2021-01-08    1 wk ahead cum… 2021-01-16      US       quant…    0.01  374959
#>  3 2021-01-08    1 wk ahead cum… 2021-01-16      US       quant…    0.025 374959
#>  4 2021-01-08    1 wk ahead cum… 2021-01-16      US       quant…    0.05  374960
#>  5 2021-01-08    1 wk ahead cum… 2021-01-16      US       quant…    0.1   375000
#>  6 2021-01-08    1 wk ahead cum… 2021-01-16      US       quant…    0.15  376896
#>  7 2021-01-08    1 wk ahead cum… 2021-01-16      US       quant…    0.2   377246
#>  8 2021-01-08    1 wk ahead cum… 2021-01-16      US       quant…    0.25  377324
#>  9 2021-01-08    1 wk ahead cum… 2021-01-16      US       quant…    0.3   377663
#> 10 2021-01-08    1 wk ahead cum… 2021-01-16      US       quant…    0.35  377723
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
#> [1] FALSE
#> 
#> $message
#> [1] "target_end_date was not the expected Saturday. forecast_date=2021-01-04, target_end_date=2021-01-16. exp_target_end_date=2021-01-09, row=['2021-01-04', '1 wk ahead cum death', '2021-01-16', 'US', 'point', 'NA', '381907']\ntarget_end_date was not the expected Saturday. forecast_date=2021-01-04, target_end_date=2021-01-16. exp_target_end_date=2021-01-09, row=['2021-01-04', '1 wk ahead cum death', '2021-01-16', 'US', 'quantile', '0.01', '374959']\ntarget_end_date was not the expected Saturday. forecast_date=2021-01-04, target_end_date=2021-01-16. exp_target_end_date=2021-01-09, row=['2021-01-04', '1 wk ahead cum death', '2021-01-16', 'US', 'quantile', '0.025', '374959']\ntarget_end_date was not the expected Saturday. forecast_date=2021-01-04, target_end_date=2021-01-16. exp_target_end_date=2021-01-09, row=['2021-01-04', '1 wk ahead cum death', '2021-01-16', 'US', 'quantile', '0.05', '374960']\ntarget_end_date was not the expected Saturday. forecast_date=2021-01-04, target_end_date=2021-01-16. exp_target_end_date=2021-01-09, row=['2021-01-04', '1 wk ahead cum death', '2021-01-16', 'US', 'quantile', '0.1', '375000']\ntarget_end_date was not the expected Saturday. forecast_date=2021-01-04, target_end_date=2021-01-16. exp_target_end_date=2021-01-09, row=['2021-01-04', '1 wk ahead cum death', '2021-01-16', 'US', 'quantile', '0.15', '376896']\ntarget_end_date was not the expected Saturday. forecast_date=2021-01-04, target_end_date=2021-01-16. exp_target_end_date=2021-01-09, row=['2021-01-04', '1 wk ahead cum death', '2021-01-16', 'US', 'quantile', '0.2', '377246']\ntarget_end_date was not the expected Saturday. forecast_date=2021-01-04, target_end_date=2021-01-16. exp_target_end_date=2021-01-09, row=['2021-01-04', '1 wk ahead cum death', '2021-01-16', 'US', 'quantile', '0.25', '377324']\ntarget_end_date was not the expected Saturday. forecast_date=2021-01-04, target_end_date=2021-01-16. exp_target_end_date=2021-01-09, row=['2021-01-04', '1 wk ahead cum death', '2021-01-16', 'US', 'quantile', '0.3', '377663']\ntarget_end_date was not the expected Saturday. forecast_date=2021-01-04, target_end_date=2021-01-16. exp_target_end_date=2021-01-09, row=['2021-01-04', '1 wk ahead cum death', '2021-01-16', 'US', 'quantile', '0.35', '377723']\ntarget_end_date was ..."
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
#> 1 <ARIMA(0,1,4)>
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
#> 1 <ARIMA(0,1,4)>
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
#>    .model       .type       ME   RMSE    MAE     MPE  MAPE  MASE    ACF1 outcome
#>    <chr>        <chr>    <dbl>  <dbl>  <dbl>   <dbl> <dbl> <dbl>   <dbl> <chr>  
#>  1 ARIMA        Test  -418859. 4.85e5 4.19e5 -32.0   32.0    NaN  0.0550 icases 
#>  2 SES_additive Test  -152790. 1.89e5 1.53e5 -11.8   11.8    NaN -0.419  icases 
#>  3 SES_multipl… Test  -152775. 1.89e5 1.53e5 -11.8   11.8    NaN -0.419  icases 
#>  4 Holt_Additi… Test  -543205. 6.06e5 5.43e5 -41.2   41.2    NaN  0.0480 icases 
#>  5 Holt_Multip… Test    51744. 1.30e5 1.08e5   3.07   7.68   NaN -0.360  icases 
#>  6 ARIMA        Test     1021. 1.99e3 1.57e3   5.84   9.27   NaN -0.0725 ideaths
#>  7 SES_additive Test      112. 1.49e3 1.48e3  -0.116  8.77   NaN -0.692  ideaths
#>  8 SES_multipl… Test     1532. 2.14e3 1.62e3   8.36   8.97   NaN -0.692  ideaths
#>  9 Holt_Additi… Test    -5461. 6.49e3 5.46e3 -34.1   34.1    NaN -0.0461 ideaths
#> 10 Holt_Multip… Test     -295. 1.62e3 1.56e3  -2.60   9.48   NaN -0.619  ideaths
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
be checked as follows:

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

You can install these dependencies by running the following:

    pip3 install -r inst/validation/requirements.txt

In order to run a very similar check using R, we have developed a
wrapper to the [`validate_quantile_csv_file()` function from the
`zoltpy`
module](https://github.com/reichlab/zoltpy/blob/master/zoltpy/covid19.py#L75-L93),
which drives the `validate_single_forecast_file.py` script above. To use
the wrapper function in R:

    validate_forecast("scratch/fable-submission-mockup-allmetrics-forecasts/2021-01-04-sigsci-ts.csv")
    validate_forecast("scratch/fable-submission-mockup-allmetrics-forecasts/2021-01-05-sigsci-ts.csv")
