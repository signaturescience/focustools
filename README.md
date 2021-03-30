
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
devtools::install_github("signaturescience/focustools", build_vignettes = FALSE)
```

## Usage

To get started, see the package vignette:

``` r
vignette("focustools")
```

## COVID-19 Forecast Hub submissions

`focustools` was originally developed to generate forecasts of COVID-19
targets in the United States. The package includes functionality to
prepare forecast output for submission to the [COVID-19 Forecast
Hub](https://covid19forecasthub.org/). The `focustools` GitHub
repository includes a `submission/` directory with select forecast
submission files. That same directory tracks a pipeline R script that
does the following:

1.  Retrieves historical observed count data (national level from JHU by
    default)
2.  Fits incident case and incident death models
3.  Forecasts future case data to facilitate the incident death forecast
4.  Creates the incident death forecast based on this new data
5.  Generates cumulative death forecast by summarizing incident death
    forecast
6.  Prepares submission format
7.  Savse the submission to
    `submission/TEAM-MODEL/YYYY-MM-DD-TEAM-MODEL.csv`
8.  Validates the submission file
