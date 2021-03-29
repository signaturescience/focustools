
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
