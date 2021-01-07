## initial exploration with MARS model and lagged predictor (cases)
## https://doi.org/10.2307/2290499
## https://cran.r-project.org/web/packages/earth/index.html
## TODO: look into using tidymodels to implement this model (MARS is one of the parsnip engines ...)
## TODO: re-implement loop for predicting horizons as purrr::map_df() for legibility ?
## TODO: check that complete.cases() doesnt create gaps between weeks when prepping the original case count tibble
## TODO: find better values for MARS model params ... via tuning / grid search ?
library(tidyverse)
library(earth)
library(lubridate)

source("utils/get_data.R")

usa <-
  get_cases(source = "jhu") %>%
  rename(week = epiweek) %>%
  filter(week != lubridate::week(Sys.Date())) %>%
  mutate(lag_cases = lag(icases, default = 0L))

## evaluate how well the model fits
## create separate training / test sets
## train on everything *except* last 4 weeks
usa_train <-
  usa %>%
  slice(-tail(row_number(), 4))

## test on last 4 weeks
usa_test <-
  usa %>%
  slice(tail(row_number(), 4))

## "default" params for earth()
usa_mars_fit <- earth(
  icases ~ lag_cases,
  data = usa_train,
  nfold = 10,
  ncross = 10,
  keepxy = TRUE,
  varmod.method = "lm"
)


## predict and plot
predict(usa_mars_fit, newdata = usa_test, interval = "pint") %>%
  ## prediction interval tibble drops the week
  ## make sure to add that column back in
  mutate(week = usa_test$week) %>%
  ## join the data together by "week" column
  ## NOTE: the right-hand side is the training and testing stacked on top
  ## this allows us to see the predicted vs observed for the last 4 weeks
  right_join(rbind(usa_test, usa_train)) %>%
  mutate(date = as.Date(paste(epiyear, week, 1, sep="-"), "%Y-%U-%u")) %>%
  ggplot() +
  ## prediction interval for forecasts
  geom_ribbon(aes(x = date, y = fit, ymin = lwr, ymax = upr), fill = "lightpink", alpha = 0.5) +
  ## line for observed
  geom_line(aes(date, icases), lwd = 1.25) +
  ## line for predicted
  geom_line(aes(date, fit), col = "firebrick", lwd = 1.25) +
  theme_minimal() +
  scale_y_continuous(labels=scales::number_format()) +
  labs(x = "Date", y = "Weekly COVID-19 Incidence")


#####################################################
## now try looking into the future
## in this case we'll use all of the available data to train
usa_train <- usa

## refit the model
usa_mars_fit <- earth(
  icases ~ lag_cases,
  data = usa_train,
  nfold = 10,
  ncross = 10,
  keepxy = TRUE,
  varmod.method = "lm"
)

## what is the horizon (i.e. how far out should we try to predict in weeks?)
horizon <- 4
## empty list to store results
res <- list()
## iterate over each week in the horizon
## have to do it one week at a time so we can get the use the previous weeks prediction as the cases for lag_cases
## only exception is first week because we can use the last week of observed data to derive lagged cases
for(i in 1:horizon) {

  ## if first week in horizon then use the last of observed data
  if(i == 1) oneweek_horizon <- tail(usa_train$icases, 1)

  ## create the input tibble to be passed to predict()
  ## use the last week number from training plus the horizon index (e.g. 2 weeks ahead of last week = 45 => 47)
  ## lag_cases is set at either the last value in cases (if first horizon index)
  ## or the fitted value ("fit") from the prediction for the previous horizon week (see below)
  whead <- tail(usa_train$week, 1) + i
  epiyear <- tail(usa_train$epiyear, 1)
  ## if the horizon spans the new year then account for it ...
  ## subtract 52 from week ahead to get actual week
  ## and bump the year to the next
  if(whead > 52) {
    whead <- whead - 52
    epiyear <- epiyear + 1
  }

  usa_future <- tibble(week = whead, lag_cases = oneweek_horizon)

  ## predict
  tmp_res <- predict(usa_mars_fit, newdata = usa_future, interval = "pint")

  ## add the week to the prediction results
  tmp_res$week <- whead
  ## add the year to the prediction results
  tmp_res$epiyear <- epiyear

  oneweek_horizon <- tmp_res$fit

  res[[i]] <- tmp_res


}

## bind result list items together as data.frame
do.call("rbind", res) %>%
  ## bind that data frame to the training data
  bind_rows(usa_train) %>%
  ## crate date column from week number
  mutate(date = as.Date(paste(epiyear, week, 1, sep="-"), "%Y-%U-%u")) %>%
  ggplot() +
  ## prediction interval for forecasts
  geom_ribbon(aes(x = date, y = fit, ymin = lwr, ymax = upr), fill = "lightpink", alpha = 0.5) +
  ## line for observed
  geom_line(aes(date, icases), lwd = 1.25) +
  ## line for predicted
  geom_line(aes(date, fit), col = "firebrick", lwd = 1.25) +
  theme_minimal() +
  scale_y_continuous(labels=scales::number_format()) +
  labs(x = "Date", y = "Weekly COVID-19 Incidence")


## try to get quantiles for 1w ahead
## first week in horizon then use the last of observed data
oneweek_horizon <- tail(usa_train$icases, 1)

## create the input tibble to be passed to predict()
## use the last week number from training plus the horizon index (e.g. 2 weeks ahead of last week = 45 => 47)
## lag_cases is set at either the last value in cases (if first horizon index)
## or the fitted value ("fit") from the prediction for the previous horizon week (see below)
whead <- tail(usa_train$week, 1) + 1
epiyear <- tail(usa_train$epiyear, 1)
## if the horizon spans the new year then account for it ...
## subtract 52 from week ahead to get actual week
## and bump the year to the next
if(whead > 52) {
  whead <- whead - 52
  epiyear <- epiyear + 1
}

usa_future <- tibble(week = whead, lag_cases = oneweek_horizon)

## predict
## create standard error (TODO: check with CHL and SDT about SE for prediction interval formula here ...)
pred <-
  predict(usa_mars_fit, newdata = usa_future, interval = "pint") %>%
  dplyr::mutate(se = (upr - fit) / qt(0.975, nrow(usa_train)))

sims <- rnorm(1000, mean = pred$fit, sd = pred$se)
quantile(sims, probs = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99))
