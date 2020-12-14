## initial exploration with MARS model and lagged predictor
## https://doi.org/10.2307/2290499
## https://cran.r-project.org/web/packages/earth/index.html
## NOTE: YMMV until the epiweeks format is fixed
## TODO: look into using tidymodels to implement this model (MARS is one of the parsnip engines ...)
## TODO: get case counts from JHU instead of NYT ?
## TODO: add logic to split predictions into binned quantiles
## TODO: re-implement loop for predicting horizons as purrr::map_df() for legibility ?
## TODO: check that complete.cases() doesnt create gaps between weeks when prepping the original case count tibble
## TODO: find better values for MARS model params ... via tuning / grid search ?
library(tidyverse)
library(earth)
library(lubridate)

## read in county level cumulative data from NYT
counties <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

# add epiyear and epiweek
counties <-
  counties %>%
  mutate(week=week(date), .after=date) %>%
  mutate(epiyear=MMWRweek::MMWRweek(date)$MMWRyear, .after=week) %>%
  mutate(epiweek=MMWRweek::MMWRweek(date)$MMWRweek, .after=epiyear)

## create tibble with number of cases, lagged cases, deaths, lagged deaths by week
usa <-
  counties %>%
  mutate(week = week(date)) %>%
  group_by(week) %>%
  summarise(cases = sum(cases, na.rm = TRUE),
            deaths = sum(deaths, na.rm = TRUE)) %>%
  ## cases are originally recorded as cumulative cases
  mutate(cases = cases - lag(cases),
         deaths = deaths - lag(deaths)) %>%
  mutate(lag_cases = lag(cases),
         lag_deaths = lag(deaths)) %>%
  ## exclude current week as the case/death tallies may be incomplete
  filter(week != week(Sys.Date())) %>%
  ## remove any weeks with NA predictions
  filter(complete.cases(.)) %>%
  ## MAKE SURE DATA IS ARRANGED WITH WEEK ASCENDING
  ## critical given that we are using lag() for predictor
  arrange(week)

usa

# get usa data straight from nyt usa summarized data
usa2 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv")
# add epiyear and epiweek
usa2 <-
  usa2 %>%
  mutate(week=week(date), .after=date) %>%
  mutate(epiyear=MMWRweek::MMWRweek(date)$MMWRyear, .after=week) %>%
  mutate(epiweek=MMWRweek::MMWRweek(date)$MMWRweek, .after=epiyear)
# calculate incident cases, deaths, and exclude today if it's in there
usa2 <-
  usa2 %>%
  mutate(cases_inc=cases-lag(cases, default=0L), deaths_inc=deaths-lag(deaths, default=0L)) %>%
  filter(date!=today())

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
  cases ~ lag_cases,
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
  mutate(date = as.Date(paste("2020", week, 1, sep="-"), "%Y-%U-%u")) %>%
  ggplot() +
  ## prediction interval for forecasts
  geom_ribbon(aes(x = date, y = fit, ymin = lwr, ymax = upr), fill = "lightpink", alpha = 0.5) +
  ## line for observed
  geom_line(aes(date, cases), lwd = 1.25) +
  ## line for predicted
  geom_line(aes(date, fit), col = "firebrick", lwd = 1.25) +
  theme_minimal() +
  labs(x = "Date", y = "Weekly COVID-19 Incidence")


#####################################################
## now try looking into the future
## in this case we'll use all of the available data to train
usa_train <- usa

## refit the model
usa_mars_fit <- earth(
  cases ~ lag_cases,
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
  if(i == 1) oneweek_horizon <- tail(usa_train$cases, 1)

  ## create the input tibble to be passed to predict()
  ## use the last week number from training plus the horizon index (e.g. 2 weeks ahead of last week = 45 => 47)
  ## lag_cases is set at either the last value in cases (if first horizon index)
  ## or the fitted value ("fit") from the prediction for the previous horizon week (see below)
  usa_future <- tibble(week = tail(usa_train$week, 1) + i, lag_cases = oneweek_horizon)

  ## predict
  tmp_res <- predict(usa_mars_fit, newdata = usa_future, interval = "pint")

  ## add the week to the prediction results
  tmp_res$week <- tail(usa_train$week, 1) + i

  oneweek_horizon <- tmp_res$fit

  res[[i]] <- tmp_res


}

## bind result list items together as data.frame
do.call("rbind", res) %>%
  ## bind that data frame to the training data
  bind_rows(usa_train) %>%
  ## crate date column from week number
  mutate(date = as.Date(paste("2020", week, 1, sep="-"), "%Y-%U-%u")) %>%
  ggplot() +
  ## prediction interval for forecasts
  geom_ribbon(aes(x = date, y = fit, ymin = lwr, ymax = upr), fill = "lightpink", alpha = 0.5) +
  ## line for observed
  geom_line(aes(date, cases), lwd = 1.25) +
  ## line for predicted
  geom_line(aes(date, fit), col = "firebrick", lwd = 1.25) +
  theme_minimal() +
  labs(x = "Date", y = "Weekly COVID-19 Incidence")
