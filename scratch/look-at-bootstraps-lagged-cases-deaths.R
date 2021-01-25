library(dplyr)
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
  filter(location %in% c("US", stringr::str_pad(1:56, width=2, pad="0"))) %>%
  make_tsibble() %>%
  filter(monday>"2020-03-01")
stopifnot(length(unique(usafull$location))==52L)

horizon <- 4

mylocs <- unique(usafull$location)
mylocs <- c("US", "48", "51", "19")

usa <- filter(usafull, location=="06")

# fit a model, forecast, boostrap
fits.icases <-  usa %>% model(arima = ARIMA(icases, stepwise=FALSE, approximation=FALSE))
forc.icases <- forecast(fits.icases, h=horizon)
boot.icases <- generate(fits.icases, h=4, times=10, bootstrap=TRUE)

# Look at the forecast and bootstraps
forc.icases
forc.icases %>% filter(yweek==min(forc.icases$yweek))
boot.icases %>% filter(yweek==min(boot.icases$yweek)) %>% arrange(as.integer(.rep))
boot.icases %>% filter(yweek==min(boot.icases$yweek)) %>% as_tibble() %>% group_by(location) %>% summarize(quibble(.sim)) %>% filter(round(q, 4) %in% c(.25, .5, .75))

# get future cases
future_cases <-
  forc.icases %>%
  group_by(location) %>%
  arrange(yweek) %>%
  select(location, yweek, icases=.mean) %>%
  ungroup() %>%
  right_join(tsibble::new_data(usa, n=horizon), by=c("location", "yweek")) %>%
  print()

# Fit a model, forecast, bootstrap deaths
fits.ideaths <- usa %>% model(linear_caselag3 = TSLM(ideaths ~ lag(icases, 3)))
forc.ideaths <- forecast(fits.ideaths, new_data = future_cases)
boot.ideaths <- generate(fits.ideaths, new_data=future_cases, times=10, bootstrap=TRUE)

# Look at the forecast and bootstraps
forc.ideaths %>% filter(yweek==min(forc.ideaths$yweek))
boot.ideaths %>% filter(yweek==min(boot.ideaths$yweek)) %>% arrange(as.integer(.rep))
boot.ideaths %>% filter(yweek==min(boot.ideaths$yweek)) %>% as_tibble() %>% group_by(location) %>% summarize(quibble(.sim)) %>% filter(round(q, 4) %in% c(.25, .5, .75))



#############################################################################################################
#### Aside, why wouldn't this work? Using a six week lag but only forecasting 2 weeks, shoudld have the data?
fits.ideaths.6weeklag <- usa %>% model(linear_caselag6 = TSLM(ideaths ~ lag(icases, 6)))
forc.ideaths.6weeklag <- forecast(fits.ideaths, h=2)
rm(fits.ideaths.6weeklag)
#############################################################################################################

# What if we actually try writing this into the data itself, could we use a horizon instead of new data?
usa <- usa %>% mutate(icases.lag=lag(icases, 3, default=0L))
fits.ideaths <- usa %>% model(linear_caselag = TSLM(ideaths ~ icases.lag))
forc.ideaths <- forecast(fits.ideaths, h=horizon)

# Try writing lagcase data into the data again, but this time use the size of
# the horizon so you don't have to use future predictions, you can use actual
# recorded data from the weeks leading up to forecast
usa <- usa %>% mutate(icases.lag=lag(icases, n=horizon, default=0L))
# The model is directly on the lagged cases variable you just created
fits.ideaths <- usa %>% model(linear_caselag3 = TSLM(ideaths ~ icases.lag))
# Your "future" cases are actually just those last few weeks of recorded data
future_cases <- usa %>%
  group_by(location) %>%
  arrange(yweek) %>%
  slice((n()-horizon+1):n()) %>%
  select(location, yweek, icases.lag=icases) %>%
  print()
forc.ideaths <- forecast(fits.ideaths, new_data=future_cases)

# Look at the forecast and bootstraps
forc.ideaths %>% filter(yweek==min(forc.ideaths$yweek))
boot.ideaths %>% filter(yweek==min(boot.ideaths$yweek)) %>% arrange(as.integer(.rep))
boot.ideaths %>% filter(yweek==min(boot.ideaths$yweek)) %>% as_tibble() %>% group_by(location) %>% summarize(quibble(.sim)) %>% filter(round(q, 4) %in% c(.25, .5, .75))

