# setup -------------------------------------------------------------------

suppressPackageStartupMessages(suppressWarnings(library(tidyverse)))
suppressPackageStartupMessages(suppressWarnings(library(focustools)))
suppressPackageStartupMessages(suppressWarnings(library(fabletools)))
suppressPackageStartupMessages(suppressWarnings(library(fable)))

# get case, death, and hosp data
usac <-  get_cases(source="jhu",  granularity = "national")
usad <- get_deaths(source="jhu",  granularity = "national")
usah <- get_hosp()
usa <-
  usac %>%
  inner_join(usad, by = c("epiyear", "epiweek", "location")) %>%
  inner_join(usah, by=c("epiyear", "epiweek", "location")) %>%
  make_tsibble(chop=TRUE) %>%
  select(location, everything())
tail(usa)

# What's your forecast horizon in weeks?
horizon <- 4

#eda
# ggplot(usa, aes(monday, ihosp)) + geom_line()
# usa %>% gather(key, value, icases:ihosp) %>% ggplot(aes(monday, value)) + geom_line() + facet_wrap(~key, scales="free_y")



# What previously and still works -----------------------------------------

# fit case model
fit.icases <- usa %>% model(arima = ARIMA(icases, stepwise=FALSE, approximation=FALSE))

# fit death model how we've done previously
fit.ideaths <- usa %>% model(linear_caselag = TSLM(ideaths ~ lag(icases, 3)))

# forecast incident cases
forecast.icases <- ts_forecast(fit.icases, outcome = "icases", horizon = horizon)

# get future cases to feed into death forecast
# TODO: explicitly state the join by in the function below to silence the message
futurecases <- ts_futurecases(usa, forecast.icases, horizon = horizon)

# Forecast incident deaths based on best guess for cases
forecast.ideaths <- ts_forecast(fit.ideaths,  outcome = "ideaths", new_data = futurecases)
forecast.ideaths %>% filter(type=="point") %>% select(yweek, value)

# cool.


# Introduce hosp data -----------------------------------------------------

# First let's try something simple. Probably not a good model, but let's model
# death on a long enough lag so that we have the actual data and we don't have
# to forecast hospitalizations. E.g. 6 week lag, max 4 week horizon.
fit.ideaths__HOSP <- usa %>% model(linear_caselag = TSLM(ideaths ~ lag(ihosp, 6)))

# But when trying to forecast that model this isn't working. It's asking for new
# data. I'd have thought looking back 6 weeks at a 4 week horizon would have
# been fine to use the actual recorded data we have.
forecast.ideaths__HOSP <- ts_forecast(fit.ideaths, outcome="ideaths", horizon=horizon)

# Let's manually make that new data by first pulling out 4 weeks worth of 6-week-lagged data
lagged_hosp_vector <-
  usa %>%
  mutate(hlag=lag(ihosp, 6)) %>%
  pull() %>%
  tail(horizon)
lagged_hosp_vector
newdata__HOSP <- tsibble::new_data(usa, n=4) %>% mutate(ihosp=lagged_hosp_vector)
newdata__HOSP

# Now try to fit that model again with newdata instead of horizon
forecast.ideaths__HOSP <- ts_forecast(fit.ideaths__HOSP, outcome="ideaths", new_data=newdata__HOSP)
forecast.ideaths__HOSP %>% filter(type=="point") %>% select(yweek, value)

# That worked but the numbers are *way* lower than they should be. Compare to
# our original model.
forecast.ideaths %>% filter(type=="point") %>% select(yweek,value)

# What if we used an approach similar to above where we forecast the
# hospitalization data based on either ARIMA or a lag of incident cases, and use
# that in the model (either alone or as a "covariate")?

# First let's forecast hospitalizations using ARIMA
fit.ihosp <- usa %>% model(arima=ARIMA(ihosp, stepwise=FALSE, approximation=FALSE))

# Then forecast hospitalizations
forecast.ihosp <- ts_forecast(fit.ihosp, outcome="ihosp", horizon=horizon)
forecast.ihosp %>% filter(type=="point") %>% select(yweek,value)

#... well, that didn't work. All the values are the same.

# Let's try forecasting hospitalizations based on a lag of cases
fit.ihosp <- usa %>% model(linear_caselag = TSLM(ihosp ~ lag(icases, 2)))
forecast.ihosp <- ts_forecast(fit.ihosp, outcome="ihosp", new_data=futurecases)

ihosp_forecast <- ts_forecast(fit.ihosp, outcome="ihosp", new_data=futurecases)
ihosp_forecast %>% filter(type=="point") %>% select(yweek, value)

# That actually looks reasonable? Looking at the last few weeks of the data:
tail(usa, 4) %>% select(yweek, ihosp)

# OK, now let's get future hospitalizations to use for the death forecast
futurehosp <- ts_futurecases(usa, ihosp_forecast, horizon=4)
futurehosp

# That seemed to work as expected, but we need to change the name becauses
# icases is hard-coded into the function for now.
futurehosp <- futurehosp %>% rename(ihosp=icases)
futurehosp

#OK, let's create another ideath model based on ihosp (2 week lag on hosp)
fit.ideaths__HOSP <- usa %>% model(linear_caselag = TSLM(ideaths ~ lag(ihosp, 2)))
forecast.ideaths__HOSP <- ts_forecast(fit.ideaths__HOSP, outcome = "ideaths", new_data = futurehosp)

# compare those to death data forecasted just from case data, lower unfortunately, with a big dip in week 2 ahead.
forecast.ideaths__HOSP %>% filter(type=="point") %>% select(yweek, value)
forecast.ideaths %>% filter(type=="point") %>% select(yweek, value)

