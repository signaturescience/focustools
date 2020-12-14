# TODO: get more info on seasonality: https://facebook.github.io/prophet/docs/seasonality,_holiday_effects,_and_regressors.html

# Setup -------------------------------------------------------------------

library(tidyverse)
library(prophet) # must install with type="source"
theme_set(theme_classic())

# Define some holidays
holidays <-
  tribble(
    ~holiday, ~ds,
    "Labor Day", "2020-09-07",
    "Thanksgiving", "2020-11-26",
    "Christmas", "2020-12-25"
  ) %>%
  mutate(ds=as.Date(ds))

# load data usa=daily, usaw=weekly
source(here::here("utils/get-us-data.R"))

# add variable name that prophet needs. in this case, use incident cases.
usa <- usa %>%
  mutate(ds=date, y=icases)

# how many weeks do you want to forecast?
n_weeks_to_forecast <- 2L

# changepoints
changepoints <- c(
  "2020-04-09",
  "2020-06-15",
  "2020-07-17",
  "2020-09-17"
) %>% as.Date()



# Predict daily incident cases over the next four weeks -------------------

# fit prophet model
m <- prophet(usa,
             yearly.seasonality = FALSE,
             weekly.seasonality = TRUE,
             daily.seasonality = FALSE,
             holidays=holidays)

# forecast
forecast <-
  m %>%
  make_future_dataframe(periods=n_weeks_to_forecast*7) %>%
  predict(m, .) %>%
  as_tibble()

# Take a look
forecast %>%
  as_tibble() %>%
  select(ds, yhat, yhat_lower, yhat_upper) %>%
  tail()

# Visualize
plot(m, forecast) +
  theme_classic() +
  scale_y_continuous(labels=scales::number_format()) +
  labs(x="Date", y="Daily incidence")



# Forecast retroactively --------------------------------------------------

# remove four weeks of data from the end
usa_train <-
  usa %>%
  slice(-tail(row_number(), n_weeks_to_forecast*7))

# fit prophet model
m <- prophet(usa_train,
             yearly.seasonality = FALSE,
             weekly.seasonality = TRUE,
             daily.seasonality = FALSE,
             holidays=holidays)

# forecast
forecast <-
  m %>%
  make_future_dataframe(periods=n_weeks_to_forecast*7) %>%
  predict(m, .) %>%
  as_tibble()

# visualize
# plot(m, forecast)

# join the forecasted data with the real data
comb <- forecast %>%
  mutate(ds=as.Date(ds)) %>%
  select(ds, yhat, yhat_lower, yhat_upper) %>%
  inner_join(usa %>% select(ds, y), ., by="ds") %>%
  rename(date=ds)

# summarize to weekly
wcomb <-
  comb %>%
  mutate(epiweek=MMWRweek::MMWRweek(date)$MMWRweek, .after=date) %>%
  group_by(epiweek) %>%
  summarize(across(c(y, yhat, yhat_lower, yhat_upper), sum, na.rm=TRUE), .groups="drop") %>%
  head(-1)

# visualize against actual
wcomb %>%
  rename(Actual=y, Forecasted=yhat) %>%
  gather(key, value, Actual, Forecasted) %>%
  ggplot(aes(epiweek, value)) +
  geom_line(aes(col=key)) +
  geom_ribbon(aes(ymin=yhat_lower, ymax=yhat_upper), alpha=.2) +
  geom_vline(xintercept=max(wcomb$epiweek)-n_weeks_to_forecast, alpha=.7, lty=2) +
  scale_y_continuous(labels=scales::number_format()) +
  labs(x="Date", y="Weekly incidence", title="Forecast for weekly incident cases",
       subtitle=glue::glue("Retroactive forecast window: {n_weeks_to_forecast} weeks (indicated by dotted vertical line)"),
       color=NULL) +
  theme(legend.position="bottom")
