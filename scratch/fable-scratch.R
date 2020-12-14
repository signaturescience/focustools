# https://fable.tidyverts.org/index.html
# https://tidyverts.github.io/tidy-forecasting-principles/

library(tidyverse)
library(fable)
library(lubridate)
source(here::here("utils/get-us-data.R"))

f <-
  usa %>%
  as_tsibble(index=date) %>%
  mutate(logicases=log(icases+1)) %>%
  model(
    ets=ETS(logicases),
    arima=ARIMA(logicases),
    snaive=SNAIVE(logicases)
  ) %>%
  forecast(h="8 weeks") %>%
  print() %>%
  as_tibble() %>%
  mutate(icases=exp(.mean)) %>%
  select(date, .model, icases) %>%
  bind_rows(usa)

f %>%
  filter(date>today()-weeks(8)) %>%
  select(date, model=.model, icases) %>%
  mutate(model=replace_na(model, "actual")) %>%
  ggplot(aes(date, icases)) + geom_line(aes(color=model)) +
  theme_classic() +
  scale_y_continuous(labels=scales::number_format())
