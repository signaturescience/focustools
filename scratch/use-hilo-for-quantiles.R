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

fits.icases  <- usa %>% model(arima = ARIMA(icases, stepwise=FALSE, approximation=FALSE))
fits.ideaths <- usa %>% model(caselag = TSLM(ideaths ~ lag(icases, 3)))
forc.icases  <- ts_forecast(fits.icases, outcome="icases", horizon=horizon, bootstrap=TRUE)
futr.icases  <- ts_futurecases(usa, forc.icases, horizon=horizon)
forc.ideaths <- ts_forecast(fits.ideaths, outcome="ideaths", new_data=futr.icases, bootstrap=FALSE)
forc.ideaths.bootstrap <- ts_forecast(fits.ideaths, outcome="ideaths", new_data=futr.icases, bootstrap=TRUE)

library(ggplot2)
theme_set(theme_bw())
p1 <- forc.ideaths %>%
  mutate(yweek=factor(yweek)) %>%
  mutate(quantile=ifelse(type=="point", .5, quantile)) %>%
  ggplot(aes(quantile, value)) + geom_point(aes(color=type)) + facet_wrap(~yweek, scale="free_y") + labs(title="California: using hilo")
p2 <- forc.ideaths.bootstrap %>%
  mutate(yweek=factor(yweek)) %>%
  mutate(quantile=ifelse(type=="point", .5, quantile)) %>%
  ggplot(aes(quantile, value)) + geom_point(aes(color=type)) + facet_wrap(~yweek, scale="free_y") + labs(title="California: using boostrapping")
library(patchwork)
p1+p2+plot_layout(guides="collect")
if(interactive()) ggsave("~/Downloads/quantile-comparison.png", width=10, height=5)
