library(tidyverse)
## need yawp for read_repo (see below)
# devtools::install_github("yawp")
library(yawp)

## NOTE: evaluating back 4w so that we can get observed for the full range of horizons  ...
## ... by doing so we will need to hardcode the date to get other forecast (see c19fh_forecasts below)
## moving forward we probably want to make this more flexible

## NOTE: the code ONLY looks at absolute difference in point estimates ...
## ... better way to do this to include quantiles ??

######################################################################################
## first get the forecast using our approach

## get data at the national scale from nyt source
usac <-  get_cases(source="jhu",  granularity = "national")
usad <- get_deaths(source="jhu",  granularity = "national")

## use the focustools helper to prep the tsibble format
usa <-  
  dplyr::inner_join(usac, usad, by = c("epiyear", "epiweek")) %>% 
  make_tsibble(chop=TRUE)

usa <-
  usa %>%
  slice(-utils::tail(dplyr::row_number(), 4))

fit.icases <- usa %>% model(arima = ARIMA(icases, stepwise=FALSE, approximation=FALSE))
fit.ideaths <- usa %>% model(linear_caselag3 = TSLM(ideaths ~ lag(icases, 3)))

icases_forecast <- ts_forecast(fit.icases, outcome = "icases", horizon = 4)
icases_forecast


## need to get future cases to pass to ideaths forecast
future_cases <- ts_futurecases(usa, icases_forecast, horizon = 4)
# Forecast incident deaths based on best guess for cases
ideaths_forecast <- ts_forecast(fit.ideaths,  outcome = "ideaths", new_data = future_cases)
ideaths_forecast

cdeaths_forecast <- ts_forecast(outcome = "cdeaths", .data = usa, inc_forecast = ideaths_forecast)
cdeaths_forecast

submission <-
  list(format_for_submission(icases_forecast, target_name = "inc case"),
       format_for_submission(ideaths_forecast, target_name = "inc death"),
       format_for_submission(cdeaths_forecast, target_name = "cum death")) %>%
  reduce(bind_rows) %>%
  arrange(target)

## prep the submission so that we can bind it with the other teams forecast data
## and eventually join to the observed
sigsci_ts <-
  submission %>% filter(type == "point") %>%
  mutate(id = "SigSci-TS",
         model = "SigSci-TS") %>%
  separate(., target, into = c("horizon", "target"), sep = "wk ahead") %>%
  mutate(horizon = str_trim(horizon, "both"),
         target = str_trim(target, "both"))

######################################################################################
## compile previous forecast data
## need yawp to do this
# devtools::install_github("vpnagraj/yawp")

## simple custom function to read the forecast files from GH
read_forecast <- function(fp) {
  tmp <- read_csv(fp)
  tmp$location <- as.character(tmp$location)
  tmp$model <- fp
  return(tmp)
}

## get the forecasts
c19fh_forecasts <- read_repo(repo = "signaturescience/covid19-forecast-hub", 
                             pattern = "data-processed/.*/2020-12-1[3|4].*.csv", 
                             to_tibble = TRUE, 
                             .f = read_forecast)


## (admittedly ugly) code to get a vector of the names of targes that we (SigSci-TS) are forecasting
trgts <- 
  sapply(1:4, function(x) paste0(x, c(" wk ahead inc death", " wk ahead inc case", " wk ahead cum death"))) %>%
  as.vector()

## prep the combined forecasts so that they can bind to ours and be joined to observed
c19fh_forecasts_prepped <-
  c19fh_forecasts %>%
  filter(target %in% trgts) %>%
  filter(type == "point") %>%
  filter(location == "US") %>%
  separate(., target, into = c("horizon", "target"), sep = "wk ahead") %>%
  mutate(horizon = str_trim(horizon, "both"),
         target = str_trim(target, "both"))

## add a column with unique id per model
c19fh_forecasts_prepped$id <-   
  c19fh_forecasts_prepped %>%
  group_indices(model)

## prep tibble with observed data
observed <- 
  ## join data we pulled above
  dplyr::inner_join(usac, usad, by = c("epiyear", "epiweek")) %>% 
  make_tsibble(chop=TRUE) %>%
  ## get just the last 4 weeks
  slice(tail(dplyr::row_number(), 4)) %>%
  ## make sure the target suffix matches the submission format
  rename(`inc case` = icases,
         `inc death` = ideaths,
         `cum death` = cdeaths) %>%
  as_tibble() %>%
  ungroup() %>%
  mutate(horizon = as.character(1:n())) %>%
  select(-ccases, -epiyear, -epiweek, -monday, -yweek) %>%
  gather(key = target, value = observed, `inc case`:`cum death`)

## bind everything together and join to observed
dat <-
  c19fh_forecasts_prepped %>%
  rbind(., sigsci_ts) %>%
  left_join(observed) %>%
  mutate(diff = value - observed) %>%
  mutate(target = 
           case_when(
             target == "cum death" ~ "cdeaths",
             target == "inc death" ~ "ideaths",
             target == "inc case" ~ "icases"))
  
dat %>%
  ggplot(aes(id, diff)) +
  geom_point() +
  ## firebrick red point is SigSci-TS
  geom_point(aes(id, diff), data = filter(dat, id == "SigSci-TS"), col = "firebrick") +
  geom_hline(yintercept = 0) +
  facet_wrap(~ target + horizon, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  labs(x = NULL, 
       y = "Absolute difference", 
       title = "Predicted vs Observed Stratified by Team and Target",  
       subtitle = "(2020-EW51 to 2021-EW1)", 
       caption = "SigSci-TS in red")

## now ... how do we (SigSci-TS) rank?
dat %>%
  mutate(diff = abs(diff)) %>%
  group_by(horizon, target) %>%
  mutate(rank = dense_rank(diff),
         total = n()) %>%
  mutate(text = paste0(rank, " out of ", total)) %>%
  filter(id == "SigSci-TS") %>%
  select(horizon, target, text)
