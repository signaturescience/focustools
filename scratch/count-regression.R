library(tidyverse)
library(focustools)
library(trending)
library(trendeval)

usac <-  get_cases(source="jhu",  granularity = "county")
usad <- get_deaths(source="jhu",  granularity = "county")

## use the focustools helper to prep the tsibble format
usa <-
  dplyr::inner_join(usac, usad, by = c("epiyear", "epiweek", "location")) %>%
  ## NOTE: need to fix this in get_* functions
  ## but basically this code will make sure that for incident cases you cannot have -
  ## looks like this happens in some counties that revise their cumulative case counts in given weeks
  ## and we use cumulative case/death counts to create incident counts
  mutate(icases = ifelse(icases < 0, 0, icases)) %>%
  make_tsibble(chop=TRUE)

## models to use in a named list
## NOTE: these model functions come from the trending pkg
models  <- list(
  # simple = lm_model(icases ~ index),
  glm_poisson = trending::glm_model(icases ~ index, family = "poisson"),
  glm_quasipoisson = trending::glm_model(icases ~ index, family = "quasipoisson"),
  glm_negbin = trending::glm_nb_model(icases ~ index)
)

## define a generic function to fit these models
glm_fit <- function(.data, models, metrics = list(yardstick::rmse, yardstick::huber_loss, yardstick::mae)) {

  ## convert the tsibble to a tibble to make it easier to work with
  ## add an index column to serve as predictor
  dat <-
    .data %>%
    as_tibble() %>%
    # group_by(location) %>%
    mutate(index = 1:n())

  message(unique(dat$location))

  ## if all of the incident cases are 0 then return a NULL tibble
  if(sum(dat$icases) == 0) {
    ret <- tibble(model_class = NULL,
                  fit = NULL,
                  location = unique(dat$location))
  } else {
    ## evaluate models and use metrics provided in arg
    res <- evaluate_models(
      models,
      dat,
      method = evaluate_resampling,
      metrics = metrics
    )

    ## pull the best model by rmse
    best_by_rmse <-
      res %>%
      filter(map_lgl(warning, is.null)) %>%  # remove models that gave warnings
      filter(map_lgl(error, is.null))  %>%   # remove models that errored
      slice_min(rmse) %>%
      select(model) %>%
      pluck(1,1)

    ## fit the model
    tmp_fit <-
      best_by_rmse %>%
      fit(dat)

    ## construct tibble with model type, actual fit, and the location
    ret <- dplyr::tibble(model_class = best_by_rmse$model_class,
                         fit = tmp_fit,
                         location = unique(dat$location))
  }

  return(ret)

}


## a generic forecast function to use fit objects from above
glm_forecast <- function(.data, fit, horizon = 4) {

  ## get the last index from the data provided
  last_index <-
    .data %>%
    arrange(location, epiweek, epiyear) %>%
    mutate(index = 1:n()) %>%
    pull(index) %>%
    tail(1)

  ## add indicies through the horizon
  ## without other predictors this is just a tibble of indices
  ## if we had other predictors you would need to pass them in here
  new_data <-
    tibble(index = last_index + 1:horizon)

  ## take the fit object provided and use predict
  fit %>%
    predict(new_data)
}

## try it on cville data
cville <-
  usa %>%
  filter(location == "51540") %>%
  mutate(index = 1:n()) %>%
  as_tibble()

cville_fit <- glm_fit(cville, models = models)
cville_forecast <- glm_forecast(cville, horizon = 4, fit = cville_fit$fit)

cville_forecast

## now try on all counties
## split the tibble by location (FIPS code)
county_dat <-
  usa %>%
  as_tibble() %>%
  group_split(location)

fits <-
  ## scratch trying on all counties at first ...
  ## just the first 10 because this takes a while
  county_dat[1:10] %>%
  ## map the glm fit over and return a list
  map(., glm_fit, models = models)

fits

##TODO: figure out how to use glm_forecast() to get predictions for all counties

