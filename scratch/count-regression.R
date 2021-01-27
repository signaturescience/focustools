library(tidyverse)
library(focustools)
library(trending)
library(trendeval)

usac <-  get_cases(source="jhu",  granularity = "county")
usad <- get_deaths(source="jhu",  granularity = "county")

## use the focustools helper to prep the tsibble format
usa <-
  dplyr::inner_join(usac, usad, by = c("epiyear", "epiweek", "location")) %>%
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
glm_fit <- function(.data,
                    models = list(glm_poisson = trending::glm_model(icases ~ index, family = "poisson"),
                                  glm_quasipoisson = trending::glm_model(icases ~ index, family = "quasipoisson"),
                                  glm_negbin = trending::glm_nb_model(icases ~ index)),
                    metrics = list(yardstick::rmse, yardstick::huber_loss, yardstick::mae)) {

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
                  location = unique(dat$location),
                  data = tidyr::nest(dat, fit_data = everything()))
    message("No model could be fit because no incident case data available.")
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
                         location = unique(dat$location),
                         data = tidyr::nest(dat, fit_data = everything()))

    message("Selected model ...")
    message(as.character(ret$fit$fitted_model$family)[1])
  }
  return(ret)

}


## helper to get the quantiles from prediction intervals
glm_quibble <- function(fit, new_data, alpha) {

  ## get the quantiles from the alpha
  q_lower <- alpha/2
  q_upper <- 1 - q_lower

  ## run the predict method on the fitted model
  ## use the given alpha
  fit %>%
    predict(new_data, alpha = alpha) %>%
    ## get just the prediction interval bounds ...
    ## index (time column must be named index) ...
    select(index, lower_pi, upper_pi) %>%
    ## reshape so that its in long format
    gather(quantile, value, lower_pi:upper_pi) %>%
    ## and subout out lower_pi/upper_pi for the appropriate quantile
    mutate(quantile = ifelse(quantile == "lower_pi", q_lower, q_upper))
}
## a generic forecast function to use fit objects from above
glm_forecast <- function(.data, fit, horizon = 4, alpha = 0.05) {

  ## get the last index from the data provided
  last_index <-
    .data %>%
    dplyr::arrange(location, epiweek, epiyear) %>%
    dplyr::mutate(index = 1:dplyr::n()) %>%
    dplyr::pull(index) %>%
    tail(1)

  ## get the last index from the data provided
  last_week <-
    .data %>%
    dplyr::arrange(location, epiweek, epiyear) %>%
    dplyr::pull(yweek) %>%
    tail(1)

  ## add indices through the horizon
  ## without other predictors this is just a tibble of indices
  ## if we had other predictors you would need to pass them in here
  new_data <-
    dplyr::tibble(index = last_index + 1:horizon, yweek = last_week + 1:horizon)

  # ## take the fit object provided and use predict
  point_estimates <-
    fit %>%
    predict(new_data) %>%
    dplyr::select(index, estimate) %>%
    dplyr::mutate(estimate = round(estimate)) %>%
    dplyr::mutate(quantile = NA) %>%
    dplyr::select(index, quantile, value = estimate)

  ## map the quibble function over the alphas
  quants <- map_df(alpha, .f = function(x) glm_quibble(fit = fit, new_data = new_data, alpha = x))

  ## prep data
  dplyr::bind_rows(point_estimates,quants) %>%
    dplyr::arrange(index, quantile) %>%
    dplyr::left_join(new_data) %>%
    dplyr::select(yweek,quantile,value)
}

## try it on cville data
cville <-
  usa %>%
  filter(location == "51540") %>%
  mutate(index = 1:n()) %>%
  as_tibble()

cville_fit <- glm_fit(cville, models = models)

## NOTE: use quantiles up to 0.45 then multiply by 2 for alpha (because two-sided / symmetrical)
alphas <- c(0.01, 0.025, seq(0.05, 0.45, by = 0.05)) * 2
cville_forecast <- glm_forecast(cville, horizon = 4, fit = cville_fit$fit, alpha = alphas)
cville_forecast

## some weirdo plotting code just to see the results for now
## TODO: figure out a better way to plot this result (eventually plot_forecast should do it)
cville_forecast %>%
  filter(is.na(quantile) | quantile == 0.25 | quantile == 0.75) %>%
  mutate(quantile = ifelse(is.na(quantile), "point", quantile)) %>%
  spread(quantile,value) %>%
  mutate(type = "Forecast") %>%
  bind_rows(select(cville, yweek, point = icases)) %>%
  mutate(type = ifelse(is.na(type), "Observed", type)) %>%
  mutate(date = as.Date(yweek)) %>%
  ggplot(aes(date, point)) +
  geom_line(aes(group = type, col = type)) +
  geom_ribbon(aes(group = type, fill = type, ymin = `0.25`, ymax = `0.75`), alpha = 0.25)

## NOTE: this is the same thing as above but with travis county (austin tx)
travis <-
  usa %>%
  filter(location == "48453") %>%
  mutate(index = 1:n()) %>%
  as_tibble()

travis_fit <- glm_fit(travis, models = models)

## NOTE: use quantiles up to 0.45 then multiply by 2 for alpha (because two-sided / symmetrical)
alphas <- c(0.01, 0.025, seq(0.05, 0.45, by = 0.05)) * 2
travis_forecast <- glm_forecast(travis, horizon = 4, fit = travis_fit$fit, alpha = alphas)
travis_forecast

travis_forecast %>%
  filter(is.na(quantile) | quantile == 0.25 | quantile == 0.75) %>%
  mutate(quantile = ifelse(is.na(quantile), "point", quantile)) %>%
  spread(quantile,value) %>%
  mutate(type = "Forecast") %>%
  bind_rows(select(travis, yweek, point = icases)) %>%
  mutate(type = ifelse(is.na(type), "Observed", type)) %>%
  mutate(date = as.Date(yweek)) %>%
  ggplot(aes(date, point)) +
  geom_line(aes(group = type, col = type)) +
  geom_ribbon(aes(group = type, fill = type, ymin = `0.25`, ymax = `0.75`), alpha = 0.25)

## now try on all counties
## split the tibble by location (FIPS code)
county_dat <-
  usa %>%
  as_tibble() %>%
  group_split(location)

system.time({
  fits <-
    ## scratch trying on all counties at first ...
    ## just the first 10 because this takes a while
    county_dat[1:20] %>%
    ## map the glm fit over and return a list
    map(., glm_fit, models = models)
})


## A LOT faster with furrr
library(furrr)
plan(multisession, workers = 4)

system.time({
  fits <- future_map(county_dat[1:100], glm_fit)
})


##TODO: figure out how to use glm_forecast() to get predictions for all counties

l <- list()
for(i in 1:length(fits)) {

  if(!"fit" %in% names(fits[[i]])) {
    message("skipping")
    next
  }

  tmp_loc <- unique(fits[[i]]$location)
  message(sprintf("forecasting for %s", tmp_loc))
  tmp_dat <- county_dat[[i]]
  tmp_fit <- fits[[i]]$fit

  tmp_res <- glm_forecast(tmp_dat, horizon = 4, fit = tmp_fit, alpha = alphas)
  tmp_res$location <- tmp_loc

  l[[i]] <- tmp_res

}

res <- bind_rows(l)

res
