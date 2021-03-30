#' Wrapper to fit time series models
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#' The time series forecasting pipeline depends on time series models fit with the \link[fabletools]{model} function. This function provides a wrapper that allows the user to pass in a list of function definitions and return a list of model outputs (`mable` objects) corresponding to each fit. The function also allows the user to pass in a vector of multiple outcome variable names (i.e. "ideaths" and "icases").
#'
#' **NOTE**: The functionality in `ts_fit()` is experimental. Users may find more flexibility using the \link[fabletools]{model} function to fit models to be used downstream in `ts_forecast()`.
#'
#' @param .data Data to use for modeling
#' @param outcomes Character vector specifying names of the column to use as the outcome
#' @param .fun List of modeling functions to use
#' @param single Boolean indicating whether or not a "shortcut" should be used to return a single `tibble`; only works if there is one outcome ("outcomes") and one model function (".fun"); default is `TRUE`
#'
#' @return A single `mable` (model table) if (`single = TRUE`) or a named list of `mable`s (if `single = FALSE`). For more details on data structure see \link[fabletools]{mable}.
#' @export
#'
#' @md
#'
ts_fit <- function(.data, outcomes, .fun, single = TRUE) {

  ## use variable names to get list of vectors for time series modeling for each outcome
  ## doing this in a for loop for expedience (convert to purrr ??)
  res <- list()
  for(i in 1:length(outcomes)) {
    outcome <- outcomes[i]
    ## probably should handle this better with quosures or similar
    ## here unlisting the subset data to ensure it is a vector
    train_dat <- unlist(.data[,outcome], use.names = FALSE)
    res[[i]] <- purrr::invoke_map(.f = .fun, list(list(x = train_dat, .data = .data)))
    names(res[[i]]) <- names(.fun)

  }

  ## make sure list is named by outcome
  res <-
    res %>%
    purrr::set_names(outcomes)

  ## handle the "single" model case
  if(single) {
    if(length(outcomes) > 1 | length(.fun) > 1) {
      stop("you cannot use the 'single' shortcut if you have more than outcome and/or model ...")
    }
    res <- res[[1]][[1]]
  }
  return(res)
}



#' Generate time series forecasts including quantile estimates
#'
#' @description
#'
#' This function can convert models fit with \link[fabletools]{model} or the \link[focustools]{ts_fit} wrapper to forecasted values. The user specifies the horizon out to which forecasts should be generated, as well as any optional covariate data needed for forecasting (e.g. when using a model of incident deaths based on lagged incident cases, the forecast function needs incident cases moving into the forecast horizons; see "new_data" argument). The forecasts generated will include point estimates as well as 23 quantiles: 0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 0.975, 0.99. By default these quantiles are calculated using the \link[fabletools]{hilo}.
#'
#' @param mable A `mable` (model table); for more information see \link[fabletools]{mable}
#' @param outcome Name of the outcome; must be one of `'icases'`,`'ideaths'`, `'cdeaths'`, `'ccases'`
#' @param horizon Optional horizon periods through which the forecasts should be generated; default is `4`
#' @param new_data Optional covariate data for forecasts using models that were fit using other variables; should be generated using \link[tsibble]{new_data}; default is `NULL`
#' @param ... Additional parameters passed to the \link[focustools]{ts_cumulative_forecast} helper; only used if the forecast is cumulative
#'
#' @return A `tibble` with forecast results, including the name of the model, year and week, value of the forecast estimate, type of estimate (quantile or point), and bin of the quantile (if applicable) for the estimate.
#'
#' @export
#'
#' @md
#'
#'
ts_forecast <- function(mable, outcome, horizon = 4, new_data = NULL, ...) {

  if(outcome == "cdeaths" | outcome == "ccases") {
    .forecast <- ts_cumulative_forecast(outcome = outcome, ...)
    return(.forecast)
  } else if (outcome == "ideaths" | outcome == "icases" | outcome=="ihosp") {
    # forecast
    if (is.null(new_data)) {
      myforecast <- fabletools::forecast(mable, h=horizon)
    } else {
      myforecast <- fabletools::forecast(mable, new_data=new_data)
    }

    # Make the 0.5 quantile the means (point estimates). The quidk doesn't contain a
    # median hilo. You'll bind this to the other quantiles in the step below.
    q5 <-
      myforecast %>%
      tibble::as_tibble() %>%
      dplyr::transmute(.model, yweek, location, quantile=0.5, value=.mean, type="quantile")

    point_estimates <-
      myforecast %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(quantile=NA_real_, .after=yweek) %>%
      dplyr::mutate(type="point") %>%
      dplyr::rename(value=.mean) %>%
      dplyr::select(.model, yweek, location, quantile, value, type)

    # Create quantile table from distribution column in the forecast
    .forecast <-
      myforecast %>%
      fabletools::hilo(sort(unique(quidk$interval))) %>%
      fabletools::unpack_hilo(dplyr::ends_with("%")) %>%
      tidyr::gather(key, value, dplyr::contains("%")) %>%
      dplyr::inner_join(quidk, by="key") %>%
      tibble::as_tibble() %>%
      dplyr::transmute(.model, yweek, location, quantile, value, type="quantile") %>%
      dplyr::bind_rows(q5) %>%
      dplyr::bind_rows(point_estimates) %>%
      dplyr::arrange(yweek, quantile)

    ## return forecast as tibble
    return(.forecast)

  } else {
    stop("Outcome must be one of 'icases', 'ideaths', 'ihosp', 'ccases', or 'cdeaths' ...")
  }

}

#' Helper to generate the estimate of incident cases from an icases forecast object
#'
#' @description
#'
#' This function takes a time series forecast and extracts the point estimate for incident cases out to a specified horizon. This is necessary to generate the "new_data" to be passed into the \link[focustools]{ts_forecast} incident death models that are based on lagged cases.
#'
#' @param .data Data from which the \link[tsibble]{new_data} should be generated; *CAUTION* for best results make sure that the data passed to this argument is the same object as used to generate the model/forecast that is specified in ".forecast"
#' @param .forecast A `tibble` with forecast data generated using \link[focustools]{ts_forecast}; should *only* be a forecast of incident cases
#' @param horizon Horizon periods through which the \link[tsibble]{new_data} should be generated; default is `4`
#'
#' @return A `tsibble` with horizon periods and respective forecasted incident cases.
#'
#' @md
#' @export
#'
ts_futurecases <- function(.data, .forecast, horizon = 4) {

  ## get the point estimates from the
  best_guess <-
    .forecast %>%
    dplyr::group_by(location) %>%
    dplyr::filter(type == "point") %>%
    dplyr::arrange(yweek) %>%
    dplyr::select(location, yweek, icases = value)

  tsibble::new_data(.data, horizon, key=location) %>%
    dplyr::left_join(best_guess, by = c("yweek", "location"))
}


#' Helper used in `ts_forecast()` to get cumulative forecast from incident
#'
#' @description
#'
#' This unexported helper is used internally in \link[focustools]{ts_forecast} to generate cumulative forecasts from incident. The function cumulatively sums incident estimates (quantile and point) at each location. Note that if used outside of \link[focustools]{ts_forecast} one must be sure that the ".data" argument matches object used to generate the incident forecast object ("inc_forecast").
#'
#' @param .data Data from which the cumulative forecast should get recent counts; *CAUTION* for best results make sure that the data passed to this argument is the same object as used to generate the model/forecast that is specified in "inc_forecast"
#' @param outcome Name of the outcome; should be be one of `'cdeaths'` or `'ccases'`
#' @param inc_forecast A `tibble` with incident forecast data generated using \link[focustools]{ts_forecast}; should *only* be incident cases corresponding to outcome for which cumulative count is to be aggregated
#'
#' @return  A `tibble` with forecast results, including the name of the model, year and week, value of the forecast estimate, type of estimate (quantile or point), and bin of the quantile (if applicable) for the estimate.
#' @md
#'
ts_cumulative_forecast <- function(.data, outcome = "cdeaths", inc_forecast) {

  # What was the last week of recorded data you have?
  last_recorded_week <- max(.data$yweek)
  # What's the first forecasted week?
  first_forecast_week <- min(inc_forecast$yweek)

  # Check to make sure they're 1 week apart. "Arithmetic" works on yearweek vctrs!
  stopifnot(first_forecast_week == last_recorded_week+1)

  # What's the cumulative {outcome} for the last week of recorded data?
  recorded_so_far <-
    .data %>%
    dplyr::group_by(location) %>%
    dplyr::filter(yweek==last_recorded_week) %>%
    dplyr::as_tibble() %>%
    dplyr::select(location, observed_to_now = outcome)

  # Starting with the inc deaths forecast
  .forecast <-
    inc_forecast %>%
    # Make sure you're arranged ascending by date (yweek)
    dplyr::arrange(yweek, type, quantile,location) %>%
    # Create a new grouping variable, throw it away when you're done with the mutate
    # I don't know what happens if you group by quantile but quantile is NA for point estimates.
    # This creates a throwaway char that's eg " US quantile 0.01" or "US point NA" so you're not grouping over an NA
    dplyr::mutate(groupvar=paste(location,type, quantile)) %>%
    # Group by each quantile(/point)
    dplyr::group_by(groupvar) %>%
    # For each point or quantile, get the cumulative sums for weeks 2, 3, and 4
    dplyr::mutate(cvalue=cumsum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(recorded_so_far, by = "location") %>%
    # Then add the deaths recorded so far
    dplyr::mutate(cvalue=cvalue+observed_to_now) %>%
    # Make the 'value' column this new cumulative sum
    dplyr::mutate(value=cvalue) %>%
    dplyr::mutate(value = ifelse(value < observed_to_now, observed_to_now, value)) %>%
    # Get rid of junk
    dplyr::select(-groupvar, -cvalue,-observed_to_now)

  return(.forecast)

}
