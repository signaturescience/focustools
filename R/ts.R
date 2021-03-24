#' Wrapper to fit time series models
#'
#' @description
#'
#' The time series forecasting pipeline depends on time series models fit with the \link[fabletools]{model} function. This function provides a wrapper that allows the user to pass in a list of function definitions and return a list of model outputs (`mable` objects) corresponding to each fit. The function also allows the user to pass in a vector of multiple outcome variable names (i.e. "ideaths" and "icases").
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
#' @param outcome Name of the outcome; must be one of `'icases',`,`'ideaths'`, `'cdeaths'`, `'ccases'`
#' @param horizon Optional horizon periods through which the forecasts should be generated; default is `4`
#' @param new_data Optional covariate data for forecasts using models that were fit using other variables; should be generated using \link[tsibble]{new_data}; default is `NULL`
#' @param bootstrap If `TRUE` use bootstrap to resample forecasts for quantiles. If `FALSE` (default), use \link[fabletools]{hilo} to extract a specified prediction interval at a particular confidence level from a distribution.
#' @param seed Random seed used in bootstrapping process; default `1863`
#' @param ... Additional parameters passed to the \link[focustools]{ts_cumulative_forecast} helper; only used if the forecast is cumulative
#'
#' @return A `tibble` with forecast results, including the name of the model, year and week, value of the forecast estimate, type of estimate (quantile or point), and bin of the quantile (if applicable) for the estimate.
#'
#' @export
#'
#' @md
#'
#'
ts_forecast <- function(mable, outcome, horizon = 4, new_data = NULL, bootstrap=FALSE, seed = 1863, ...) {

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

    # If using bootstrapping to resample forecasts:
    if (bootstrap) {

      # bootstrap a model
      boots <-
        mable %>%
        fabletools::generate(h=horizon, times=1000, bootstrap=TRUE, new_data = new_data, seed = seed)

      # get the quantiles
      myquibbles <-
        boots %>%
        dplyr::as_tibble() %>%
        dplyr::group_by(.model, yweek, location) %>%
        dplyr::summarize(quibble(.sim), .groups="drop")

      .forecast <-
        dplyr::bind_rows(
          myquibbles %>%
            dplyr::mutate(type="quantile") %>%
            dplyr::rename(quantile=q, value=x),
          myforecast %>%
            dplyr::as_tibble() %>%
            dplyr::mutate(quantile=NA_real_, .after=yweek) %>%
            dplyr::mutate(type="point") %>%
            dplyr::rename(value=.mean)
        )

    } else {

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

    }

    ## return named list with forecast AND quibbles
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

#' Compute accuracy metrics
#'
#' @description
#'
#' This function wraps \link[focustools]{ts_fit} to provide an interface for evaluting accuracy measures for one or more model definitions. Metrics included come from the \link[fabletools]{accuracy} function, and model definitions must be time series models (see \link[fabletools]{model} for more information). Note that the function uses the "horizon" to split data in training/test sets. For example, the default "horizon" (`4`) will use the last four weeks of input data as the test set and all data prior to those four weeks as the training set.
#'
#' @param .data Data to use for modeling
#' @param horizon Horizon of periods to use for splitting input to ".data" into training / test sets; default is `4`
#' @param outcomes Character vector specifying names of the column to use as the outcome
#' @param .fun List of modeling functions to use
#'
#' @return A `tibble`` containing one row for each combination of model and outcome with accuracy measures from \link[fabletools]{accuracy}.
#' @md
#' @export
#'
ts_accuracy <- function(.data, horizon = 4, outcomes, .fun) {

  ## create training and test split by horizon
  train_data <-
    .data %>%
    dplyr::slice(-utils::tail(dplyr::row_number(), horizon))

  test_data <-
    .data %>%
    dplyr::slice(utils::tail(dplyr::row_number(), horizon))

  ## run ts_fit
  fit <- ts_fit(.data = train_data, outcomes = outcomes, .fun = .fun, single = FALSE)

  ## run forecast operation on all fits
  ## need the nested map here to traverse the list of lists
  ## recall: ts_fit returns a list of models nested in a list of otucomes
  l <- purrr::map(fit, function(x) purrr::map(x, function(y) (fabletools::forecast(y, h=4))))

  ## now loop over each outcome in that list ...
  ## get its name ...
  ## force the column names for test data to be named to match the outcome name in the .fun funs
  ## NOTE: using a for loop here to have a little more flexibility than a nested map()
  ## create empty tibble to store loop results
  res <- dplyr::tibble()

  for(i in 1:length(names(l))) {

    test_data_tmp <- test_data

    outcome <- names(l)[i]

    names(test_data_tmp)[which(names(test_data_tmp) == outcome)] <- "x"
    tmp_res <- purrr::map_df(l[[i]], fabletools::accuracy, test_data_tmp)
    tmp_res$outcome <- outcome

    res <- rbind(res,tmp_res)

  }

  return(res)
}
