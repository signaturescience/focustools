#' Wrapper to fit time series models
#'
#' @param .data
#' @param outcomes
#' @param .fun
#' @param single
#'
#' @return A `mable` (model table). For more information see \link[fabletools]{mable}.
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
    # return(train_dat)
    res[[i]] <- purrr::invoke_map(.f = .fun, list(list(x = train_dat, .data = .data)))
    names(res[[i]]) <- names(.fun)

  }
  res <-
    res %>%
    purrr::set_names(outcomes)

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
#' @param mable A `mable` (model table). For more information see \link[fabletools]{mable}.
#' @param horizon Optional horizon periods through which the forecasts should be generated; default is `4`
#' @param new_data Optional covariate data for forecasts using models that were fit using other variables; should be generated using \link[tsibble]{new_data}; default is `NULL`
#' @param seed Random seed used in bootstrapping process; default `1863`
#'
#' @return
#' @export
#'
#' @md
#'
#'
ts_forecast <- function(mable, horizon = 4, new_data = NULL, seed = 1863) {

  # forecast
  if (is.null(new_data)) {
    myforecast <- fabletools::forecast(mable, h=horizon)
  } else {
    myforecast <- fabletools::forecast(mable, new_data=new_data)
  }

  # bootstrap a model
  boots <-
    mable %>%
    fabletools::generate(h=horizon, times=1000, bootstrap=TRUE, new_data = new_data, seed = seed)

  # get the quantiles
  myquibbles <-
    boots %>%
    dplyr::as_tibble() %>%
    dplyr::group_by(.model, yweek) %>%
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

  ## return named list with forecast AND quibbles
  return(.forecast)

}
