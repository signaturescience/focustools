#' Generate time series forecasts including quantile estimates
#'
#' @param mable
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
