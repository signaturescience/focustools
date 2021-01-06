#' Title
#'
#' @param mable
#' @param horizon
#' @param new_data
#' @param seed
#'
#' @return
#' @export
#'
#' @examples
#'
ts_forecast <- function(mable, horizon = 4, new_data = NULL, seed = 1863) {

  # forecast
  if (is.null(new_data)) {
    myforecast <- forecast(mable, h=horizon)
  } else {
    myforecast <- forecast(mable, new_data=new_data)
  }

  # bootstrap a model
  boots <-
    mable %>%
    generate(h=horizon, times=1000, bootstrap=TRUE, new_data = new_data, seed = seed)

  # get the quantiles
  myquibbles <-
    boots %>%
    as_tibble() %>%
    group_by(.model, yweek) %>%
    summarize(quibble(.sim), .groups="drop")

  .forecast <-
    bind_rows(
      myquibbles %>%
        mutate(type="quantile") %>%
        rename(quantile=q, value=x),
      myforecast %>%
        as_tibble() %>%
        mutate(quantile=NA_real_, .after=yweek) %>%
        mutate(type="point") %>%
        rename(value=.mean)
    )

  ## return named list with forecast AND quibbles
  return(.forecast)

}
