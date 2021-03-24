#' Make `tsibble`
#'
#' @description
#'
#' This function converts an input `tibble` with columns for \link[lubridate]{epiyear} and \link[lubridate]{epiweek} into a \link[tsibble]{tsibble} object. The `tsibble` has columns specifying indices for the time series as well as a date for the Monday of the epiyear/epiweek combination at each row. Users can optionally ignore the current week when generating the `tsibble` via the "chop" argument.
#'
#' @param df A `tibble` containing columns `epiyear` and `epiweek`.
#' @param chop Logical indicating whether or not to remove the most current week (default `TRUE`).
#' @return A `tsibble` containing additional columns `monday` indicating the date
#'   for the Monday of that epiweek, and `yweek` (a yearweek vctr class object)
#'   that indexes the `tsibble` in 1 week increments.
#' @export
#' @md
make_tsibble <- function(df, chop=TRUE) {
  out <- df %>%
    # get the monday that starts the MMWRweek
    dplyr::mutate(monday=MMWRweek::MMWRweek2Date(MMWRyear=epiyear, MMWRweek=epiweek, MMWRday=2), .after="epiweek") %>%
    # convert represent as yearweek (see package?tsibble)
    dplyr::mutate(yweek=tsibble::yearweek(monday), .after="monday") %>%
    # convert to tsibble
    tsibble::as_tsibble(index=yweek, key=location)
  # Remove the incomplete week
  if (chop) out <- utils::head(out, -1)
  return(out)
}

#' Make quantile tibbles
#'
#' Make a quantile tibble for required quantiles.
#' Defaults to `c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)`.
#' For `N wk ahead inc case target`, filter to: `c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)`
#'
#' @param x A numeric vector.
#' @param q Quantiles to report. Defaults to `c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)`.
#' @return A tibble of quantiles (`q`) and their values (`x`).
#' @export
#' @md
#' @examples
#' quibble(iris$Sepal.Length)
quibble <- function(x, q = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)) {
  tibble::tibble(q = q, x = stats::quantile(x, q))
}

#' Get Monday
#'
#' @description
#'
#' This function is a helper to get the date for the Monday of the current week.
#'
#' @return Date for the Monday of the current week. For more details see \link[lubridate]{floor_date}.
#' @export
#' @md
#'
this_monday <- function() {
  lubridate::floor_date(lubridate::today(), "weeks", week_start = 1)
}

#' Check Monday
#'
#' @description
#'
#' This is a helper function to see if today is Monday.
#
#' @return Logical indicating whether or not today is Monday
#' @export
#' @md
is_monday <- function() {
  lubridate::wday(lubridate::today(), label=TRUE) %in% c("Mon")
}

#' Visualize forecast output
#'
#' @description
#'
#' This function serves as a plotting mechanism for prepped forecast submission data (see \link[focustools]{format_for_submission}). Using truth data supplied, the plots show the historical trajectory of each outcome along with the point estimates for forecasts. Optionally, the user can include 50% prediction interval as well. Plots include trajectories of incident cases, incident deaths, and cumulative deaths faceted by location.
#'
#'
#' @param .data Historical truth data for all locations and outcomes in submission targets
#' @param submission Formatted submission
#' @param location Vector specifying locations to filter to; `'US'` by default.
#' @param pi Logical as to whether or not the plot should include 50% prediction interval; default is `TRUE`
#'
#' @return A `ggplot2` plot object with line plots for outcome trajectories faceted by location
#'
#' @md
#' @export
#'
plot_forecast <- function(.data, submission, location="US", pi = TRUE) {

  ## pretty sure we need to add an intermediary variable for the filter below
  ## otherwise the condition will interpret as the column name not the vector ... i think?
  loc <- location

  # Check that the specified location is in the data and submission.
  stopifnot("Specified location is not in recorded data" = loc %in% unique(.data$location))
  stopifnot("Specified location is not in forecast data" = loc %in% unique(submission$location))

  # Grab the real data
  real <-
    .data %>%
    tibble::as_tibble() %>%
    dplyr::filter(location %in% loc) %>%
    tidyr::gather(target, value, icases, ccases, ideaths, cdeaths) %>%
    dplyr::mutate(target = target %>% stringr::str_remove_all("s$") %>% stringr::str_replace_all(c("^i"="inc ", "^c"="cum "))) %>%
    dplyr::select(location, date=monday, target, point=value) %>%
    dplyr::mutate(type="recorded") %>%
    dplyr::filter(type!="cum case")


  # Grab the forecasted data
  forecasted <-
    submission %>%
    dplyr::filter(type=="point" | quantile==.25 | quantile==.75) %>%
    dplyr::filter(location %in% loc) %>%
    dplyr::mutate(quantile=tidyr::replace_na(quantile, "point")) %>%
    dplyr::select(-type) %>%
    tidyr::separate(target, into=c("nwk", "target"), sep=" wk ahead ") %>%
    dplyr::select(location, date=target_end_date, target, quantile, value) %>%
    tidyr::spread(quantile, value) %>%
    dplyr::mutate(type="forecast")

  # Bind them
  bound <-
    dplyr::bind_rows(real, forecasted) %>%
    dplyr::arrange(date, location) %>%
    dplyr::filter(location %in% loc) %>%
    dplyr::mutate(target =
                    dplyr::case_when(target == "inc case" ~ "Incident Cases",
                                     target == "inc death" ~ "Incident Deaths",
                                     target == "cum case" ~ "Cumulative Cases",
                                     target == "cum death" ~ "Cumulative Deaths")
    )

  ## get location *names* rather than code
  bound <-
    bound %>%
    dplyr::left_join(dplyr::select(locations, location, location_name), by = "location") %>%
    dplyr::select(-location) %>%
    dplyr::rename(location = location_name)

  # Plot
  p <-
    bound %>%
    ## exclude cumulative cases from plot
    dplyr::filter(target != "Cumulative Cases") %>%
    ggplot2::ggplot(ggplot2::aes(date, point)) +
    ggplot2::geom_line(ggplot2::aes(col=type)) +
    ggplot2::scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
    ggplot2::facet_wrap(~location + target, scales="free", ncol = 3) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Date", y = NULL) +
    ggplot2::theme(legend.position = "Bottom", legend.title = ggplot2::element_blank())

  if(pi) {
    p <-
      p +
      ggplot2::geom_ribbon(ggplot2::aes(fill = type, ymin = `0.25`, ymax = `0.75`),
                           alpha = 0.5, color="lightpink", data=dplyr::filter(bound, type == "forecast"))
  }

  return(p)
}

#' Reshape data for submission summary
#'
#' @description
#'
#' This unexported helper function is used in \link[focustools]{submission_summary}. It spreads forecast targets to a wide format and forces "US" locations to be at the top of the resulting `tibble`.
#'
#' @param .data Tibble with submission data
#' @param ... Additional arguments passed to \link[tidyr]{spread}
#'
#' @return A `tibble` with wide summary data.
#'
#' @md
#'
spread_value <- function(.data, ...) {

  ## quietly ...
  suppressMessages({
    tmp <-
      ## spread the data
      tidyr::spread(.data, ...) %>%
      ## then get the location names
      dplyr::left_join(dplyr::select(locations, location, location_name)) %>%
      dplyr::select(-location)
  })

  ## one more piece of logic to get "Previous" column before w ahead columns if need be
  if("Previous" %in% names(tmp)) {
    tmp <-
      tmp %>%
      dplyr::select(location = location_name, Previous, dplyr::everything())
  } else {
    tmp <-
      tmp %>%
      dplyr::select(location = location_name, dplyr::everything())
  }

  ## if US is in there put it on top
  if("US" %in% tmp$location) {
    tmp <-
      dplyr::bind_rows(dplyr::filter(tmp, location == "US"), dplyr::filter(tmp, location !="US"))
  }
}

#' Extract ARIMA parameters
#'
#' @description
#'
#' Extracts ARIMA model parameters, including p, d, q, P, D, Q, and results from \link[broom]{tidy} and \link[broom]{glance} on an ARIMA model object.
#'
#' @param arimafit A single-row mable (`mdl_df`) from `fabeltools::model(arima=ARIMA(...))`.
#'
#' @return A single-row `tibble` containing ARIMA model parameter and diagnostic information.
#' @md
#' @export
extract_arima_params <- function(arimafit) {
  if (!("mdl_df" %in% class(arimafit))) stop("Input must be a mdl_df (mable) from fabletools::model().")
  if (nrow(arimafit)>1) stop("Input mdl_df must have only one row (one location).")
  if (names(arimafit)[1]!="location") stop("Input mdl_df must have location.")
  if (class(arimafit[[2]][[1]]$fit)!="ARIMA") stop("Model must be ARIMA.")
  .tidy <- fabletools::tidy(arimafit)
  .glance <- fabletools::glance(arimafit)
  .broom <- dplyr::inner_join(.tidy, .glance, by=c("location", ".model"))
  .params <- arimafit[[2]][[1]]$fit$spec
  dplyr::bind_cols(.params, .broom) %>%
    dplyr::select(location, .model, dplyr::everything()) %>%
    dplyr::select_if(is.atomic) %>%
    dplyr::inner_join(locations %>% dplyr::select(location, abbreviation, location_name), ., by="location")
}
