#' Make tsibble
#'
#' @param df A tibble containing columns `epiyear` and `epiweek`.
#' @param chop Logical indicating whether or not to remove the most current week (default `TRUE`).
#' @return A tsibble containing additional colums `monday` indicating the date
#'   for the Monday of that epiweek, and `yweek` (a yearweek vctr class object)
#'   that indexes the tsibble in 1 week increments.
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
  # if (chop) out <- out %>% dplyr::filter(lubridate::week(monday)!=lubridate::week(lubridate::today()))
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

#' Parse model metadata from COVID-19 Forecast Hub repository
#'
#' @param team Optional indvidual team name for which the metadata; default `NULL`
#' @param write Boolean indicating whether or not the metadata results should be written to disk
#'
#' @return A `tibble` with the model metadata, which includes (among other fields) team name, model name, description of methods, licensing info, and relevant URLs.
#'
#' @export
#'
#' @md
#'
#'
c19fh_meta <- function(team = NULL, write = TRUE) {

  ## construct request to GH API endpoint
  req <- httr::GET("https://api.github.com/repos/reichlab/covid19-forecast-hub/git/trees/master?recursive=1")

  ## get paths to all files
  repo_files <- purrr::map_chr(httr::content(req)$tree, "path")

  ## if you want to look at a specific team ...
  if(!is.null(team)) {
    ## subset to only metadata files
    meta_files <- repo_files[grepl(paste0("^data-processed/", team, "/metadata*"), repo_files)]
  } else {
    ## subset to only metadata files
    meta_files <- repo_files[grepl("^data-processed/.*/metadata*", repo_files)]
  }

  ## add prefix for API endpoint
  meta_files <- paste0("https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/", meta_files)

  ## read_yaml from GH directly and map to a tibble
  c19f_methods <- purrr::map_df(meta_files, yaml::read_yaml)

  if(write) {
    ## write csv to disk
    outfile <- paste0(format(Sys.Date(), "%F"), "-c19fh-metadata.csv")
    message(paste0("Saving to file: "), outfile)
    readr::write_csv(c19f_methods, outfile)
  }

  ## return tibble regardless of whether or not the output is written
  return(c19f_methods)

}


#' Helper to get the date for the Monday of the current week
#'
#' @return Date for the Monday of the current week. For more details see \link[lubridate]{floor_date}.
#' @export
#'
this_monday <- function() {
  lubridate::floor_date(lubridate::today(), "weeks", week_start = 1)
}

#' Helper function to see if today is Monday
#'
#' \lifecycle{experimental}
#'
#' @return Logical indicating whether or not today is Monday
#' @export
is_monday <- function() {
  lubridate::wday(lubridate::today(), label=TRUE) %in% c("Mon")
}

#' Pipeline to produce a forecast
#'
#' @description
#' \lifecycle{experimental}
#'
#' Runs the pipeline with reasonable defaults and some hard-coded values to do the following. See the Examples. For now this function only works on Mondays!
#' 1. Get data (national level from JHU by default)
#' 1. Fit incident case and incident death models (ARIMA and lagged TSLM respectively)
#' 1. Get future case data to create the incident death forecast
#' 1. Create the incident death forecast based on this new data
#' 1. Prepare submission format
#' 1. Suggest a submission filename
#' 1. Return all resulting objects to a list.
#'
#' @param method Forecasting method to use; currently only `'ts'` (time series) is supported
#' @param source Data source to query; must be one of `'jhu'` or `'nyt'`; default is `'jhu'`
#' @param granularity Data aggregation level; must be one of `'national'`, `'state'`, or `'county'`; if data source is `'nyt'` then only `'national'` can be used currently; default is `'national'`
#' @param horizon Horizon periods through which the forecasts should be generated; default is `4`
#' @param force Logical -- force the pipeline to run even if it isn't Monday? (default is `FALSE`; changing to `TRUE` may break something if today isn't Monday).
#' @param ... Arguments passed to other functions
#'
#' @examples
#' \dontrun{
#' # Run all the steps to create models and forecasts
#' myforecast <- forecast_pipeline()
#' # Look at the submission and the suggested filename.
#' myforecast$submission
#' myforecast$submission_filename
#' # Write submission to file
#' readr::write_csv(myforecast$submission, file=myforecast$submission_filename)
#' # Validate submission
#' validate_forecast(myforecast$submission_filename)
#' }
#' @md
#' @export
forecast_pipeline <- function(method = "ts", source="jhu", granularity="national", horizon=4, force=FALSE, ...) {

  # If it isn't monday and you haven't set FORCE=TRUE, then don't run the code.
  if (!is_monday()) {
    if (!force) {
      stop("Try again on Monday or set `force=TRUE`.")
    } else {
      warning("Forcing forecast on a non-Monday. This may not validate. Proceeding...")
    }
  } else {
    message("Today is a Monday, proceeding with forecasting...")
  }

  # Get data
  message("Getting data...")
  usac <-  get_cases(source=source, granularity=granularity)
  usad <- get_deaths(source=source, granularity=granularity)
  usa <-
    dplyr::inner_join(usac, usad, by = c("location", "epiyear", "epiweek")) %>%
    make_tsibble(...)

  if(method == "ts") {
    # Fit incident deaths and incident cases
    message("Fitting incident death and case models...")
    fit.icases <-  usa %>% fabletools::model(arima = fable::ARIMA(icases, stepwise=FALSE, approximation=FALSE))
    fit.ideaths <- usa %>% fabletools::model(linear_caselag3 = fable::TSLM(ideaths ~ lag(icases, 3)))

    ## Generate incident case forecast
    message("Generating incident case forecast...")
    icases_forecast <- ts_forecast(fit.icases, outcome = "icases", horizon = horizon)

    ## Get future cases to pass to ideaths forecast
    message("Generating future case data for incident death forecast...")
    future_cases <- ts_futurecases(usa, icases_forecast, horizon = horizon)

    # Forecast incident deaths based on best guess for cases
    message("Generating incident death forecast...")
    ideaths_forecast <- ts_forecast(fit.ideaths,  outcome = "ideaths", new_data = future_cases)

    ## Generate cumulative death forecast from incident death forecast created above
    message("Generating cumulative death forecast...")
    cdeaths_forecast <- ts_forecast(outcome = "cdeaths", .data = usa, inc_forecast = ideaths_forecast)
  } else {
    stop("Currently only time series (method='ts') is supported ...")
  }

  ## create submission object
  message("Formatting data for submission...")
  submission <-
    list(format_for_submission(icases_forecast, target_name = "inc case"),
         format_for_submission(ideaths_forecast, target_name = "inc death"),
         format_for_submission(cdeaths_forecast, target_name = "cum death")) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    dplyr::arrange(target)

  # Suggested submission filename
  submission_filename <- here::here("submission", "SigSci-TS", paste0(Sys.Date(), "-SigSci-TS.csv"))

  # Create and return output
  out <- list(data=usa,
              fit.icases=fit.icases,
              fit.ideaths=fit.ideaths,
              icases_forecast=icases_forecast,
              future_cases=future_cases,
              ideaths_forecast=ideaths_forecast,
              cdeaths_forecast=cdeaths_forecast,
              submission=submission,
              submission_filename=submission_filename)

  message("Done!")
  return(out)
}


#' Visualize and sanity check a forecast
#'
#' #' @description
#' \lifecycle{experimental}
#'
#' @param .data Data used to create the submission
#' @param submission Formatted submission
#' @param location Vector specifying locations to filter to; "US" by default.
#' @param pi Logical as to whether or not the plot should include 50% prediction interval; default is `TRUE`
#'
#' @examples
#' \dontrun{
#' myforecast <- forecast_pipeline(force=TRUE)
#' plot_forecast(data=myforecast$data, submission=myforecast$submission, location="US")
#' }
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

  # Plot
  p <-
    ggplot2::ggplot(bound, ggplot2::aes(date, point)) +
    ggplot2::geom_line(ggplot2::aes(col=type)) +
    ggplot2::facet_wrap(~location + target, scales="free", ncol = 4) +
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

