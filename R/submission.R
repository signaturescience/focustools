#' Check for valid quantile csv file input
#'
#' @description
#'
#' The submission file for the COVID-19 Forecast Hub must adhere to requirements for file format, column names, target identifiers, and date ranges for horizons. The organizers include Python scripts to validate weekly submission data. This function provides an R wrapper for one of the validation methods from the `zoltpy` Python module. In order to wrap the Python functionality, the function calls `reticulate` internally to attach the Python environment with `zoltpy` installed. Any changes made upstream (in `zoltpy` release on PyPi repository) will be propagated to this function given a fresh module installation (see "install" argument).
#'
#' @param filename Full path to the forecast file to be checked
#' @param verbose Logical indicating whether or not the output from this function should include validation message; default `TRUE`
#' @param install Logical as to whether or not the python dependencies should be installed; if `TRUE` the module will be installed to the virtual environment specified in "envname"; default is `FALSE`
#' @param envname Character vector specifying the name of the virtualenv to which the python dependencies should be installed if `install = TRUE`; default is `NULL` which will install the module to a virtualenv named `r-reticulate`
#'
#' @return If `verbose = FALSE`, the returned value will be a boolean with `TRUE` for valid submission file and `FALSE` for invalid file. If `verbose = FALSE`, the function will return a named list with two elements: "valid" (boolean with the `TRUE`/`FALSE` validation code) and "message" (the output from the `zoltpy valid_quantile_csv_file()` function).
#'
#' @references https://pypi.org/project/zoltpy/
#' @references https://covid19forecasthub.org/
#'
#' @export
#' @md
validate_forecast <- function(filename, verbose = TRUE, install = FALSE, envname = NULL) {

  deps <- c("pandas","requests","git+https://github.com/reichlab/zoltpy/","click","pymmwr")

  ## uses install method from PyPi
  ## installs to virtualenv (optionally named with envname)
  if(install) {
    if(!is.null(envname)) {
      reticulate::py_install(packages = deps, envname = envname, pip = TRUE)
    } else {
      reticulate::py_install(packages = deps, pip = TRUE)
    }
  }

  ## load the validate_quantile function into the main py
  reticulate::py_run_string("from zoltpy.covid19 import validate_quantile_csv_file")

  ## use the validate_quantile_csv_file() function imported above
  validate_msg <- reticulate::py$validate_quantile_csv_file(filename)
  ## collapse msg for legibility
  validate_msg <- paste0(validate_msg, collapse = "\n")
  ## check if valid (msg should == 'no errors')
  res <- ifelse(validate_msg == "no errors", TRUE, FALSE)

  ## optionally output 'valid' and 'messsge' as named list
  if(verbose) {
    return(list(valid = res, message = validate_msg))
  } else {
    return(res)
  }
}


#' Format forecast for COVID-19 Forecast Hub submission entry
#'
#' @description
#'
#' The submission file for the COVID-19 Forecast Hub must adhere to requirements for file format, column names, target identifiers, and date ranges for horizons. This function takes output from a `focustools` forecasting function (e.g. \link[focustools]{ts_forecast}) and prepares an appropriately formatted object that can be written to a file. Formatting steps include constructing a valid string for horizon and target name (e.g. '3 wk ahead inc case'), computing the 'target_end_date' value based on the epidemiological week for the horizon, filtering distributional cutpoints for certain targets ('inc case' only needs 7 of the quantiles), converting all estimates to integers, and bounding all predicted values at minimum of 0.
#'
#' @param .forecast Forecast object
#' @param target_name Name of the target for the forecast; must be one of `'inc case'`, `'inc death'`, or `'cum death'`
#'
#' @return A `tibble` with target names and quantiles/point estimates formatted per the COVID-19 Forecast Hub submission guidelines.
#' @export
#'
#' @md
#' @references https://covid19forecasthub.org/
#'
format_for_submission <- function(.forecast, target_name) {

  # Check for the correct target type
  stopifnot(target_name %in% c("inc case", "inc death", "cum death"))

  bound <-
    .forecast %>%
    dplyr::select(.model:type) %>%
    dplyr::arrange(type, quantile, yweek) %>%
    ## processing to get horizon N
    dplyr::group_by(yweek) %>%
    dplyr::mutate(N=dplyr::cur_group_id()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(target=glue::glue("{N} wk ahead {target_name}")) %>%
    dplyr::select(-N) %>%
    # Fix dates: as_date(yweek) returns the MONDAY that starts that week. add 5 days to get the Saturday date.
    dplyr::mutate(target_end_date=lubridate::as_date(yweek)+lubridate::days(5)) %>%
    dplyr::mutate(forecast_date=lubridate::today()) %>%
    dplyr::select(forecast_date, target, target_end_date, location, type, quantile, value)

  # restrict inc case quantiles to c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)
  if (target_name=="inc case") {
    bound <- bound %>%
      dplyr::filter(type=="point" | round(quantile, 3)  %in% c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975))
  }

  # Found quantile and make value integer (don't need/want precision / half-people)
  bound <-
    bound %>%
    dplyr::mutate(quantile=round(quantile, 4)) %>%
    dplyr::mutate(value=as.integer(round(value)))

  # Don't allow negative values
  bound <-
    bound %>%
    dplyr::mutate(value=ifelse(value<0, 0L, value))

  return(bound)
}


#' Summarize submission
#'
#' @description
#'
#' This function summarizes and reformats submission data as 4-week ahead counts and percent change. The summaries are stratified by location and target (incident cases, incident deaths, and cumulative deaths).
#'
#' @param .data Tibble with historical data for trend leading up to forecast
#' @param submission Formatted submission
#' @param location Vector specifying locations to filter to; `NULL` by default meaning all locations will be used
#'
#' @return Named list with summarized count and percent change data. Each summary is stratified by target and returned in the list as a `tibble` with columns for "location", "Previous" (value week prior to forecast), "1w ahead", "2w ahead", 3w ahead, and "4w ahead".
#'
#' @export
#' @md
submission_summary <- function(.data, submission, location = NULL) {

  if(!is.null(location)) {
    loc_name <- location
    submission <-
      submission %>%
      dplyr::filter(location %in% loc_name)
  }

  ## get epiweek and epiyear for week before based on submission data
  ## this will be used find event count to determine 1wk horizon % change
  submission_ew <- min(lubridate::epiweek(submission$target_end_date))
  submission_ey <- min(lubridate::epiyear(submission$target_end_date))

  previous_ew <- ifelse(submission_ew == 1, 53, submission_ew - 1)
  previous_ey <- ifelse(submission_ew == 1, submission_ey - 1, submission_ey)

  previous_week <-
    .data %>%
    dplyr::as_tibble() %>%
    dplyr::group_by(location) %>%
    ## restrict to appropriate epiyear/epiweek for week prior to submission
    dplyr::filter(epiyear == previous_ey, epiweek == previous_ew) %>%
    ## add a column for horizon 0 so we can stack on submission data (see below)
    dplyr::mutate(horizon = as.character(0)) %>%
    dplyr::select(horizon, location, icases, ideaths, cdeaths)

  ## take the submission data ...
  tmp_counts <-
    submission %>%
    ## restrict to point estimates
    dplyr::filter(type == "point") %>%
    ## only need target value and location columns
    dplyr::select(target, value, location) %>%
    ## string manip to get the horizon and target name separated
    tidyr::separate(., target, into = c("horizon", "target"), sep = "wk ahead") %>%
    dplyr::mutate(horizon = stringr::str_trim(horizon, "both"),
                  target = stringr::str_trim(target, "both")) %>%
    ## clean up taret name
    dplyr::mutate(target =
                    dplyr::case_when(
                      target == "inc case" ~ "icases",
                      target == "inc death" ~ "ideaths",
                      target == "cum death" ~ "cdeaths")) %>%
    ## reshape wide
    tidyr::spread(target, value) %>%
    ## stack on top of the "previous week" data
    dplyr::bind_rows(previous_week) %>%
    ## must sort by horizon and location so that window lag function below will work
    dplyr::arrange(horizon, location) %>%
    ## reshape long again
    tidyr::gather(target, value, cdeaths:ideaths)

  ## formatting for percentage difference
  tmp_perc_diff <-
    tmp_counts %>%
    ## need to do the window function stuff by unique combo of location and target
    dplyr::group_by(location, target) %>%
    ## figure out the % change
    dplyr::mutate(diff = value / dplyr::lag(value)) %>%
    ## drop the horizon 0 (previous week) since we don't need it any more
    dplyr::filter(horizon != 0) %>%
    dplyr::mutate(diff = ifelse(diff < 1, -1*abs(1-diff), abs(1-diff))) %>%
    dplyr::mutate(diff = diff*100) %>%
    dplyr::mutate(diff = paste0(as.character(round(diff, 1)), "%")) %>%
    dplyr::select(-value) %>%
    dplyr::mutate(horizon = ifelse(horizon == 0, "Previous", paste0(horizon, "w ahead"))) %>%
    dplyr::group_by(target)

  ## get names for each target from group keys
  ## used to name the list below ...
  target_names <-
    tmp_perc_diff %>%
    dplyr::group_keys() %>%
    dplyr::pull(target)

  perc_diff <-
    tmp_perc_diff %>%
    dplyr::group_split(., .keep = FALSE) %>%
    purrr::map(., .f = function(x) spread_value(x, horizon, diff)) %>%
    purrr::set_names(target_names)

  ## formatting for counts
  tmp_counts <-
    tmp_counts %>%
    dplyr::mutate(horizon = ifelse(horizon == 0, "Previous", paste0(horizon, "w ahead"))) %>%
    dplyr::group_by(target)

  target_names <-
    tmp_counts %>%
    dplyr::group_keys() %>%
    dplyr::pull(target)

  counts <-
    tmp_counts %>%
    dplyr::group_split(., .keep = FALSE) %>%
    purrr::map(., .f = function(x) spread_value(x, horizon, value)) %>%
    purrr::set_names(target_names)


  return(list(counts = counts, perc_diff = perc_diff))

}
