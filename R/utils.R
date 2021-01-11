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
    tsibble::as_tsibble(index=yweek)
  # Remove the incomplete week
  if (chop) out <- out %>% dplyr::filter(lubridate::week(monday)!=lubridate::week(lubridate::today()))
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

