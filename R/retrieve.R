#' Retrieve data
#'
#' @name retrieve
#'
#' @description
#'
#' The package includes functions to retrieve observed data from two canonical sources: the New York Times and the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University. Both organizations administer data aggregation efforts that post daily COVID-19 case and death to GitHub. The data retrieval functions allow the user to specify "source" and "granularity" of data. Internally each function builds a path to the appropriate `.csv` file on GitHub, then reads the data into memory. The returned object is data aggregated weekly (using \link[lubridate]{epiweek} and \link[lubridate]{epiyear} designations) for available locations at the granularity specified (national, state, or county level).
#'
#' @param source Data source to query; must be one of `'jhu'` or `'nyt'`; default is `'jhu'`
#' @param granularity Data aggregation level; must be one of `'national'`, `'state'`, or `'county'`; if data source is `'nyt'` then only `'national'` can be used currently; default is `'national'`
#'
#' @return A `tibble` with (at minimum) the following columns:
#' - **epiyear**: Epidemiological year (see \link[lubridate]{epiyear} for more details)
#' - **epiweek**: Epidemiological week (see \link[lubridate]{epiweek} for more details)
#' - **icases/ideaths**: Incident counts (cases or deaths)
#' - **ccases/cdeaths**: Cumulative counts (cases or deaths)
#'
#' If `source = 'jhu'` and `granularity = 'state'` then the **location** column  will include the full name of the state. If `source = 'jhu'` and `granularity = 'county'` then the **location** column  will include fips (county code).
#'
#' @md
#'
#' @references https://github.com/CSSEGISandData/COVID-19
#' @references https://github.com/nytimes/covid-19-data
#'

#' @rdname retrieve
#' @export
get_cases <- function(source = "jhu", granularity = "national") {

  if(source == "jhu") {
    ## first read in data
    jhuspec <- readr::cols(
      .default = readr::col_double(),
      iso2 = readr::col_character(),
      iso3 = readr::col_character(),
      Admin2 = readr::col_character(),
      Province_State = readr::col_character(),
      Country_Region = readr::col_character(),
      Combined_Key = readr::col_character())
    jhuurl <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
    dat <- readr::read_csv(jhuurl, col_types=jhuspec)

    ## need this to get number of columns and indices for reshaping (see below)
    ind <- which(names(dat) == "1/22/20")

    dat <-
      dat %>%
      tidyr::gather(date, count, dplyr::all_of(ind:ncol(dat))) %>%
      ## drop unnecessary columns
      dplyr::select(-iso2,-code3,-Country_Region) %>%
      dplyr::mutate(date = as.Date(date, format = "%m/%d/%y")) %>%
      dplyr::mutate(epiyear=lubridate::epiyear(date), .after=date) %>%
      dplyr::mutate(epiweek=lubridate::epiweek(date), .after=epiyear) %>%
      dplyr::rename(county = Admin2, fips = FIPS, state = Province_State) %>%
      dplyr::group_by(county, fips, state) %>%
      dplyr::arrange(date) %>%
      ## coerce from cumulative to incident cases
      ## hold onto count as "ccases" for cumulative cases
      dplyr::mutate(icases = count - dplyr::lag(count, default = 0L),
                    ccases = count)


    ## by county
    if (granularity == "county") {
      dat <-
        dat %>%
        ## within each county (fips code), year, week grouping
        dplyr::group_by(fips,epiyear,epiweek) %>%
        dplyr::summarise(icases = sum(icases, na.rm = TRUE), .groups = "drop") %>%
        ## ignore the current week because it will likely be incomplete ...
        dplyr::mutate(yweek_tmp = paste0(epiyear, "-", epiweek)) %>%
        dplyr::filter((yweek_tmp != paste0(lubridate::epiyear(Sys.Date()), "-", lubridate::epiweek(Sys.Date())))) %>%
        dplyr::select(-yweek_tmp) %>%        dplyr::group_by(fips) %>%
        dplyr::arrange(fips,epiyear,epiweek) %>%
        dplyr::mutate(ccases = cumsum(icases)) %>%
        dplyr::ungroup() %>%
        dplyr::rename(location = fips) %>%
        dplyr::mutate(location = stringr::str_pad(location, 5, side = "left", pad = "0"))
    } else if(granularity == "state") {
      dat <-
        dat %>%
        ## within each state, year, week grouping
        dplyr::group_by(state,epiyear,epiweek) %>%
        ## sum up the number of deaths at that week
        dplyr::summarise(icases = sum(icases, na.rm = TRUE), .groups = "drop") %>%
        ## ignore the current week because it will likely be incomplete ...
        dplyr::mutate(yweek_tmp = paste0(epiyear, "-", epiweek)) %>%
        dplyr::filter((yweek_tmp != paste0(lubridate::epiyear(Sys.Date()), "-", lubridate::epiweek(Sys.Date())))) %>%
        dplyr::select(-yweek_tmp) %>%
        ## now group by state
        dplyr::group_by(state) %>%
        ## arrange by state first then date (ascending)
        dplyr::arrange(state,epiyear,epiweek) %>%
        ## then use the arranged data and cumsum to get at the cumulative deaths at each week/state
        dplyr::mutate(ccases = cumsum(icases)) %>%
        dplyr::ungroup() %>%
        ## make sure we don't have any bogus "states/territories"
        dplyr::filter(!state %in% c("Diamond Princess", "Grand Princess")) %>%
        dplyr::rename(location_name = state) %>%
        dplyr::left_join(dplyr::select(locations, location, location_name), by = "location_name") %>%
        dplyr::select(-location_name)
    } else if (granularity == "national") {
      ## by usa
      dat <-
        dat %>%
        dplyr::group_by(epiyear, epiweek) %>%
        dplyr::summarise(icases = sum(icases, na.rm=TRUE), .groups="drop") %>%
        dplyr::mutate(ccases = cumsum(icases)) %>%
        dplyr::mutate(location = "US")
    }

  } else if (source == "nyt") {
    dat <-
      readr::read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv", col_types="Dii") %>%
      dplyr::mutate(epiyear=lubridate::epiyear(date), .after=date) %>%
      dplyr::mutate(epiweek=lubridate::epiweek(date), .after=epiyear) %>%
      dplyr::mutate(icases  = cases  - dplyr::lag(cases, default = 0L),
                    ccases = cases)

    ## by usa
    if(granularity == "national") {
      dat <-
        dat %>%
        dplyr::group_by(epiyear, epiweek) %>%
        dplyr::summarise(icases = sum(icases, na.rm=TRUE), .groups="drop") %>%
        dplyr::arrange(epiyear,epiweek) %>%
        dplyr::mutate(ccases = cumsum(icases)) %>%
        dplyr::mutate(location = "US")
    } else {
      stop("for source='nyt' granularity must be 'national' (still working on incorporating 'county' and 'state') ... ")
    }

  }

  ## check that icases is NEVER negative at the weekly aggregate
  dat <-
    dat %>%
    dplyr::mutate(icases = ifelse(icases < 0, 0, icases))

  return(dat)
}

#' @rdname retrieve
#' @export
get_deaths <- function(source = "jhu", granularity = "national") {

  if(source == "jhu") {
    ## first read in data
    ## need this to get number of columns and indices for reshaping (see below)
    jhuspec <- readr::cols(
      .default = readr::col_double(),
      iso2 = readr::col_character(),
      iso3 = readr::col_character(),
      Admin2 = readr::col_character(),
      Province_State = readr::col_character(),
      Country_Region = readr::col_character(),
      Combined_Key = readr::col_character())
    jhuurl <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
    dat <- readr::read_csv(jhuurl, col_types=jhuspec)
    ind <- which(names(dat) == "1/22/20")

    dat <-
      dat %>%
      tidyr::gather(date, count, dplyr::all_of(ind:ncol(dat))) %>%
      ## drop unnecessary columns
      dplyr::select(-iso2,-code3,-Country_Region) %>%
      dplyr::mutate(date = as.Date(date, format = "%m/%d/%y")) %>%
      dplyr::mutate(epiyear=lubridate::epiyear(date), .after=date) %>%
      dplyr::mutate(epiweek=lubridate::epiweek(date), .after=epiyear) %>%
      dplyr::rename(county = Admin2, fips = FIPS, state = Province_State) %>%
      dplyr::group_by(county, fips, state) %>%
      dplyr::arrange(date) %>%
      ## coerce from cumulative to incident deaths
      ## hold onto count as "cdeaths" for cumulative deaths
      dplyr::mutate(ideaths = count - dplyr::lag(count, default = 0L),
                    cdeaths = count)

    ## by county
    if (granularity == "county") {
      dat <-
        dat %>%
        ## within each county (fips code), year, week grouping
        dplyr::group_by(fips,epiyear,epiweek) %>%
        dplyr::summarise(ideaths = sum(ideaths, na.rm = TRUE), .groups = "drop") %>%
        ## ignore the current week because it will likely be incomplete ...
        dplyr::mutate(yweek_tmp = paste0(epiyear, "-", epiweek)) %>%
        dplyr::filter((yweek_tmp != paste0(lubridate::epiyear(Sys.Date()), "-", lubridate::epiweek(Sys.Date())))) %>%
        dplyr::select(-yweek_tmp) %>%
        dplyr::group_by(fips) %>%
        dplyr::arrange(fips,epiyear,epiweek) %>%
        dplyr::mutate(cdeaths = cumsum(ideaths)) %>%
        dplyr::ungroup() %>%
        dplyr::rename(location = fips) %>%
        dplyr::mutate(location = stringr::str_pad(location, 5, side = "left", pad = "0"))
    } else if(granularity == "state") {
      dat <-
        dat %>%
        ## within each state, year, week grouping
        dplyr::group_by(state,epiyear,epiweek) %>%
        ## sum up the number of deaths at that week
        dplyr::summarise(ideaths = sum(ideaths, na.rm = TRUE), .groups = "drop") %>%
        ## ignore the current week because it will likely be incomplete ...
        dplyr::mutate(yweek_tmp = paste0(epiyear, "-", epiweek)) %>%
        dplyr::filter((yweek_tmp != paste0(lubridate::epiyear(Sys.Date()), "-", lubridate::epiweek(Sys.Date())))) %>%
        dplyr::select(-yweek_tmp) %>%
        ## now group by statee
        dplyr::group_by(state) %>%
        ## arrange by state first then date (ascending)
        dplyr::arrange(state,epiyear,epiweek) %>%
        ## then use the arranged data and cumsum to get at the cumulative deaths at each week/state
        dplyr::mutate(cdeaths = cumsum(ideaths)) %>%
        dplyr::ungroup() %>%
        ## make sure we don't have any bogus "states/territories"
        dplyr::filter(!state %in% c("Diamond Princess", "Grand Princess")) %>%
        dplyr::rename(location_name = state) %>%
        dplyr::left_join(dplyr::select(locations, location, location_name), by = "location_name") %>%
        dplyr::select(-location_name)
    } else if (granularity == "national") {
      ## by usa
      dat <-
        dat %>%
        dplyr::group_by(epiyear, epiweek) %>%
        dplyr::summarise(ideaths = sum(ideaths, na.rm=TRUE), .groups="drop") %>%
        dplyr::mutate(cdeaths = cumsum(ideaths)) %>%
        dplyr::mutate(location = "US")
    }

  } else if (source == "nyt") {
    dat <-
      readr::read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv", col_types="Dii") %>%
      dplyr::mutate(epiyear=lubridate::epiyear(date), .after=date) %>%
      dplyr::mutate(epiweek=lubridate::epiweek(date), .after=epiyear) %>%
      dplyr::mutate(ideaths  = deaths - dplyr::lag(deaths, default = 0L),
                    cdeaths = deaths)

    ## by usa
    if(granularity == "national") {
      dat <-
        dat %>%
        dplyr::group_by(epiyear, epiweek) %>%
        dplyr::summarise(ideaths = sum(ideaths, na.rm=TRUE), .groups="drop") %>%
        dplyr::arrange(epiyear,epiweek) %>%
        dplyr::mutate(cdeaths = cumsum(ideaths)) %>%
        dplyr::mutate(location = "US")
    } else {
      stop("for source='nyt' granularity must be 'national' (still working on incorporating 'county' and 'state') ... ")
    }

  }

  ## check that ideaths is NEVER negative at the weekly aggregate
  dat <-
    dat %>%
    dplyr::mutate(ideaths = ifelse(ideaths < 0, 0, ideaths))

  return(dat)
}
