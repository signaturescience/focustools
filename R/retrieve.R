#' Retrieve case count data
#'
#' @param source Data source to query; must be one of `'jhu'` or `'nyt'`; default is `'jhu'`
#' @param granularity Data aggregation level; must be one of `'national'`, `'state'`, or `'county'`; if data source is `'nyt'` then only `'national'` can be used currently; default is `'national'`
#'
#' @return A `tibble` with (at minimum) the following columns:
#' - **epiyear**: Epidemiological year (see \link[lubridate]{epiyear} for more details)
#' - **epiweek**: Epidemiological week (see \link[lubridate]{epiweek} for more details)
#' - **icases**: Incident case count
#' - **ccases**: Cumulative case count
#'
#' If `source = 'jhu'` and `granularity = 'state'` then the data will include column for **state** (full name of the state) and **date**. If `source = 'jhu'` and `granularity = 'county'` then the data will include column for **fips** (county code) and **date**.
#'
#' @export
#' @md
#'
#' @references https://github.com/CSSEGISandData/COVID-19
#' @references https://github.com/nytimes/covid-19-data
#'
get_cases <- function(source = "jhu", granularity = "national") {

  if(source == "jhu") {
    ## first read in data
    ## need this to get number of columns and indices for reshaping (see below)
    dat <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")

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
        dplyr::filter(epiweek != lubridate::week(Sys.Date())) %>%
        dplyr::mutate(date = as.Date(paste(epiyear, epiweek, 1, sep="-"), "%Y-%U-%u")) %>%
        dplyr::group_by(fips) %>%
        dplyr::arrange(fips,date) %>%
        dplyr::mutate(ccases = cumsum(icases)) %>%
        dplyr::ungroup() %>%
        dplyr::rename(location = fips)
    } else if(granularity == "state") {
      dat <-
        dat %>%
        ## within each state, year, week grouping
        dplyr::group_by(state,epiyear,epiweek) %>%
        ## sum up the number of deaths at that week
        dplyr::summarise(icases = sum(icases, na.rm = TRUE), .groups = "drop") %>%
        ## ignore the current week because it will likely be incomplete ...
        dplyr::filter(epiweek != lubridate::week(Sys.Date())) %>%
        ## create a variable for date using epiyear and week
        dplyr::mutate(date = as.Date(paste(epiyear, epiweek, 1, sep="-"), "%Y-%U-%u")) %>%
        ## now group by statee
        dplyr::group_by(state) %>%
        ## arrange by state first then date (ascending)
        dplyr::arrange(state,date) %>%
        ## then use the arranged data and cumsum to get at the cumulative deaths at each week/state
        dplyr::mutate(ccases = cumsum(icases)) %>%
        dplyr::ungroup() %>%
        dplyr::rename(location = state)
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
  return(dat)
}

#' Retrieve deaths data
#'
#' @param source Data source to query; must be one of `'jhu'` or `'nyt'`; default is `'jhu'`
#' @param granularity Data aggregation level; must be one of `'national'`, `'state'`, or `'county'`; if data source is `'nyt'` then only `'national'` can be used currently; default is `'national'`
#'
#' @return A `tibble` with (at minimum) the following columns:
#' - **epiyear**: Epidemiological year (see \link[lubridate]{epiyear} for more details)
#' - **epiweek**: Epidemiological week (see \link[lubridate]{epiweek} for more details)
#' - **ideaths**: Incident case count
#' - **cdeaths**: Cumulative case count
#'
#' If `source = 'jhu'` and `granularity = 'state'` then the data will include column for **state** (full name of the state) and **date**. If `source = 'jhu'` and `granularity = 'county'` then the data will include column for **fips** (county code) and **date**.
#'
#' @export
#' @md
#'
get_deaths <- function(source = "jhu", granularity = "national") {

  if(source == "jhu") {
    ## first read in data
    ## need this to get number of columns and indices for reshaping (see below)
    dat <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")

    ind <- which(names(dat) == "1/22/20")

    dat <-
      dat %>%
      tidyr::gather(date, count, ind:ncol(dat)) %>%
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
        dplyr::filter(epiweek != lubridate::week(Sys.Date())) %>%
        dplyr::mutate(date = as.Date(paste(epiyear, epiweek, 1, sep="-"), "%Y-%U-%u")) %>%
        dplyr::group_by(fips) %>%
        dplyr::arrange(fips,date) %>%
        dplyr::mutate(cdeaths = cumsum(ideaths)) %>%
        dplyr::ungroup() %>%
        dplyr::rename(location = fips)
    } else if(granularity == "state") {
      dat <-
        dat %>%
        ## within each state, year, week grouping
        dplyr::group_by(state,epiyear,epiweek) %>%
        ## sum up the number of deaths at that week
        dplyr::summarise(ideaths = sum(ideaths, na.rm = TRUE), .groups = "drop") %>%
        ## ignore the current week because it will likely be incomplete ...
        dplyr::filter(epiweek != lubridate::week(Sys.Date())) %>%
        ## create a variable for date using epiyear and week
        dplyr::mutate(date = as.Date(paste(epiyear, epiweek, 1, sep="-"), "%Y-%U-%u")) %>%
        ## now group by statee
        dplyr::group_by(state) %>%
        ## arrange by state first then date (ascending)
        dplyr::arrange(state,date) %>%
        ## then use the arranged data and cumsum to get at the cumulative deaths at each week/state
        dplyr::mutate(cdeaths = cumsum(ideaths)) %>%
        dplyr::ungroup() %>%
        dplyr::rename(location = state)
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
  return(dat)
}
