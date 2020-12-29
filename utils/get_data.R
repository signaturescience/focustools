## TODO: check weekly case count logic (i.e. disentangling cumulative cases) is working
## TODO: flesh out cache = TRUE idea (option to use pre-downloaded object)

get_cases <- function(source = "jhu", granularity = "national", cache = TRUE) {

  if(source == "jhu") {
    ## first read in data
    ## need this to get number of columns and indices for reshaping (see below)
    dat <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")

    ind <- which(names(dat) == "1/22/20")

    dat <-
      dat %>%
      tidyr::gather(date, count, ind:ncol(dat)) %>%
      ## drop unnecessary columns
      dplyr::select(-iso2,-code3,-Country_Region) %>%
      dplyr::mutate(date = as.Date(date, format = "%m/%d/%y")) %>%
      dplyr::mutate(epiyear=MMWRweek::MMWRweek(date)$MMWRyear, .after=date) %>%
      dplyr::mutate(epiweek=MMWRweek::MMWRweek(date)$MMWRweek, .after=epiyear) %>%
      dplyr::rename(county = Admin2, fips = FIPS, state = Province_State) %>%
      dplyr::group_by(county, fips, state) %>%
      dplyr::arrange(date) %>%
      ## coerce from cumulative to incident cases
      ## hold onto count as "ccases" for cumulative cases
      dplyr::mutate(icases = count - lag(count, default = 0L),
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
        dplyr::ungroup()
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
        ## NOTE: for now this only keeps state names (not territories)
        dplyr::filter(state %in% datasets::state.name)
    } else if (granularity == "national") {
      ## by usa
      dat <-
        dat %>%
        dplyr::group_by(epiyear, epiweek) %>%
        dplyr::summarise(icases = sum(icases, na.rm=TRUE), .groups="drop") %>%
        dplyr::mutate(ccases = cumsum(icases))
    }

  } else if (source == "nyt") {
    dat <-
      readr::read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv", col_types="Dii") %>%
      dplyr::mutate(epiyear=MMWRweek::MMWRweek(date)$MMWRyear, .after=date) %>%
      dplyr::mutate(epiweek=MMWRweek::MMWRweek(date)$MMWRweek, .after=epiyear) %>%
      dplyr::mutate(icases  = cases  - lag(cases, default = 0L),
                    ccases = cases)

    ## by usa
    if(granularity == "national") {
      dat <-
        dat %>%
        dplyr::group_by(epiyear, epiweek) %>%
        dplyr::summarise(icases = sum(icases, na.rm=TRUE), .groups="drop") %>%
        dplyr::arrange(epiyear,epiweek) %>%
        dplyr::mutate(ccases = cumsum(icases))
    } else {
      stop("for source='nyt' granularity must be 'national' (still working on incorporating 'county' and 'state') ... ")
    }

  }
  return(dat)
}


get_deaths <- function(source = "jhu", granularity = "national", cache = TRUE) {

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
      dplyr::mutate(epiyear=MMWRweek::MMWRweek(date)$MMWRyear, .after=date) %>%
      dplyr::mutate(epiweek=MMWRweek::MMWRweek(date)$MMWRweek, .after=epiyear) %>%
      dplyr::rename(county = Admin2, fips = FIPS, state = Province_State) %>%
      dplyr::group_by(county, fips, state) %>%
      dplyr::arrange(date) %>%
      ## coerce from cumulative to incident deaths
      ## hold onto count as "cdeaths" for cumulative deaths
      dplyr::mutate(ideaths = count - lag(count, default = 0L),
                    cdeaths = count)

    ## by county
    if (granularity == "county") {
      dat <-
        dat %>%
        ## within each county (fips code), year, week grouping
        dplyr::group_by(fips,epiyear,epiweek) %>%
        dplyr::summarise(ideaths = sum(ideaths, na.rm = TRUE), .groups = "drop") %>%
        filter(epiweek != lubridate::week(Sys.Date())) %>%
        mutate(date = as.Date(paste(epiyear, epiweek, 1, sep="-"), "%Y-%U-%u")) %>%
        group_by(fips) %>%
        arrange(fips,date) %>%
        mutate(cdeaths = cumsum(ideaths)) %>%
        ungroup()
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
        ## NOTE: for now this only keeps state names (not territories)
        dplyr::filter(state %in% datasets::state.name)
    } else if (granularity == "national") {
      ## by usa
      dat <-
        dat %>%
        dplyr::group_by(epiyear, epiweek) %>%
        dplyr::summarise(ideaths = sum(ideaths, na.rm=TRUE), .groups="drop") %>%
        mutate(cdeaths = cumsum(ideaths))
    }

  } else if (source == "nyt") {
    dat <-
      readr::read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv", col_types="Dii") %>%
      dplyr::mutate(epiyear=MMWRweek::MMWRweek(date)$MMWRyear, .after=date) %>%
      dplyr::mutate(epiweek=MMWRweek::MMWRweek(date)$MMWRweek, .after=epiyear) %>%
      dplyr::mutate(ideaths  = deaths - lag(deaths, default = 0L),
                    cdeaths = deaths)

    ## by usa
    if(granularity == "national") {
      dat <-
        dat %>%
        dplyr::group_by(epiyear, epiweek) %>%
        dplyr::summarise(ideaths = sum(ideaths, na.rm=TRUE), .groups="drop") %>%
        dplyr::arrange(epiyear,epiweek) %>%
        dplyr::mutate(cdeaths = cumsum(ideaths))
    } else {
      stop("for source='nyt' granularity must be 'national' (still working on incorporating 'county' and 'state') ... ")
    }

  }
  return(dat)
}
