## TODO: add a way to optionally summarize by county or state
## TODO: check weekly case count logic (i.e. disentangling cumulative cases) is working
## TODO: add get_deaths() function
## TODO: flesh out cache = TRUE idea (option to use pre-downloaded object)

get_cases <- function(source = "jhu", cache = TRUE) {

  if(source == "jhu") {
    dat <-
      readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") %>%
      tidyr::gather(date, count, ind:ncol(jhu_cases)) %>%
      ## drop unnecessary columns
      dplyr::select(-iso2,-code3,-Country_Region) %>%
      dplyr::mutate(date = as.Date(date, format = "%m/%d/%y")) %>%
      dplyr::mutate(epiyear=MMWRweek::MMWRweek(date)$MMWRyear, .after=date) %>%
      dplyr::mutate(epiweek=MMWRweek::MMWRweek(date)$MMWRweek, .after=epiyear) %>%
      dplyr::rename(county = Admin2, fips = FIPS, state = Province_State) %>%
      dplyr::group_by(county, fips, state) %>%
      dplyr::arrange(date) %>%
      ## coerce from cumulative to incident cases
      dplyr::mutate(icases = count - lag(count, default = 0L))

    ## by usa
    dat <-
      dat %>%
      dplyr::group_by(epiyear, epiweek) %>%
      dplyr::summarise(icases = sum(icases, na.rm=TRUE), .groups="drop") %>%
      head(-1)

  } else if (source == "nyt") {
    dat <-
      readr::read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv", col_types="Dii") %>%
      dplyr::mutate(epiyear=MMWRweek::MMWRweek(date)$MMWRyear, .after=date) %>%
      dplyr::mutate(epiweek=MMWRweek::MMWRweek(date)$MMWRweek, .after=epiyear) %>%
      # dplyr::mutate(week=week(date), .after=epiweek) %>%
      dplyr::mutate(icases  = cases  - lag(cases, default = 0L))

    ## by usa
    dat <-
      dat %>%
      dplyr::group_by(epiyear, epiweek) %>%
      dplyr::summarise(icases = sum(icases, na.rm=TRUE), .groups="drop") %>%
      head(-1)

  }
}
