library(tidyverse)
library(EpiEstim)

## read in county level cumulative data from NYT
counties <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

## helper for compaRE
## wraps the EpiEstim::estimate_R() function so it can be used in group_modify()
## uses the assumption that serial interval (SI) is parametric
## also seeds default values for mean/sd for SI with published values per 10.1101/2020.04.23.20075796
## that paper was based on chinese cases early in the pandemic ... better estimates out there by now?
estimate_summary <- function(x, mean_si = 5.29, std_si = 5.32, overall = FALSE) {

  if(overall) {
    configs <- list(mean_si = mean_si, std_si = std_si, t_start = 2, t_end = length(x)-7)
  } else {
    configs <- list(mean_si = mean_si, std_si = std_si)
  }

  estimate_R(x,
             method = "parametric_si",
             config = configs) %>%
    ## estimate_R returns a named list
    ## pull out R data.frame
    pluck("R") %>%
    ## add time index column called 'day'
    mutate(day = 1:n()) %>%
    ## select and rename columns of interest
    select(mean = `Mean(R)`, lower = `Quantile.0.025(R)`, upper = `Quantile.0.975(R)`, day)
}


## compaRE function wraps the estimate_summary function such that it can be mapped over multiple counties
## includes option to get overall (average) Re
## also includes an option to specify "start_date" to restrict time frame
## passes along mean_si and std_si params
compaRE <- function(.data, counties, start_date, mean_si = 5.29, std_si = 5.32, overall = FALSE) {

  ## incidence data
  dat <-
    ## take county data
    .data %>%
    ## filter for county fips
    dplyr::filter(fips %in% counties)

  ## run the estimate_summary() wrapper in each group
  res <-
    dat %>%
    ## group by fips, county, state to retain county and state names
    dplyr::group_by(fips,county,state) %>%
    dplyr::group_modify(~ estimate_summary(x = .x$cases, mean_si = mean_si, std_si = std_si, overall = overall))

  ## get the average Re over the time frame? use overall = TRUE
  if(overall) {
    res <-
      res %>%
      dplyr::select(-day)

    return(res)
    ## otherwise get
  } else {
    ## join back to dat (with some manipulation) to get date column
    res <-
      dat %>%
      dplyr::arrange(date) %>%
      dplyr::group_by(fips) %>%
      ## skip the first 7 rows in each grouping because the Re uses first week to initialize
      ## in other words res will have 7*n groups fewer rows than dat
      dplyr::filter(dplyr::row_number() > 7) %>%
      ## add indicator for day
      ## this will be used to join to the
      dplyr::mutate(day = 1:n()) %>%
      ## filter for dates >= start date in yyyy-mm-dd format
      dplyr::filter(date >= as.Date(start_date)) %>%
      dplyr::left_join(res)

    return(res)
  }
}

## pick several fips codes
## 51540 = charlottesville
## 48453 = travis (austin)
## 48141 = el paso
## 48201 = harris (houston)
## 06035 = lassen (in california; one of the counties NYT designated as "hot spot" as of 12/4)

## first take a look at the averageoverall = TRUE
compaRE(counties,
        counties = c("51540", "48141", "48453", "48201", "36081", "06035"),
        start_date = "2020-05-01",
        overall = TRUE)

## plot the results
compaRE(counties,
        counties = c("51540", "48141", "48453", "48201", "36081", "06035"),
        start_date = "2020-09-01") %>%
  ggplot() +
  geom_line(aes(date, mean, group = county, col = county)) +
  geom_ribbon(aes(date, ymin = lower, ymax = upper, group = county, fill = county), alpha = 0.5, lty = "dotted" ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0.8,2), breaks = seq(0.8, 2, by = .1)) +
  scale_x_date(breaks = "months", date_labels = "%Y-%m") +
  labs(y = "Effective reproduction number (Re)",
       x = "Date")

ggsave("re_example.png")
