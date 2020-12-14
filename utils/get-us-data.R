suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lubridate))

# get usa data straight from nyt usa summarized data
usa <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv", col_types="Dii")

# remove today's data if it's present
usa <-
  usa %>%
  filter(date!=today())

# add epiyear and epiweek
usa <-
  usa %>%
  mutate(epiyear=MMWRweek::MMWRweek(date)$MMWRyear, .after=date) %>%
  mutate(epiweek=MMWRweek::MMWRweek(date)$MMWRweek, .after=epiyear) %>%
  mutate(week=week(date), .after=epiweek)

# calculate incident cases and deaths
usa <-
  usa %>%
  mutate(icases  = cases  - lag(cases, default=0L),
         ideaths = deaths - lag(deaths, default=0L))

# get weekly incident cases and deaths, excluding current week
usaw <- usa %>%
  group_by(epiyear, epiweek) %>%
  summarise(across(c(icases, ideaths), sum, na.rm=TRUE), .groups="drop") %>%
  head(-1)
