j <- get_cases(source="jhu", cache=F, granularity = "national")
n <- get_cases(source="nyt", cache=F, granularity = "national")

d <-
  inner_join(n, j, by=c("epiyear", "epiweek"), suffix=c(".nyt", ".jhu")) %>%
  mutate(date=MMWRweek::MMWRweek2Date(epiyear, epiweek))

d %>% ggplot(aes(icases.nyt, icases.jhu)) + geom_point()
d %>% ggplot(aes(ccases.nyt, ccases.jhu)) + geom_point()

j <- get_deaths(source="jhu", cache=F, granularity = "national")
n <- get_deaths(source="nyt", cache=F, granularity = "national")

d <-
  inner_join(n, j, by=c("epiyear", "epiweek"), suffix=c(".nyt", ".jhu")) %>%
  mutate(date=MMWRweek::MMWRweek2Date(epiyear, epiweek))

d %>% ggplot(aes(ideaths.nyt, ideaths.jhu)) + geom_point()
d %>% ggplot(aes(cdeaths.nyt, cdeaths.jhu)) + geom_point()




