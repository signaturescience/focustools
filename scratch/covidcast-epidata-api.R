# https://cmu-delphi.github.io/covidcast/covidcastR/

# devtools::install_github("cmu-delphi/covidcast", ref = "main", subdir = "R-packages/covidcast")

library(covidcast)

data <- covidcast_signal(data_source = "indicator-combination",
                         signal = "nmf_day_doc_fbc_fbs_ght",
                         start_day = "2020-11-15", end_day = "2020-11-30")
class(data)
head(data)
plot(data)
