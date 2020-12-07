library(tidyverse)
library(yaml)
library(httr)

## construct request to GH API endpoint
req <- GET("https://api.github.com/repos/reichlab/covid19-forecast-hub/git/trees/master?recursive=1")

## get paths to all files
repo_files <- map_chr(content(req)$tree, "path")
## subset to only metadata files
meta_files <- repo_files[grepl("^data-processed/.*/metadata*", repo_files)]
## add prefix for API endpoint
meta_files <- paste0("https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/", meta_files)

## read_yaml from GH directly and map to a tibble
c19f_methods <- map_df(meta_files, read_yaml)

## write csv to disk
outfile <- here::here("misc", "c19fh-scrape-metadata", paste0(format(Sys.Date(), "%F"), "-c19fh-metadata.csv"))
message(paste0("Saving to file: "), outfile)
write_csv(c19f_methods, outfile)
