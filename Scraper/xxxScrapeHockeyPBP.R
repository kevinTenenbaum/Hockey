library(RCurl); library(xml2); library(rvest); library(jsonlite); library(foreach)
library(lubridate)
library(devtools)
library(tidyverse) ## -- specifically: stringr, readr, tidyr, and dplyr


## Source scraper functions from GitHub
devtools::source_url("https://raw.githubusercontent.com/evolvingwild/evolving-hockey/master/EH_scrape_functions.R")

## Scrape games
pbp_scrape <- sc.scrape_pbp(games = c("2018020001", "2018020002"))

names(pbp_scrape)


pbp <- pbp_scrape


names(pbp)
pbp$scratches_df %>% head()

