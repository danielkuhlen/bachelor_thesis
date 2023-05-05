# import tweets 3 months prior to the election 2021
# ---------------------------------------------------------------------------- #

# packages ---------------------------------------------------------------------
library(academictwitteR)
library(tidyverse)
library(rio)

# data import ------------------------------------------------------------------

# import tweets and bring it into tidy format
tweets.tidy <- bind_tweets(data_path = "raw_data/tweets_scraping_2",
                           output_format = "tidy")


# export data ------------------------------------------------------------------
export(tweets.tidy, "output_data/tweets_tidy.rds")