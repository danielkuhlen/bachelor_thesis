# clean text and datawrangling
# ---------------------------------------------------------------------------- #

# packages ---------------------------------------------------------------------
library(tidyverse)
library(quanteda)
library(rio)
library(tm)

# data import ------------------------------------------------------------------
tweets_clean <- import("output_data/tweets_clean.rds")
rauh_dictionary <- import("../dictionary/1_Dictionaries/Rauh_SentDictionaryGerman.Rdata")
rauh_dictionary_negation <- import("../dictionary/1_Dictionaries/Rauh_SentDictionaryGerman_Negation.Rdata")


# analysis ---------------------------------------------------------------------

# create corpus from dataframe
tweets_corpus <- corpus(tweets_clean, text_field = "text_clean")
summary(tweets_corpus, 10)

# corpus incumbant
incumbent_corpus <- tweets_corpus %>%
  corpus_subset(incumbent == 1)

# corpus incumbant
opposition_corpus <- tweets_corpus %>%
  corpus_subset(incumbent == 0)

