# clean text and datawrangling
# ---------------------------------------------------------------------------- #

# packages ---------------------------------------------------------------------
library(tidyverse)
library(quanteda)
library(rio)
library(tm)

# data import ------------------------------------------------------------------
tweets <- import("output_data/tweets_tidy.rds")

# import & datawrangling metadata
candidates2021_raw <- import("raw_data/twitter2021.rds")
institutions2021_raw <- import("raw_data/epin2021.rds")


# datawrangling ----------------------------------------------------------------

# datawrangling tweets
tweets <- tweets %>%
  select(tweet_id, user_username, text, lang, created_at,
         user_verified, user_location, user_created_at, user_url,
         retweet_count, like_count, quote_count,
         user_tweet_count, user_list_count, user_followers_count,
         user_following_count) %>%
  rename(language = lang,
         tweet_date = created_at,
         twitter_handle = user_username) %>%
  mutate(twitter_handle = tolower(twitter_handle)) %>%
  filter(language == "de")

# datawrangling candidates
candidates2021 <- candidates2021_raw %>%
  mutate(name = paste(firstname, lastname),
         gender = recode(gender,
                         "w" = "female",
                         "m" = "male",
                         "d" = "diverse"),
         party = recode(party,
                        "DIE LINKE" = "LINKE",
                        "GRÜNE" = "GRUENE")) %>%
  rename(twitter_handle = screen_name1,
         listed_candidate = isListed,
         direct_candidate = isDC,
         region = state) %>%
  filter(!is.na(twitter_handle)) %>% 
  select(name, twitter_handle, gender, party, district_name, district_number,
         region, incumbent, listed_candidate, direct_candidate)

# datawrangling institutions
institutions2021 <- institutions2021_raw %>%
  ungroup() %>% 
  rename(name = official_name) %>%
  mutate(district_name = NA,
         district_number = NA,
         listed_candidate = NA,
         direct_candidate = NA,
         region = case_when(
           region == "Brandenburg" ~ "BB",
           region == "Schleswig-Holstein" ~ "SH",
           region == "Saarland" ~ "SL",
           region == "Hamburg" ~ "HH",
           region == "Baden-Württemberg" ~ "BW",
           region == "Bavaria" ~ "BY",
           region == "Berlin" ~ "BE",
           region == "Bremen" ~ "HB",
           region == "Hesse" ~ "HE",
           region == "Mecklenburg-West Pomerania" ~ "MV",
           region == "Lower Saxony" ~ "NI",
           region == "North Rhine-Westphalia" ~ "NW",
           region == "Rhineland-Palatinate" ~ "RP",
           region == "Saxony" ~ "SN",
           region == "Saxony-Anhalt" ~ "ST",
           region == "Thuringia" ~ "TH",
           TRUE ~ region),
         party = recode(party,
                        "DIE LINKE" = "LINKE",
                        "Bündnis 90/Die Grünen" = "GRUENE"),
         incumbent = case_when(
           party == "CDU" ~ 1,
           party == "CSU" ~ 1,
           party == "SPD" ~ 1,
           TRUE ~ 0),
         twitter_handle = tolower(twitter_handle)) %>%
  filter(office == "Parliamentary Party Group") %>%
  select(name, twitter_handle, gender, party, district_name, district_number,
         region, incumbent, listed_candidate, direct_candidate) %>% 
  ungroup()

# rbind into one dataframe
pol_twitter_accounts <- rbind(candidates2021, institutions2021) %>%
  distinct(twitter_handle, .keep_all = TRUE)


# join information which institution and office were candidates part of?
institution_join <- institutions2021_raw %>%
  ungroup() %>% 
  filter((is.na(until) | until >= "2017-10-24") &
           !(office == "Speaker" |
               office == "Parliamentary Party Group" |
               office == "Ministry")) %>%
  mutate(level = case_when(
    institution %in% c("Federal Parliament", "Federal Government") ~ "federal",
    institution %in% c("State Parliament", "State Government") ~ "state",
    institution == "European Parliament" ~ "european", TRUE ~ NA_character_)) %>%
  mutate(binary_federal_parliamentarian = if_else(level == "federal" & office == "Parliamentarian", 1, 0),
         binary_state_parliamentarian = if_else(level == "state" & office == "Parliamentarian", 1, 0),
         binary_european_parliamentarian = if_else(level == "european" & office == "Parliamentarian", 1, 0),
         binary_federal_state_secretary = if_else(level == "federal" & office == "State Secretary", 1, 0),
         binary_state_state_secretary = if_else(level == "state" & office == "State Secretary", 1, 0),
         binary_federal_minister = if_else(level == "federal" & office == "Minister", 1, 0),
         binary_state_minister = if_else(level == "state" & office == "Minister", 1, 0)) %>%
  group_by(twitter_handle) %>%
  summarise(across(starts_with("binary_"), max, na.rm = TRUE)) %>% 
  ungroup()

# left join
pol_twitter_accounts <- pol_twitter_accounts %>%
  left_join(institution_join, by = "twitter_handle")

# when binary variables NA -> 0
pol_twitter_accounts <- pol_twitter_accounts %>%
  mutate(across(starts_with("binary_"), ~if_else(is.na(.), 0, .), .names = "{col}"))

# remove process files
rm(candidates2021, candidates2021_raw, institutions2021, institutions2021_raw, institution_join)

# leftjoin metadata to tweets 

tweets_master <- left_join(tweets, pol_twitter_accounts, by = "twitter_handle")


# corpus and textual formatting ------------------------------------------------

# corpus object
tweets_corpus <- corpus(tweets_master$text)

# preprocess and tokenize the text
cleaned_tokens <- tokens(tweets_corpus,
                         remove_punct = TRUE,
                         remove_numbers = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("de")) %>%
  tokens_remove(pattern = c("[[:punct:]]", "http", "https",
                            "rt", "t.co", "www", "@\\w+", "#\\w+"),
                valuetype = "regex")

# convert cleaned tokens back to text
cleaned_text <- tokens_compound(cleaned_tokens, phrase('*'))
cleaned_text <- tokens_ngrams(cleaned_text, n = 1, concatenator = " ")
cleaned_text <- unlist(lapply(cleaned_text, paste, collapse = " "))

# Replace the original text column with the cleaned text
tweets_master <- tweets_master %>%
  mutate(text_clean = cleaned_text)

# Columns in correct order
tweets_master <- tweets_master %>%  
  select(
    tweet_id,
    twitter_handle,
    text,
    text_clean,
    language,
    tweet_date,
    retweet_count,
    like_count,
    quote_count,
    name,
    gender,
    party,
    district_name,
    district_number,
    region,
    incumbent,
    listed_candidate,
    direct_candidate,
    binary_federal_parliamentarian,
    binary_state_parliamentarian,
    binary_european_parliamentarian,
    binary_federal_state_secretary,
    binary_state_state_secretary,
    binary_federal_minister,
    binary_state_minister,
    user_verified,
    user_location,
    user_created_at,
    user_url,
    user_tweet_count,
    user_list_count,
    user_followers_count,
    user_following_count)


# remove process files
rm(cleaned_text, tweets_corpus, cleaned_tokens, tweets)

# data export ------------------------------------------------------------------

export(tweets_master, "output_data/tweets_master.rds")
export(pol_twitter_accounts, "output_data/pol_twitter_accounts.rds")
