# descriptive plots
# ---------------------------------------------------------------------------- #

# packages ---------------------------------------------------------------------
library(tidyverse)
library(rio)
library(grid)

# data import ------------------------------------------------------------------

# import tweets
tweets <- import("/Users/danielkuhlen/Desktop/uni/ba/02_data/02_output_data/tweets_master.rds")

# import twitter accounts
candidates2021_raw <- import("/Users/danielkuhlen/Desktop/uni/ba/02_data/01_raw_data/twitter2021.rds")

# datawrangling ----------------------------------------------------------------

candidates2021 <- candidates2021_raw %>%
  mutate(name = paste(firstname, lastname),
         gender = recode(gender,
                         "w" = "female",
                         "m" = "male",
                         "d" = "diverse"),
         party = recode(party,
                        "DIE LINKE" = "LINKE",
                        "GRÜNE" = "GRUENE"),
         twitter_active = ifelse(is.na(screen_name1), 0, 1)) %>%
  rename(twitter_handle = screen_name1,
         listed_candidate = isListed,
         direct_candidate = isDC,
         region = state) %>%
  select(name, twitter_handle, gender, party, district_name, district_number,
         region, incumbent, listed_candidate, direct_candidate, twitter_active)

# plotting ---------------------------------------------------------------------

# percentage with twitter account by party -------------------------------------
party_totals <- candidates2021 %>%
  group_by(party) %>%
  summarize(total = n())

twiiteraccounts_party <- candidates2021 %>%
  group_by(party, twitter_active) %>%
  summarize(count = n()) %>%
  left_join(party_totals, by = "party") %>%
  mutate(percentage = count / total * 100,
         party = factor(party,
                        levels = c("LINKE", "GRUENE", "SPD", "FDP",
                                   "CDU", "CSU", "AfD"))) %>%
  ungroup() %>%
  ggplot(aes(x = party,
             y = percentage,
             fill = interaction(party, factor(twitter_active)))) +
  geom_bar(stat = "identity",
           position = position_dodge(width = .5)) +
  geom_text(data = . %>% filter(twitter_active == 1),
            aes(x = party, y = percentage, label = sprintf("%.1f", percentage)),
            position = position_dodge(width = .5),
            vjust = -0.5,
            size = 3,
            family = "Times New Roman",
            hjust = -.02) +
  labs(x = "",
       y = "",
       title = "",
       legend = "",
       caption = "Note: In this plot, party-colored bars correspond to the percentage of candidates with a Twitter account, \nand grey to the percentage without an account.") +
  guides(fill = guide_legend(title = NULL)) +
  scale_fill_manual(values = c("LINKE.0" = "#C0C0C0AA", "LINKE.1" = "#BE3075",
                               "GRUENE.0" = "#C0C0C0AA", "GRUENE.1" = "#1AA037",
                               "SPD.0" = "#C0C0C0AA", "SPD.1" = "#E3000F",
                               "FDP.0" = "#C0C0C0AA", "FDP.1" = "#FFEF00",
                               "CDU.0" = "#C0C0C0AA", "CDU.1" = "#000000",
                               "CSU.0" = "#C0C0C0AA", "CSU.1" = "#0570C9",
                               "AfD.0" = "#C0C0C0AA", "AfD.1" = "#0489DB"),
                    labels = rep(c("No Twitter", "Has Twitter"), 7)) +
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, 10),
                     labels = scales::percent(seq(0, 1, 0.1))) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),
        legend.position = "none",
        plot.caption = element_text(hjust = 0,
                                    size = 8,
                                    face="italic",
                                    margin = margin(t = 8)))

ggsave("plots/twitter_accounts_party.png",
       twiiteraccounts_party, width = 16, height = 16, units = "cm", dpi = 500)


# number of tweets by party ----------------------------------------------------

number_tweets_party <- tweets %>%
  group_by(party) %>%
  summarise(tweet_count = n()) %>%
  mutate(party = factor(party,
                        levels = c("LINKE", "GRUENE", "SPD", "FDP",
                                   "CDU", "CSU", "AfD"))) %>%
  ungroup() %>%
  mutate(party = reorder(party, tweet_count, FUN = max)) %>% # Reorder the factor levels
  ggplot(aes(x = party,
             y = tweet_count,
             fill = party)) +
  geom_bar(stat = "identity", width = .6) +
  geom_text(aes(label = tweet_count),
            hjust = -0.5,
            size = 3,
            family = "Times New Roman") +
  labs(x = "",
       y = "",
       title = "",
       subtitle = "",
       legend = "") +
  guides(fill = guide_legend(title = NULL)) +
  scale_fill_manual(values = c("LINKE" = "#BE3075",
                               "GRUENE" = "#1AA037",
                               "SPD" = "#E3000F",
                               "FDP" = "#FFEF00",
                               "CDU" = "#000000",
                               "CSU" = "#0570C9",
                               "AfD" = "#0489DB")) +
  scale_y_continuous(limits = c(0, 300000),
                     breaks = seq(0, 300000, 50000)) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),
        legend.position = "none",
        plot.caption = element_text(hjust = 0,
                                    size = 8,
                                    face="italic",
                                    margin = margin(t = 8))) +
  coord_flip()

ggsave("plots/number_tweets_party.png",
       number_tweets_party, width = 16, height = 16, units = "cm", dpi = 500)


# average number of tweets by candidate and party ------------------------------

average_tweets_by_party <- tweets %>%
  group_by(party, name) %>%
  summarise(total_tweets = n()) %>%
  group_by(party) %>%
  summarise(average_tweets = mean(total_tweets))

# average length of tweets by party --------------------------------------------

average_tweet_length_by_party <- tweets %>%
  mutate(tweet_length = nchar(text)) %>%
  group_by(party) %>%
  summarise(average_length = mean(tweet_length))

# most active users ------------------------------------------------------------

most_active_users <- tweets %>%
  group_by(party, name) %>% 
  summarise(total_tweets = n()) %>%
  arrange(desc(total_tweets)) %>%
  head(20)
  
