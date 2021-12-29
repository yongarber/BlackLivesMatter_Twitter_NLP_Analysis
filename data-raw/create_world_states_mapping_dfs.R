# Create world and state level tables for mapping purposes
library(tidyverse)
library(usmap)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(ggspatial)
library(maps)

# Load in tweet df
for (i in 1:3) {
  filename <- str_c("../data-clean/clean_blm_tweets",
                    as.character(i),
                    ".rds",
                    collapse = "")
  if (i==1) {
    tweets <- read_rds(filename)
  } else {
    tweets <- bind_rows(tweets, read_rds(filename))
  }
  
}

###############
# World
###############

# Pare down tweets df to desired variables

tweets %>%
  group_by(country_loc) %>%
  mutate(ave_num_ht = mean(num_ht[!is.na(country_loc)])) %>%
  ungroup() %>%
  select(country_loc, sent_score_senticnet, sent_score_slang, ave_num_ht) %>%
  filter(!is.na(country_loc)) %>%
  group_by(country_loc) %>%
  summarise(sent_score_senticnet = mean(sent_score_senticnet, na.rm = TRUE),
            sent_score_slang = mean(sent_score_slang, na.rm = TRUE),
            ave_num_ht) %>%
  group_by(country_loc) %>%
  add_count() %>%
  distinct() %>%
  rename(name = country_loc,
         num_tweets = n) -> country_tweets

# Replace NaN values with 0

country_tweets$sent_score_senticnet[country_tweets$sent_score_senticnet == "NaN"] <- 0
country_tweets$sent_score_slang[country_tweets$sent_score_slang == "NaN"] <- 0

# Create world df to join country_tweet to

world <- ne_countries(scale = "medium", returnclass = "sf")

# Coerce country_tweets name values to match world df

country_tweets$name[country_tweets$name == "Bavaria"] <- "Germany"
country_tweets$name[country_tweets$name == "Christmas Island"] <- "Australia"
country_tweets$name[country_tweets$name == "Dominican Republic"] <- "Dominican Rep."
country_tweets$name[country_tweets$name == "Eswatini"] <- "Swaziland"
country_tweets$name[country_tweets$name == "Hong Kong SAR China"] <- "Hong Kong"
country_tweets$name[country_tweets$name == "Martinique"] <- "France"
country_tweets$name[country_tweets$name == "North Macedonia"] <- "Macedonia"
country_tweets$name[country_tweets$name == "South Korea"] <- "Korea"
country_tweets$name[country_tweets$name == "St. Kitts & Nevis"] <- "St. Kitts and Nevis"
country_tweets$name[country_tweets$name == "Trinidad & Tobago"] <- "Trinidad and Tobago"
country_tweets$name[country_tweets$name == "U.S. Virgin Islands"] <- "U.S. Virgin Is."
country_tweets$name[country_tweets$name == "Vatican City"] <- "Vatican"

# Check for mis-matches
country_tweets %>%
  filter(!name %in% world$name)

# Merge world and country_tweets
world %>%
  left_join(country_tweets) -> world

# Test map
ggplot(world) +
  geom_sf(aes(fill = sent_score_senticnet)) +
  scale_fill_gradient2(low = "red",
                       mid = "white",
                       high = "blue",
                       na.value = "darkgray")

###############
# US States
###############

# Pare down for state level comparison

tweets %>%
  group_by(state_loc) %>%
  mutate(ave_num_ht = mean(num_ht[!is.na(state_loc)])) %>%
  ungroup() %>%
  select(state_loc, sent_score_senticnet, sent_score_slang, ave_num_ht) %>%
  filter(!is.na(state_loc)) %>%
  group_by(state_loc) %>%
  summarise(sent_score_senticnet = mean(sent_score_senticnet, na.rm = TRUE),
            sent_score_slang = mean(sent_score_slang, na.rm = TRUE),
            ave_num_ht) %>%
  group_by(state_loc) %>%
  add_count() %>%
  distinct() %>%
  rename(name = state_loc,
         num_tweets = n) -> state_tweets

# Replace NaN value with 0

state_tweets$sent_score_slang[state_tweets$sent_score_slang == "NaN"] <- 0

# Create state df to joing state_tweets to

states <- statepop %>%
  select(full) %>%
  rename(name = full)

# Coerce state name value to match 
state_tweets$name[state_tweets$name == "DC"] <- "District of Columbia"

# Check for mis-matches
state_tweets %>%
  filter(! name %in% states$name)

# Merge states and state_tweets
states %>%
  left_join(state_tweets) %>%
  rename(state = name) -> states

# Test map

plot_usmap(regions = "states", data = states, values = "sent_score_slang") +
  scale_fill_gradient2(name = "Sentiment Score (SlangSD)",
                       low = "red",
                       mid = "white",
                       high = "blue",
                       midpoint = 0,
                       na.value = "black") +
  theme(legend.position = "right")

# Write world and states to .rds files for reading into shiny

saveRDS(world, "../data-clean/world_tweets.rds")
saveRDS(states, "../data-clean/states_tweets.rds")
