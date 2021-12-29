# Pre-processing for generating NLP related variables
library(tidyverse)
library(tidytext)

# Read in each file and combine into one
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

# read in senticnet5 corpus
senticnet <- read.table("senticnet5.txt", header = TRUE)

senticnet %>%
  mutate(CONCEPT = str_replace_all(CONCEPT, "_", " ")) %>%
  rename(word = CONCEPT) -> senticnet

# read in SlangSD corpus
slangSD <- read.delim("SlangSD.txt") %>%
  rename(word = 'a.',
         score = 'X.1')# improve the reading?

# get tweet text
tweet_text <- tweets %>%
  select(status_id, text)

# start framework data frame to add other variables to
tweet_text %>%
  mutate(tweet_num = row_number()) %>%
  select(tweet_num, text) -> tweet_text

# create new variables:
# we want a variable for total number of hashtags used in a tweet, variables for which of our
# primary hashtags was used, as well as a variable of the alphanumeric strings in the urls found
# in the tweets to create custom stopword list
#
# had to use strip_punct = FALSE for this section to be able to identify hashtags

tweet_text %>%
  unnest_tokens(word, text, strip_punct = FALSE) %>%
  mutate(hash_mark = str_detect(word, "#")) %>%
  mutate(complete_hash = ifelse(hash_mark == TRUE, str_c(word, lead(word)), NA)) %>%
  mutate(hash_index = ifelse(!is.na(complete_hash), 1, 0)) %>%
  mutate(complete_hash = ifelse(!is.na(complete_hash),str_squish(complete_hash), NA)) %>%
  group_by(tweet_num) %>%
  mutate(num_ht = sum(hash_index)) %>%
  ungroup() %>%
  mutate(primary_ht = ifelse(complete_hash %in% c("#blm", "#blacklivesmatter", "#defundthepolice", "#policebrutality"), 1, 0)) %>%
  mutate(is_blm = ifelse(complete_hash == "#blm", 1, 0),
         is_blacklivesmatter = ifelse(complete_hash == "#blacklivesmatter", 1, 0),
         is_defundthepolice = ifelse(complete_hash == "#defundthepolice", 1, 0),
         is_policebrutality = ifelse(complete_hash == "#policebrutality", 1, 0)) %>%
  mutate(slash_mark = str_detect(word, "/")) %>%
  mutate(slash_link = ifelse(slash_mark == TRUE, str_c(word, lead(word)), NA)) -> tweet_text_create_vars

# create a df to be joined later that summarizes previously generated variables back into tidy
# format, one row per tweet

tweet_text_create_vars %>%
  select(tweet_num, complete_hash, num_ht, primary_ht, is_blm, is_blacklivesmatter, is_defundthepolice, is_policebrutality) %>%
  filter(complete.cases(complete_hash)) -> tweet_text_create_vars_pare_down

tweet_text_create_vars_pare_down %>%
  group_by(tweet_num) %>%
  summarise(num_ht = unique(num_ht),
            is_blm = sum(is_blm),
            is_blacklivesmatter = sum(is_blacklivesmatter),
            is_defundthepolice = sum(is_defundthepolice),
            is_policebrutality = sum(is_policebrutality)) -> tweet_text_create_vars_pare_down

# create sentiment score variables using our two dictionaries
# this involves unnesting by token, taking out standard stopwords (tidytext) and link stopwords
# (custom created from url codes from tweets), joining separately with each dictionary, and
# computing the average score per tweet

tweet_text2 <- tweets %>%
  select(text)

# unnest tokens and filter out primary hashtags and hyperlink tokens
tweet_text2 %>%
  mutate(tweet_num = row_number()) %>%
  unnest_tokens(word, text) %>%
  filter(!(word %in% c("https", "t.co", "amp", "blm", "blacklivesmatter", "policebrutality", "defundthepolice"))) -> tweet_text2

# get rid of stopwords from tidytext stopwords list
data("stop_words")
tweet_text2 %>%
  anti_join(stop_words) -> tweet_text2

# create link stopword list from alphanumeric part of hyperlink
tweet_text_create_vars %>%
  filter(!is.na(slash_link)) %>%
  select(slash_link) %>%
  filter(!slash_link == "//") %>%
  filter(!slash_link == "/t.co") %>%
  mutate(slash_link = str_remove(slash_link, "/")) -> link_stopwords

# format custom stopword vector be be able to anti_join with unnested tokens
link_stopwords <- as.vector(link_stopwords)
link_stopwords <- unique(link_stopwords)
link_stopwords <- tibble(link_stopwords)
link_stopwords %>%
  rename(word = slash_link) -> link_stopwords

# remove link stopwords
tweet_text2 %>%
  anti_join(link_stopwords) -> tweet_text2

# generate sentiment scores using senticnet dictionary
tweet_text2 %>%
  inner_join(senticnet) -> tweet_sentiment

tweet_sentiment %>%
  group_by(tweet_num) %>%
  summarize(sent_score_senticnet = mean(INTENSITY)) -> tweet_sentiment

# repeat sentiment score generation using slangSD dictionary
tweet_text2 %>%
  inner_join(slangSD) -> tweet_sentiment_slang

tweet_sentiment_slang %>%
  group_by(tweet_num) %>%
  summarise(sent_score_slang = mean(score)) -> tweet_sentiment_slang

# append sentiment score variables to tweet text df
tweet_text %>%
  left_join(tweet_sentiment) %>%
  left_join(tweet_sentiment_slang) -> tweet_text

# append the other generated variables (# of hashtags, etc) with tweet text df
tweet_text %>%
  left_join(tweet_text_create_vars_pare_down) -> tweet_text

# join all new generated columns to original df
tweets %>%
  mutate(tweet_num = row_number()) -> tweets

tweet_text %>%
  select(-text) -> tweet_text

tweets %>%
  left_join(tweet_text) -> tweets

# create one factor-class variable to identify which one or more primary hashtag was used in the tweet
tweets %>%
  mutate(primary_hts = case_when(is_blm == T & is_blacklivesmatter == F & is_defundthepolice == F & is_policebrutality == F ~ "#blm",
                                 is_blm == T & is_blacklivesmatter == T & is_defundthepolice == F & is_policebrutality == F ~ "#blm, #blacklivesmatter",
                                 is_blm == T & is_blacklivesmatter == T & is_defundthepolice == T & is_policebrutality == F ~ "#blm, #blacklivesmatter, #defundthepolice",
                                 is_blm == T & is_blacklivesmatter == T & is_defundthepolice == F & is_policebrutality == T ~ "#blm, #blacklivesmatter, #policebrutality",
                                 is_blm == T & is_blacklivesmatter == T & is_defundthepolice == T & is_policebrutality == T ~ "#blm, #blacklivesmatter, #defundthepolice, #policebrutality",
                                 is_blm == T & is_blacklivesmatter == F & is_defundthepolice == T & is_policebrutality == F ~ "#blm, #defundthepolice",
                                 is_blm == T & is_blacklivesmatter == F & is_defundthepolice == T & is_policebrutality == T ~ "#blm, #defundthepolice, #policebrutality",
                                 is_blm == T & is_blacklivesmatter == F & is_defundthepolice == F & is_policebrutality == T ~ "#blm, #policebrutality",
                                 is_blm == F & is_blacklivesmatter == T & is_defundthepolice == F & is_policebrutality == F ~ "#blacklivesmatter",
                                 is_blm == F & is_blacklivesmatter == T & is_defundthepolice == T & is_policebrutality == F ~ "#blacklivesmatter, #defundthepolice",
                                 is_blm == F & is_blacklivesmatter == T & is_defundthepolice == T & is_policebrutality == T ~ "#blacklivesmatter, #defundthepolice, #policebrutality",
                                 is_blm == F & is_blacklivesmatter == T & is_defundthepolice == F & is_policebrutality == T ~ "#blacklivesmatter, #policebrutality",
                                 is_blm == F & is_blacklivesmatter == F & is_defundthepolice == T & is_policebrutality == F ~ "#defundthepolice",
                                 is_blm == F & is_blacklivesmatter == F & is_defundthepolice == T & is_policebrutality == T ~ "#defundthepolice, #policebrutality",
                                 is_blm == F & is_blacklivesmatter == F & is_defundthepolice == F & is_policebrutality == T ~ "#policebrutality")) %>%
  mutate(primary_hts = as.factor(primary_hts)) -> tweets

# generate state and country columns for location data

countrylist <- countrycode::countryname_dict

tweets %>%
  mutate(state_loc = case_when(country == "United States" & place_type == "admin" ~ place_name,
                               country == "United States" & place_type == "city" ~ str_sub(place_full_name, start = -2, end = -1),
                               str_detect(location, state.name) ~ str_extract(location, state.name))) %>%
  mutate(country_loc = case_when(!is.na(country) ~ country,
                                 is.na(country) & location %in% countrylist$country.name.en ~ location,
                                 is.na(country) & location %in% countrylist$country.name.alt ~ location)) -> tweets

tweets$state_loc[tweets$state_loc %in% state.abb] <- state.name[tweets$state_loc %in% state.abb]
tweets$country_loc[!is.na(tweets$state_loc)] <- "United States"

countrylist2 <- countrylist %>%
  rename(country_loc = country.name.alt)

tweets %>%
  left_join(countrylist2) -> tweets

tweets %>%
  mutate(country_loc = ifelse(country_loc != country.name.en, country.name.en, country_loc)) %>%
  select(-country.name.en) -> tweets

# turn is_blm et al into binary T/F variables
tweets %>%
  mutate(is_blm = ifelse(is_blm == 0, F, T),
         is_blacklivesmatter = ifelse(is_blacklivesmatter == 0, F, T),
         is_defundthepolice = ifelse(is_defundthepolice == 0, F, T),
         is_policebrutality = ifelse(is_policebrutality == 0, F, T)) -> tweets

# save dataframe into .rds file
saveRDS(tweets, "../../clean_blm_tweets.rds")

# save new table back into split .rds files
file_name <- "../../clean_blm_tweets.rds"
file_info <- file.info(file_name)
df <- read_rds(file_name)

file_rows <- nrow(df)

# How many smaller files do I need to create such that they are all smaller than 5 MB?
min_subfiles <- ceiling(file_info$size/(5*1024^2))

# Max # rows per subfile (round down)
max_rps <- floor(file_rows/min_subfiles)

# Save each of the subfiles to data-clean directory
for (i in 1:min_subfiles) {
  df_sub <- df[((i - 1)*max_rps + 1):((i - 1)*max_rps + max_rps),]
  fname_sub <- str_c("../data-clean/",
                     str_extract(file_name, "\\w+"),
                     as.character(i),
                     ".rds",
                     collapse = "")
  saveRDS(df_sub,fname_sub)
}

#Now to test the files read them back in
df2 <- df[1,] # Create a dummy row for the header
for (i in seq(1:min_subfiles)){
  fname_sub <- str_c("../data-clean/",
                     str_extract(file_name, "\\w+"),
                     as.character(i),
                     ".rds",
                     collapse = "")
  df2 <- bind_rows(df2,read_rds(fname_sub))
  file_info <- bind_rows(file_info, file.info(fname_sub))
  
} # end for loop 

#compare the original with the read in after removing the dummy row
df2 <- df2[-1,]
all(df2 ==df, na.rm = TRUE)

# The data frames match, but file info shows that each of the 3 compressed files is over 8.5 MB.
# Try instead arbitrarily doubling the min # of files.
# File info shows that file size has only decreased from about 8.6 MB to 7 MB after
# doubling the number of files. Clearly diminishing returns.
# Try squaring number of minimum files (originally 3; set to 9).
# No perceptible change from doubling to squaring number of subfiles.
