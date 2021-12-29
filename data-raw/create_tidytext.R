# Create .rds files in tidytext format

library(tidyverse)
library(tidytext)
library(textstem)

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


# Create df of terms x tweets

## load stop words from tidytext package
data("stop_words")

## lemmatize text, remove stop words, and extract word tokens
tweets %>%
  mutate(lemma_text = lemmatize_strings(text)) %>%
  unnest_tokens(word, lemma_text, token = "tweets", strip_url = TRUE) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!word %in% str_remove_all(stop_words$word, "'")) ->
  word_tokens

## select just word tokens and status id; save to .rds
word_tokens %>%
  select(status_id, word) %>%
  filter(!word %in% c("amp", "blacklivesmatter", "blm", "defundthepolice", "policebrutality")) ->
  tweets_words

saveRDS(tweets_words, "../data-clean/tweets_words.rds")


# Create df of hashtags x tweets
tweets %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(str_detect(word, "^#")) ->
  hashtag_tokens

## select just hashtag tokens and status id; save to .rds
hashtag_tokens %>%
  select(status_id, word) %>%
  filter(! word %in% c("#blm", "#blacklivesmatter", "#defundthepolice", "#policebrutality")) ->
  tweets_hashtags

saveRDS(tweets_hashtags, "../data-clean/tweets_hashtags.rds")


# Create df of mentions x tweets
tweets %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(str_detect(word, "^@")) ->
  mention_tokens

## select just mention tokens and status id; save to .rds
mention_tokens %>%
  select(status_id, word) ->
  tweets_mentions

saveRDS(tweets_mentions, "../data-clean/tweets_mentions.rds")


# Create df of emojis x tweets

## Load emoji codes from rtweet
emoji_codes <- rtweet::emojis
emoji_codes %>%
  mutate(escaped_codes = stringi::stri_escape_unicode(code),
         num_backslash = str_count(escaped_codes, "\\\\")) %>%
  arrange(desc(num_backslash)) ->
  emoji_codes

## Create regex pattern to catch emojis (more complex first)
emoji_pattern <- str_c(paste0("\\Q", emoji_codes$code, "\\E"), collapse = "|")

## Extract emojis from tweet text; create vector of strings containing only emojis
text_emojis <- c()
for (i in 1:nrow(tweets)) {
  tweet <- tweets$text[i]
  tweet_emojis <- unlist(str_extract_all(tweet, emoji_pattern))
  tweet_emojis <- str_c(tweet_emojis, collapse = " ")
  text_emojis <- append(text_emojis, tweet_emojis)
}

## Add to df
tweets$text_emoji <- text_emojis

## Get emoji tokens
tweets %>%
  unnest_tokens(word, text_emoji, token = "characters", to_lower = FALSE) ->
  emoji_tokens

## select just hashtag tokens and status id; save to .rds
emoji_tokens %>%
  select(status_id, word) ->
  tweets_emojis

tweets_emojis %>%
  inner_join(emoji_codes, by = c("word" = "code")) %>%
  select(status_id:description) ->
  tweets_emojis

saveRDS(tweets_emojis, "../data-clean/tweets_emojis.rds")
