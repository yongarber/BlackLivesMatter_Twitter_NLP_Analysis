# Clean the tweets scraped from Twitter

library(tidyverse)

blm_tweets <- readRDS("../../blm_tweets.rds")

blm_tweets %>%
  select(user_id,
         status_id,
         created_at,
         screen_name,
         text,
         is_quote,
         is_retweet,
         favorite_count,
         retweet_count,
         quote_count,
         reply_count,
         urls_expanded_url,
         media_expanded_url,
         media_type,
         ext_media_expanded_url,
         ext_media_type,
         mentions_screen_name,
         place_name,
         place_full_name,
         place_type,
         country,
         country_code,
         name,
         location,
         followers_count,
         friends_count,
         listed_count,
         statuses_count,
         favourites_count,
         account_created_at,
         verified,
         lat,
         lng) ->
  blm_tweets


# Clean list columns: urls_expanded_url, media_expanded_url, media_type, ext_media_expanded_url,
# ext_media_type, and mentions_screen_name
#
# Convert list to collapsed string with space in between list elements

# urls_expanded_url
url_text <- c()

for (i in 1:nrow(blm_tweets)) {
  text <- str_c(unlist(blm_tweets$urls_expanded_url[i]), collapse = " ")
  url_text <- append(url_text, text)
}

blm_tweets$urls_expanded_url <- url_text

# media_expanded_url
media_url_text <- c()

for (i in 1:nrow(blm_tweets)) {
  text <- str_c(unlist(blm_tweets$media_expanded_url[i]), collapse = " ")
  media_url_text <- append(media_url_text, text)
}

blm_tweets$media_expanded_url <- media_url_text

# media_type
media_text <- c()

for (i in 1:nrow(blm_tweets)) {
  text <- str_c(unlist(blm_tweets$media_type[i]), collapse = " ")
  media_text <- append(media_text, text)
}

blm_tweets$media_type <- media_text

# ext_media_expanded_url
ext_media_url_text <- c()

for (i in 1:nrow(blm_tweets)) {
  text <- str_c(unlist(blm_tweets$ext_media_expanded_url[i]), collapse = " ")
  ext_media_url_text <- append(ext_media_url_text, text)
}

blm_tweets$ext_media_expanded_url <- ext_media_url_text

# ext_media_type
ext_media_text <- c()

for (i in 1:nrow(blm_tweets)) {
  text <- str_c(unlist(blm_tweets$ext_media_type[i]), collapse = " ")
  ext_media_text <- append(ext_media_text, text)
}

blm_tweets$ext_media_type <- ext_media_text

# mentions_screen_name
mentions_text <- c()

for (i in 1:nrow(blm_tweets)) {
  text <- str_c(unlist(blm_tweets$mentions_screen_name[i]), collapse = " ")
  mentions_text <- append(mentions_text, text)
}

blm_tweets$mentions_screen_name <- mentions_text


# Save cleaned df to RDS object
saveRDS(blm_tweets, "../data-clean/clean_blm_tweets.rds")

