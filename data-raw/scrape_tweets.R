# Get tweets from Twitter using rtweet

# load packages
library(tidyverse)
library(rtweet)

# Download 50K tweets on #BLM or #DefundThePolice
query = "(#blm OR #blacklivesmatter OR #defundthepolice OR #policebrutality) lang:en"
tweets <- search_tweets(q = query,
                        n = 50000,
                        retryonratelimit = TRUE)

# Add latitude & longitude variables
tweets <- lat_lng(tweets)

# Save as RDS file
saveRDS(tweets, "../../blm_tweets.rds")