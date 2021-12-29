# read in & combine clean tweet files
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

# limit tweets file for analysis; create combination hashtag variables
tweets %>%
  select(status_id, is_blm, is_blacklivesmatter, is_policebrutality, is_defundthepolice,
         sent_score_senticnet, sent_score_slang) %>%
  mutate(SenticNet = sent_score_senticnet,
         SlangSD = sent_score_slang,
         #Inclusive = case_when(is_blm == T ~ "#blm",
         #                      is_blacklivesmatter == T ~ "#blacklivesmatter",
         #                      is_policebrutality == T ~ "#policebrutality",
         #                      is_defundthepolice == T ~ "#defundthepolice",
         #                      is_blm == T | is_blacklivesmatter == T | is_defundthepolice == T | is_policebrutality == T ~ "Any blm-related hashtag"
         #),
         Exclusive = case_when(is_blm == T & is_blacklivesmatter == F & is_defundthepolice == F & is_policebrutality == F ~ "#blm",
                               is_blm == F & is_blacklivesmatter == T & is_defundthepolice == F & is_policebrutality == F ~ "#blacklivesmatter",
                               is_blm == F & is_blacklivesmatter == F & is_defundthepolice == T & is_policebrutality == F ~ "#defundthepolice",
                               is_blm == F & is_blacklivesmatter == F & is_defundthepolice == F & is_policebrutality == T ~ "#policebrutality",
                               is_blm == T & is_blacklivesmatter == T & is_defundthepolice == T & is_policebrutality == T ~ "All blm-related hashtags"
         ),
         BLM = case_when(is_blm == T & is_blacklivesmatter == F & is_defundthepolice == F & is_policebrutality == F ~ "#blm",
                         is_blm == T & is_blacklivesmatter == T & is_defundthepolice == F & is_policebrutality == F ~ "#blm, #blacklivesmatter",
                         is_blm == T & is_blacklivesmatter == F & is_defundthepolice == T & is_policebrutality == F ~ "#blm, #defundthepolice",
                         is_blm == T & is_blacklivesmatter == F & is_defundthepolice == F & is_policebrutality == T ~ "#blm, #policebrutality",
                         is_blm == T & is_blacklivesmatter == T & is_defundthepolice == T & is_policebrutality == F ~ "#blm, #blacklivesmatter, #defundthepolice",
                         is_blm == T & is_blacklivesmatter == T & is_defundthepolice == F & is_policebrutality == T ~ "#blm, #blacklivesmatter, #policebrutality",
                         is_blm == T & is_blacklivesmatter == F & is_defundthepolice == T & is_policebrutality == T ~ "#blm, #defundthepolice, #policebrutality",
                         is_blm == T & is_blacklivesmatter == T & is_defundthepolice == T & is_policebrutality == T ~ "#blm, #blacklivesmatter, #defundthepolice, #policebrutality"
         ),
         
         BlackLivesMatter = case_when(is_blm == F & is_blacklivesmatter == T & is_defundthepolice == F & is_policebrutality == F ~ "#blacklivesmatter",
                                      is_blm == T & is_blacklivesmatter == T & is_defundthepolice == F & is_policebrutality == F ~ "#blacklivesmatter, #blm",
                                      is_blm == F & is_blacklivesmatter == T & is_defundthepolice == T & is_policebrutality == F ~ "#blacklivesmatter, #defundthepolice",
                                      is_blm == F & is_blacklivesmatter == T & is_defundthepolice == F & is_policebrutality == T ~ "#blacklivesmatter, #policebrutality",
                                      is_blm == T & is_blacklivesmatter == T & is_defundthepolice == T & is_policebrutality == F ~ "#blacklivesmatter, #blm, #defundthepolice",
                                      is_blm == T & is_blacklivesmatter == T & is_defundthepolice == F & is_policebrutality == T ~ "#blacklivesmatter, #blm, #policebrutality",
                                      is_blm == F & is_blacklivesmatter == T & is_defundthepolice == T & is_policebrutality == T ~ "#blacklivesmatter, #defundthepolice, #policebrutality",
                                      is_blm == T & is_blacklivesmatter == T & is_defundthepolice == T & is_policebrutality == T ~ "#blacklivesmatter, #blm, #defundthepolice, #policebrutality"
         ),
         
         DefundthePolice = case_when(is_blm == F & is_blacklivesmatter == F & is_defundthepolice == T & is_policebrutality == F ~ "#defundthepolice",
                                     is_blm == T & is_blacklivesmatter == F & is_defundthepolice == T & is_policebrutality == F ~ "#defundthepolice, #blm",
                                     is_blm == F & is_blacklivesmatter == T & is_defundthepolice == T & is_policebrutality == F ~ "#defundthepolice, #blacklivesmatter",
                                     is_blm == F & is_blacklivesmatter == F & is_defundthepolice == T & is_policebrutality == T ~ "#defundthepolice, #policebrutality",
                                     is_blm == T & is_blacklivesmatter == T & is_defundthepolice == T & is_policebrutality == F ~ "#defundthepolice, #blm, #blacklivesmatter",
                                     is_blm == T & is_blacklivesmatter == F & is_defundthepolice == T & is_policebrutality == T ~ "#defundthepolice, #blm, #policebrutality",
                                     is_blm == F & is_blacklivesmatter == T & is_defundthepolice == T & is_policebrutality == T ~ "#defundthepolice, #blacklivesmatter, #policebrutality",
                                     is_blm == T & is_blacklivesmatter == T & is_defundthepolice == T & is_policebrutality == T ~ "#defundthepolice, #blm, #blacklivesmatter, #policebrutality"
         ),
         
         PoliceBrutality = case_when(is_blm == F & is_blacklivesmatter == F & is_defundthepolice == F & is_policebrutality == T ~ "#policebrutality",
                                     is_blm == T & is_blacklivesmatter == F & is_defundthepolice == F & is_policebrutality == T ~ "#policebrutality, #blm",
                                     is_blm == F & is_blacklivesmatter == T & is_defundthepolice == F & is_policebrutality == T ~ "#policebrutality, #blacklivesmatter",
                                     is_blm == F & is_blacklivesmatter == F & is_defundthepolice == T & is_policebrutality == T ~ "#policebrutality, #defundthepolice",
                                     is_blm == T & is_blacklivesmatter == T & is_defundthepolice == F & is_policebrutality == T ~ "#policebrutality, #blm, #blacklivesmatter",
                                     is_blm == T & is_blacklivesmatter == F & is_defundthepolice == T & is_policebrutality == T ~ "#policebrutality, #blm, #defundthepolice",
                                     is_blm == F & is_blacklivesmatter == T & is_defundthepolice == T & is_policebrutality == T ~ "#policebrutality, #blacklivesmatter, #defundthepolice",
                                     is_blm == T & is_blacklivesmatter == T & is_defundthepolice == T & is_policebrutality == T ~ "#policebrutality, #blm, #blacklivesmatter, #defundthepolice"
         )
  ) %>%
  mutate(
    #Inclusive = as.factor(Inclusive),
         Exclusive = as.factor(Exclusive),
         BLM = as.factor(BLM),
         BlackLivesMatter = as.factor(BlackLivesMatter),
         PoliceBrutality = as.factor(PoliceBrutality),
         DefundthePolice = as.factor(DefundthePolice)) ->
  tweets_analysis_temp

tweets_analysis_temp %>%
  distinct() ->
  tweets_analysis

saveRDS(tweets_analysis, file = "../data-clean/tweets_analysis.rds")

# read in tweets_hashtags dataset
tweets_hashtags <- readRDS("../data-clean/tweets_hashtags.rds")
tweets_hashtags %>%
  distinct() ->
  tweets_hashtags

tweets_hashtags %>%
  filter(word=="#blm" | word=="#blacklivesmatter" | word=="#defundthepolice" | word=="#policebrutality") %>%
  left_join(tweets_analysis, by="status_id") %>%
  mutate(Inclusive = word) %>%
  mutate(Inclusive = as.factor(Inclusive)) %>%
  select(status_id, word, SenticNet, SlangSD, Inclusive, Exclusive, BLM, BlackLivesMatter, DefundthePolice, PoliceBrutality) ->
  tweets_incl

saveRDS(tweets_incl, file = "../data-clean/tweets_incl.rds")

#tweets_hashtags_excl <- tweets_hashtags_incl[!(duplicated(tweets_hashtags_incl$status_id) |
#                                                 duplicated(tweets_hashtags_incl$status_id,
#                                                            fromLast = TRUE)), ]

#drop NAs from exclusive group
tweets_incl %>%
  drop_na(Exclusive) ->
  tweets_excl_temp

#drop duplicate IDs so we have an accurate list of "All BLM-related hashtags"
tweets_excl_temp %>%
  distinct(status_id, Exclusive, .keep_all=TRUE) ->
  tweets_excl

saveRDS(tweets_excl, file = "../data-clean/tweets_excl.rds")