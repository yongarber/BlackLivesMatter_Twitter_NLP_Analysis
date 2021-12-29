# Creating mapping variables

# load libraries
library(tidyverse)
library(tidytext)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(ggspatial)
library(maps)

# load in clean_blm_tweets
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

# load data for world map, list of world cities, US state lists

city_list <- world.cities %>%
  select(name, country.etc, lat, long) %>%
  rename(city_loc = name,
         country_loc = country.etc,
         city_lat = lat,
         city_lon = long)

city_list$country_loc[city_list$country_loc == "USA"] <- "United States"
city_list$country_loc[city_list$country_loc == "UK"] <- "United Kingdom"
city_list$country_loc[city_list$country_loc == "Korea South"] <- "South Korea"
city_list$country_loc[city_list$country_loc == "Korea North"] <- "North Korea"
city_list <- city_list[-29627,] # remove Portland, Maine, assuming tweets are from Oregon
city_list <- city_list[-8027,] # remove Columbus, Georgia
city_list <- city_list[-8029,] # remove Columbus, Indiana
city_list <- city_list[-965,] # remove Alexandria, Virginia

world <- ne_countries(scale = "medium", returnclass = "sf")

world_points <- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

world_points %>%
  select(name, X, Y) %>%
  rename(country_loc = name,
         country_lat = Y,
         country_lon = X) -> world_points

world_points <- st_drop_geometry(world_points)

us_state <- ne_states(country = "united states of america", returnclass = "sf")

us_states_points <- st_centroid(us_state)
us_states_points <- cbind(us_state, us_states_points)
us_states_points %>%
  select(name, postal, latitude, longitude) %>%
  rename(state_loc = name,
         state_lat = latitude,
         state_lon = longitude) -> us_states_points

us_states_points <- st_drop_geometry(us_states_points)

# add country, state, and city lat/long to tweets

tweets %>%
  left_join(world_points) -> tweets

tweets %>%
  left_join(us_states_points) -> tweets

tweets %>%
  mutate(city_loc = case_when(location %in% city_list$name ~ location,
                              place_type == "city" ~ place_name)) -> tweets

tweets %>%
  left_join(city_list) -> tweets

# create summary lat/lon variables 
tweets %>%
  mutate(map_lat = case_when(!is.na(lat) ~ lat,
                             is.na(lat) & !is.na(city_lat) ~ city_lat,
                             is.na(lat) & is.na(city_lat) & !is.na(state_lat) ~ state_lat,
                             is.na(lat) & is.na(city_lat) & is.na(state_lat) & !is.na(country_lat) ~ country_lat),
         map_lon = case_when(!is.na(lng) ~ lng,
                             is.na(lng) & !is.na(city_lon) ~ city_lon,
                             is.na(lng) & is.na(city_lon) & !is.na(state_lon) ~ state_lon,
                             is.na(lng) & is.na(city_lon) & is.na(state_lon) & !is.na(country_lon) ~ country_lon)) -> tweets

tweets %>%
  mutate(city_lat = ifelse(is.na(city_lat) & !is.na(lat), lat, city_lat),
         city_lon = ifelse(is.na(city_lon) & !is.na(lng), lng, city_lon)) -> tweets

tweets %>%
  distinct(tweet_num, .keep_all = TRUE) -> tweets

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

