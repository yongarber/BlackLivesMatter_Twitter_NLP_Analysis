# Split large file into multiple smaller files
library(tidyverse)

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