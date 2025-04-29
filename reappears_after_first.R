library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(tidyr)

# Make sure to use setwd() in the terminal first before running this.

# Load the dataset
df <- read_csv("data-with-major-label-binary.csv")

df %>% slice_head(n=10)

df %>% count(is_first_appearance)

df_flagged <- df %>%
  group_by(primary_artist) %>%
  mutate(
    # capture the year of their first appearance
    first_year = Year[is_first_appearance],
    # check if there is any row with Year > that first_year
    reappears_after_first = any(Year > first_year)
  ) %>%
  ungroup() %>%
  select(-first_year)  # optional: drop the helper column

df_flagged %>% slice_head(n = 10)
df_flagged %>% count(reappears_after_first)

df_flagged %>% count(is_first_appearance)

# df_modified does not have rows such that is_first_appearance is false
df_modified <- df_flagged %>%
  filter(is_first_appearance)

df_modified %>% slice_head(n=10) 

df_modified %>% count(is_first_appearance)

# Dropping is_first_appearance because now every row is is_first_appearance == TRUE
df_modified <- df_flagged %>% select(-is_first_appearance)

# Dropping record_label_list b/c unnecessary after populating is_major_label
df_modified <- df_modified %>% select(-record_label_list)

# Dropping total_appearances, artist_list because irrelevant to our thesis
df_modified <- df_modified %>% select(-c(total_appearances, artist_list))


df_modified %>% slice_head(n=10)

# Write csv file
write_csv(df_modified, "reappears_after_first_cleaned.csv")
