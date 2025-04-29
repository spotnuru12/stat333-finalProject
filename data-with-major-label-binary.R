library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(tidyr)

# Make sure to use setwd() in the terminal first before running this.

# Load the dataset
df <- read_csv("STAT333_full_with_labels.csv")

# Count unique values in the record_label column
unique_labels <- df %>%
  distinct(record_label) %>%
  pull(record_label)

# Print the unique labels
cat("Number of unique record labels:", length(unique_labels), "\n\n")
print(unique_labels)

# Mutate new column
df <- df %>%
  mutate(
    record_label_list = str_split(record_label, pattern = "\\s*/\\s*")
  )


# THERES AN ISSUE WHERE LABELS LIKE OVO (WARNER) AND DEF JAM (UMG) ARE HOUSED/DISTRIBUTED UNDER MAJOR RECORD LABELS
# Updated dictionary of major labels (including sub‐labels under the Big Three) by decade
major_labels_list <- list(
  `1950s` = c(
    "RCA Records", "Columbia Records", "Capitol Records",
    "Atlantic Records", "MOTOWN",
    "Decca Records", "Imperial Records", "Chess Records"
  ),
  
  `1960s` = c(
    "RCA Records", "Columbia Records", "Capitol Records",
    "Atlantic Records", "MOTOWN",
    "EMI Records", "Island Records", "Decca Records",
    "Imperial Records", "Chess Records",
    # Rhino Atlantic did reissues even back then
    "Rhino Atlantic"
  ),
  
  `1970s` = c(
    "RCA Records", "Columbia Records", "Capitol Records",
    "Atlantic Records", "MOTOWN",
    "EMI Records", "Island Records", "PolyGram",
    "Warner Records", "MCA Records",
    # sub-labels / catalog divisions showing up in your data
    "Rhino", "Rhino Atlantic", "Legacy", "UNI"
  ),
  
  `1980s` = c(
    "RCA Records", "Columbia Records", "Capitol Records",
    "Atlantic Records", "MOTOWN",
    "EMI Records", "Island Records", "PolyGram",
    "Sony Music Entertainment", "Warner Music Group",
    "MCA Records",
    # catalog & reissue labels
    "Rhino", "Rhino Atlantic", "Legacy", "UNI"
  ),
  
  `1990s` = c(
    "Universal Music Group", "Sony Music Entertainment",
    "Warner Music Group", "Bertelsmann Music Group",
    "EMI Records", "Island Records", "PolyGram",
    "MCA Records",
    # big-three sub-labels
    "Rhino", "Legacy", "Arista", "A&M"
  ),
  
  `2000s` = c(
    "Universal Music Group", "Sony Music Entertainment",
    "Warner Music Group", "EMI", "BMG",
    # modern imprint sub-labels
    "Interscope Records", "Def Jam Recordings",
    "Jive", "Atlantic Records", "Geffen", "Epic"
  ),
  
  `2010s` = c(
    "Universal Music Group", "Sony Music Entertainment",
    "Warner Music Group",
    # today’s big-three imprints
    "Interscope Records", "Def Jam Recordings",
    "Republic Records", "OVO Sound", "Aftermath",
    "Jive", "Geffen", "Epic", "RCA Records"
  ),
  
  `2020s` = c(
    "Universal Music Group", "Sony Music Entertainment",
    "Warner Music Group",
    # current sub-labels you saw in your 2020s top-10
    "Interscope Records", "Def Jam Recordings",
    "Republic Records", "OVO Sound", "Aftermath",
    "Kemosabe Records", "Mercury Records",
    "Atlantic Records", "Epic", "RCA Records"
  )
)

# Update each row if ANY of its record_label_list is in the decade’s majors
df_flagged <- df %>%
  mutate(
    # derive “1970s” etc.
    decade = paste0(floor(Year / 10) * 10, "s")
  ) %>%
  rowwise() %>%
  mutate(
    is_major_label = any(
      str_trim(record_label_list) %in% major_labels_list[[decade]]
    )
  ) %>%
  ungroup()

df_flagged %>% select(primary_artist, Year, record_label_list, is_major_label) %>% slice_head(n=10)

df_flagged %>% slice_head(n=10)

df_flagged %>%count(is_major_label)


# ChatGPT code to print out top 10 record labels for each decade:

# Assuming df is your original data frame with Year and record_label_list
df %>%
  # 1) derive the decade string
  mutate(decade = paste0(floor(Year / 10) * 10, "s")) %>%
  # 2) explode the list-column so each label is its own row
  unnest_longer(record_label_list) %>%
  # 3) trim whitespace
  mutate(record_label_list = str_trim(record_label_list)) %>%
  # 4) count per decade & label
  group_by(decade, record_label_list) %>%
  summarise(n = n(), .groups = "drop") %>%
  # 5) for each decade, pick the top 10 by count
  group_by(decade) %>%
  slice_max(order_by = n, n = 10) %>%
  ungroup() %>%
  # 6) show them sorted nicely
  arrange(decade, desc(n)) %>%
  print(n = Inf)


write_csv(df_flagged, "data-with-major-label-binary.csv")

