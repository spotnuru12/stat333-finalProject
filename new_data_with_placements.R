library(dplyr)
library(readr)

df_placements <- read_csv("finalData.csv") # has no. col 
df_major_label <- read_csv("data-with-major-label-binary-cleaned.csv") #everything else

df <- df_major_label %>%
  filter(is_first_appearance) %>%
  left_join(
    df_placements %>% select(Title, Year, primary_artist, No.),
    by = c("Title", "Year", "primary_artist")
  ) %>%
  mutate(
    # squaring reappearance count
    reappearance_count  = total_appearances - 1,
    sqrt_reappearance = sqrt(reappearance_count),
    
    reappears_after_first = reappearance_count > 0
  )

readr::write_csv(df, "new_data_with_placements.csv")

