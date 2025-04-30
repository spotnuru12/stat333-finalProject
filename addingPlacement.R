library(dplyr)
library(readr)

df_placements <- read_csv("finalData.csv")  
df_major_label <- read_csv("data-with-major-label-binary (1).csv")

df <- df_major_label %>%
  filter(is_first_appearance) %>%
  left_join(
    df_placements %>% select(Title, Year, primary_artist, No.),
    by = c("Title", "Year", "primary_artist")  
  ) %>%
  mutate(
    reappearance_count = total_appearances - 1,
    sqrt_reappearance  = sqrt(reappearance_count)
  )

mod_first <- lm(
  sqrt_reappearance ~ is_major_label + No. + factor(decade),
  data = df
)
summary(mod_first)

readr::write_csv(df, "new_data_with_placements.csv")

