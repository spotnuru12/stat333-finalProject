library(dplyr)
library(readr)

df <- read_csv("new_data_with_placements.csv")

# no confounders
lm_no_confounders <- lm(
  sqrt_reappearance ~ is_major_label,
  data = df
)
summary(lm_no_confounders)

# confounders: major_label & decade 
lm <- lm(
  sqrt_reappearance ~ is_major_label + No. + factor(decade),
  data = df
)
summary(lm)

plot(lm)
