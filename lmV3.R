df <- read_csv("billboard_top40_cleaned.csv") 
               

df_lm <- df %>%
  mutate(
    genre = factor(genre),
    decade = factor(decade)
  )

lm_model <- lm(
  sqrt_reappearance ~ is_major_label + No. + decade + duration_sec + genre,
  data = df_lm
)

summary(lm_model)

