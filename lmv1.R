library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(tidyr)

# Load the dataset
df <- read_csv("reappears_after_first_cleaned.csv")

# lm_model <- lm(reappears_after_first ~ is_major_label, data = df_modified)

# Fit a logistic regression model: binary outcome ~ major label
glm_model <- glm(reappears_after_first ~ is_major_label, data = df, family = "binomial")

# Print the model summary
summary(glm_model)

# Plot diagnostic plots (these are still limited for binary outcomes)
par(mfrow = c(2, 2))
plot(glm_model)
