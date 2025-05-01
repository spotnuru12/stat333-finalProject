library(ggplot2)
library(readr)
library(dplyr)
library(ggpubr)

df <- read_csv("billboard_filtered_for_model.csv", show_col_types = FALSE)

df <- df %>%
  mutate(
    is_major_label = factor(is_major_label, labels = c("Independent", "Major"))
  )

# Generate Figure 1
ggplot(df, aes(x = is_major_label, y = sqrt_reappearance, fill = is_major_label)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.3, color = "black", size = 1.2) +
  stat_compare_means(method = "t.test", label.y = max(df$sqrt_reappearance, na.rm = TRUE) + 0.5) +
  labs(
    title = "Figure 1. Reappearance by Record Label Type",
    x = "Record Label",
    y = "√(Reappearance Count)"
  ) +
  scale_fill_manual(values = c("Independent" = "#b2df8a", "Major" = "#1f78b4")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    legend.position = "none"
  )



