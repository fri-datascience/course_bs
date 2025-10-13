# library
library(tidyverse)
library(ggridges)
library(forcats)

# set year
year <- 2025

# load the data
df <- read.csv(paste0("./session_02_probabilistic_thinking/data/expressions", year, ".csv"))
df <- df %>% select(-Timestamp)

# to long format
df_long <- df %>% gather(key = "term", value = "value")
df_long$term <- gsub("\\.", " ", df_long$term)
df_long$value <- ifelse(df_long$value < 1, df_long$value * 100, df_long$value)

# drop na
df_long <- df_long %>% filter(!is.na(value))

# sort descendingly by average value
average_values <- df_long %>%
  group_by(term) %>%
  summarize(average_value = mean(value)) %>%
  arrange(average_value)
df_long$term <- factor(df_long$term, levels = average_values$term)

# plot
ggplot(df_long, aes(x = value, y = term, fill = term)) +
  geom_density_ridges(fill = "skyblue", jittered_points = TRUE) +
  theme(legend.position = "none") +
  xlab("Distribution") +
  ylab("Uncertainty term") +
  xlim(0, 100)

# save
ggsave(
  paste0("session_02_probabilistic_thinking/figs/expressions", year, ".png"),
  width = 1920, height = 1920, units = "px", dpi = 300
)
