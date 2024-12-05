# libraries --------------------------------------------------------------------
library(ggplot2)
library(tidyverse)

# load the data-----------------------------------------------------------------
df <- read.csv("../data/flanker.csv")

# plot -------------------------------------------------------------------------
# get means
df_mean <- df %>%
  group_by(subject, congruency) %>%
  summarize(mean_rt = round(mean(rt), 2)) %>%
  arrange(mean_rt)

df_mean_combined <- df %>%
  group_by(subject) %>%
  summarize(mean_rt = round(mean(rt), 2)) %>%
  arrange(mean_rt)

df$subject <- factor(df$subject, levels = df_mean_combined$subject)

# plot
ggplot() +
  geom_density(data = df, aes(x = rt), color = NA, fill = "grey75") +
  facet_grid(subject ~ congruency) +
  geom_segment(data = df_mean,
               aes(x = mean_rt, xend = mean_rt, y = 0, yend = 3.9),
               linewidth = 1, color = "grey25", linetype = "dashed") +
  geom_text(data = df_mean, aes(x = mean_rt, y = 4, label = mean_rt))
