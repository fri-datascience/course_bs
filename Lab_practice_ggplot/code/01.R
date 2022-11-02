# libraries --------------------------------------------------------------------
library(cowplot)
library(ggplot2)
library(tidyverse)

# load the data-----------------------------------------------------------------
df <- read.csv("../data/50_startups.csv")

# 1 ----------------------------------------------------------------------------
# get state counts
df_state <- df %>% group_by(state) %>% summarise(count = n())

# plot
p1 <- ggplot(df_state, aes(x = state, y = count)) +
        geom_bar(stat = "identity")
p1

# 2 ----------------------------------------------------------------------------
# plot
p2 <- ggplot(df, aes(x = profit)) +
    geom_histogram(bins = 10)
p2

# 3 ----------------------------------------------------------------------------
# plot
p3 <- ggplot(df, aes(x = research)) +
    geom_density(color = NA, fill = "grey50")
p3

# 4 ----------------------------------------------------------------------------
plot_grid(p1, p2, p3, ncol = 1, scale = 0.95,
          labels = "auto", label_size = 32)
