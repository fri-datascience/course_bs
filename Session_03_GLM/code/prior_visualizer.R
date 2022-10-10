# libraries --------------------------------------------------------------------
library(ggplot2)
library(ggdist)
library(distributional)
library(rmutil)
library(tidyverse)
library(cowplot)

# plot -------------------------------------------------------------------------
dfl <- data.frame(dist = "laplace")
dfn <- data.frame(dist = "norm")

p1 <- ggplot() +
  stat_dist_slab(data = dfn, aes(y = 0, dist = dist, arg1 = 0, arg2 = 1),
                 alpha = 0.75, fill = "skyblue", normalize = "none", scale = 1) +
  xlim(-5, 5) +
  xlab("") +
  ylab("density") +
  coord_cartesian(ylim=c(0, 0.6)) +
  theme_minimal() +
  ggtitle("Normal") +
  theme(plot.title = element_text(hjust = 0.5))

p2 <- ggplot() +
  stat_dist_slab(data = dfl, aes(y = 0, dist = dist, arg1 = 0, arg2 = 1),
                 alpha = 0.75, fill = "skyblue", normalize = "none", scale = 1) +
  xlim(-5, 5) +
  xlab("") +
  ylab("density") +
  coord_cartesian(ylim=c(0, 0.6)) +
  theme_minimal() +
  ggtitle("Laplace") +
  theme(plot.title = element_text(hjust = 0.5))

plot_grid(p1, p2, nrow = 2, scale = 0.9)
