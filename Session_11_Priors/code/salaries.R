# libraries
library(ggplot2)
library(cowplot)
library(tidyverse)

# load the data
df <- read.csv("../data/salaries.csv")

# data in k
df$salary <- df$salary / 1000

# setup
precision <- 10000
x <- seq(0, 10, length.out=precision)

# plot 1
df1 <- data.frame(x=x, y=dgamma(x, 10, 2))
p1 <- ggplot() +
  geom_density(data=df, aes(x=salary), color=NA, fill="black", alpha=0.2) +
  geom_line(data=df1, aes(x=x, y=y), size=1) +
  theme_minimal() +
  xlim(0, 10) +
  ylim(0, 0.5) +
  xlab("Salary") + 
  ylab("Density") +
  ggtitle("Team 1")

# plot 2
df2 <- data.frame(x=x, y=dgamma(x, 7, 3))
p2 <- ggplot() +
  geom_density(data=df, aes(x=salary), color=NA, fill="black", alpha=0.2) +
  geom_line(data=df2, aes(x=x, y=y), size=1) +
  theme_minimal() +
  xlim(0, 10) +
  ylim(0, 0.5) +
  xlab("Salary") + 
  ylab("Density") +
  ggtitle("Team 2")

# plot 3
df3 <- data.frame(x=x, y=dgamma(x, 9, 4))
p3 <- ggplot() +
  geom_density(data=df, aes(x=salary), color=NA, fill="black", alpha=0.2) +
  geom_line(data=df3, aes(x=x, y=y), size=1) +
  theme_minimal() +
  xlim(0, 10) +
  ylim(0, 0.5) +
  xlab("Salary") + 
  ylab("Density") +
  ggtitle("Team 3")

# plot 4
df4 <- data.frame(x=x, y=dgamma(x, 6, 1))
p4 <- ggplot() +
  geom_density(data=df, aes(x=salary), color=NA, fill="black", alpha=0.2) +
  geom_line(data=df4, aes(x=x, y=y), size=1) +
  theme_minimal() +
  xlim(0, 10) +
  ylim(0, 0.5) +
  xlab("Salary") + 
  ylab("Density") +
  ggtitle("Team 4")

# plot grid
plot_grid(p1, p2, p3, p4, scale=0.9, ncol=1)
