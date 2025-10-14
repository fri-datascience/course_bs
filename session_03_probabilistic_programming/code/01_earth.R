# library
library(tidyverse)
library(ggplot2)

# x axis
x <- seq(0, 1, 0.1)

# prior
prior <- rep(1, 11) / 11

# plot our initial belief
df <- data.frame(x, prior)
ggplot(df, aes(x, prior)) +
  geom_bar(stat = "identity") +
  xlab("Water percentage") +
  ylab("Belief") +
  ylim(0, 1)

# water
water <- x * 2

# land
land <- 2 - water

# plot likelihood
df <- data.frame(x, y = water, group = "water")
df <- rbind(df, data.frame(x, y = land, group = "land"))
ggplot(df, aes(x, y, color = group)) +
  geom_line(linewidth = 2) +
  xlab("Water percentage") +
  ylab("Likelihood") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.title = element_blank())

# set posterior
our_belief <- NULL

saw_land <- function(our_belief) {
  # calculate the posterior
  if (is.null(our_belief)) {
    posterior <- prior * land
  } else {
    posterior <- our_belief * land
  }
  posterior <- posterior / sum(posterior)

  # plot
  df <- data.frame(x, posterior)
  p <- ggplot(df, aes(x, posterior)) +
    geom_bar(stat = "identity") +
    xlab("Water percentage") +
    ylab("Belief") +
    ylim(0, 1)
  print(p)

  results <- list(our_belief = posterior, plot = p)
  return(results)
}

saw_water <- function(our_belief) {
  # calculate the posterior
  if (is.null(our_belief)) {
    posterior <- prior * water
  } else {
    posterior <- our_belief * water
  }
  posterior <- posterior / sum(posterior)

  # plot
  df <- data.frame(x, posterior)
  p <- ggplot(df, aes(x, posterior)) +
    geom_bar(stat = "identity") +
    xlab("Water percentage") +
    ylab("Belief") +
    ylim(0, 1)
  print(p)

  results <- list(our_belief = posterior, plot = p)
  return(results)
}

# init belief is equal to prior
r <- list()
r$our_belief <- prior

# sample location from https://www.realrandom.net/location.html

# saw land
r <- saw_land(r$our_belief)

# saw water
r <- saw_water(r$our_belief)

# ------------------------------------------------------------------------------
# WARNING: spoilers below

# plot the truth
r$plot +
  geom_vline(xintercept = 0.71, linetype = "dashed", size = 1)
