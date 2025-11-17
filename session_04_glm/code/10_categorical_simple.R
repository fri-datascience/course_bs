# libraries --------------------------------------------------------------------
library(cmdstanr)
library(ggplot2)
library(bayesplot)
library(posterior)
library(tidyverse)

# modelling and data prep ------------------------------------------------------
# compile the model
model <- cmdstan_model("./session_04_glm/models/categorical.stan")

# load data
data <- read.csv("./session_04_glm/data/continents.csv", stringsAsFactors = TRUE)

# contrasts (one hot encoding with a reference category)
contrasts(data$hemisphere) <- contr.treatment(n_distinct(data$hemisphere))

# default sets equator as reference category, set south instead
# data$hemisphere <- relevel(data$hemisphere, ref = "south")

# display contrasts
contrasts(data$hemisphere)

# normalize longitude (store mean/sd for interpretability later)
longitude_scaled <- scale(data$longitude)
lon_center <- attr(longitude_scaled, "scaled:center")
lon_scale <- attr(longitude_scaled, "scaled:scale")
data$longitude <- as.numeric(longitude_scaled)

standardize_longitude <- function(x) {
  (x - lon_center) / lon_scale
}

# the intercept is part of the model matrix (1st column always equals 1)
X <- model.matrix(~ longitude + hemisphere, data)

# show a couple of top rows to check if all is OK
head(X)

# default reference is europe, change to asia
# continent_order <- c("africa", "europe", "asia")

# dependent variable (convert factor to integer levels for Stan)
data$continent <- factor(data$continent, levels = continent_order)
continent_levels <- levels(data$continent)
y <- as.integer(data$continent)

# show levels of y
continent_levels

# stan_data
stan_data <- list(n = nrow(data), m = ncol(X), k = length(continent_levels), x = X, y = y)

# fit
fit <- model$sample(
  data = stan_data,
  parallel_chains = 4,
  seed = 1
)

# diagnostics ------------------------------------------------------------------
# traceplot for beta parameters
mcmc_trace(fit$draws("beta"))

# summary of betas
fit$summary("beta")

# analysis ---------------------------------------------------------------------
# extract parameters
df_betas <- as_draws_df(fit$draws("beta"))
df_betas <- df_betas %>% select(-.chain, -.iteration, -.draw)

# beta matrix composed of sample means
# working with means from here one for brevity and simplicity purposes
# a true bayesian approach would be to work with samples all the way
betas <- matrix(colMeans(df_betas), nrow = 3, ncol = 4)

# inspect betas
# rows represent categories: africa, asia, europe (reference in last row)
# columns represent predictors: intercept, longitude, hemisphere (north), hemisphere (south)
# reference hemisphere is equator
betas

# helper softmax function ------------------------------------------------------
softmax <- function(x) {
  as.vector(exp(x) / sum(exp(x)))
}

# calculate example probabilities ----------------------------------------------
# example 1: Point in equator, longitude = 25 (typical for Africa)
# x <- c(intercept, longitude, hemisphere_north, hemisphere_south)
x1 <- c(1, standardize_longitude(25), 0, 0)
prob1 <- softmax(betas %*% x1)
names(prob1) <- levels(data$continent)
cat("\nProbabilities for: longitude=25, hemisphere=equator\n")
print(round(prob1, 3))

# example 2: Point in north, longitude = 0 (typical for Western Europe)
x2 <- c(1, standardize_longitude(0), 1, 0)
prob2 <- softmax(betas %*% x2)
names(prob2) <- levels(data$continent)
cat("\nProbabilities for: longitude=0, hemisphere=north\n")
print(round(prob2, 3))

# example 3: Point in north, longitude = 110 (typical for East Asia)
x3 <- c(1, standardize_longitude(110), 1, 0)
prob3 <- softmax(betas %*% x3)
names(prob3) <- levels(data$continent)
cat("\nProbabilities for: longitude=110, hemisphere=north\n")
print(round(prob3, 3))

# plot probabilities across longitude ranges -----------------------------------
precision <- 200
longitude_range <- seq(-20, 140, length.out = precision)
longitude_range_z <- standardize_longitude(longitude_range)

# calculate probabilities from longitude
df_probs <- data.frame(
  Longitude = numeric(),
  Continent = factor(),
  Probability = numeric(),
  Hemisphere = factor()
)

# continents
continent_levels <- levels(data$continent)

# iterate over longitude and hemisphere combinations
for (hemi in c("equator", "north", "south")) {
  hemi_north <- ifelse(hemi == "north", 1, 0)
  hemi_south <- ifelse(hemi == "south", 1, 0)

  for (i in 1:precision) {
    x <- c(1, longitude_range_z[i], hemi_north, hemi_south)
    probs <- softmax(betas %*% x)

    df_temp <- data.frame(
      Longitude = longitude_range[i],
      Continent = continent_levels,
      Probability = probs,
      Hemisphere = hemi
    )

    df_probs <- rbind(df_probs, df_temp)
  }
}

# plot
ggplot(data = df_probs, aes(x = Longitude, y = Probability, fill = Continent)) +
  geom_area(linewidth = 0.5, alpha = 0.8) +
  ggtitle("Continent Probability by Longitude and Hemisphere") +
  scale_fill_brewer(palette = "Set2") +
  facet_grid(Hemisphere ~ .) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Longitude (degrees east)", y = "Probability")
