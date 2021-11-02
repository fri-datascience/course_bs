# libraries --------------------------------------------------------------------
library(cmdstanr)  # for interfacing Stan
library(ggplot2)   # for visualizations
library(posterior) # for extracting samples
library(bayesplot) # for some quick MCMC visualizations
library(mcmcse)    # for comparing samples and calculating MCSE
library(arm)       # for logit and inverse logit functions
library(tidyverse) # for data manipulations


# color palette ----------------------------------------------------------------
# 1 - #a6cee3
# 2 - #1f78b4
# 3 - #b2df8a


# data prep --------------------------------------------------------------------
data <- read.csv("../data/golf.csv", sep="\t")

# input data to stan will be always the same
stan_data <- list(N = nrow(data),
                  n = data$n,
                  x = data$distance,
                  y = data$y)

# distances sequence used later on for visualizations
precision <- 100
x <- seq(from=0, to=max(data$distance), length.out=precision)

# calculate error bars in data for visualizations
df_golf <- data.frame(x=numeric(), p=numeric(), q5=numeric(), q95=numeric())
for (i in 1:nrow(data)) {
  # get row
  row <- data[i,]
  
  # calculate percentage
  p <- row$y / row$n
  
  # draw
  b <- rbinom(1000, row$n, p) / row$n
  
  # quantiles
  q5 <- quantile(b, 0.05)
  q95 <- quantile(b, 0.95)
  
  # bind
  df_golf <- df_golf %>% add_row(x=row$distance, p=p, q5=q5, q95=q95)
}

# plot data
ggplot(df_golf, aes(x=x, y=p, ymin=q5, ymax=q95)) +
  geom_errorbar(color="grey75") +
  geom_point(color="grey25") +
  theme_minimal() +
  ylim(0, 1)


# binomial logit modelling -----------------------------------------------------
# compile the model
model <- cmdstan_model("../models/binomial_logit.stan")

# fit
fit_binomial <- model$sample(
  data = stan_data,
  seed = 1
)

# traceplot
mcmc_trace(fit_binomial$draws())

# summary
fit_binomial$summary()

# extract samples
df_binomial_samples <- as_draws_df(fit_binomial$draws())

# print key parameters
mcse(df_binomial_samples$a)
mcse(df_binomial_samples$b)

# storage for probabilities
X <- matrix(0, nrow = nrow(df_binomial_samples), ncol = precision)

# calculate probabilities from samples
for (i in 1:nrow(df_binomial_samples)) {
  # extract values
  a <- df_binomial_samples[i,]$a
  b <- df_binomial_samples[i,]$b

  # calculate probability
  X[i,] <- invlogit(a + b * x)
}

# data frame for storing probabilities
df_binomial <- data.frame(x=numeric(), p=numeric(), q5=numeric(), q95=numeric())

# calculate mean and quantiles for each distance
for (i in 1:ncol(X)) {
  # get column
  column <- X[, i]
  
  # calcuate
  p <- mean(column)
  q5 <- quantile(column, 0.05)
  q95 <- quantile(column, 0.95)
  
  # bind
  df_binomial <- df_binomial %>%
    add_row(x=x[i], p=p, q5=q5, q95=q95)
}

# visualize data and results
ggplot(df_binomial,
       aes(x=x, y=p, ymin=q5, ymax=q95)) +
  geom_errorbar(df_golf,
                mapping=aes(x=x, y=p, ymin=q5, ymax=q95),
                color="grey75") +
  geom_point(df_golf,
             mapping=aes(x=x, y=p),
             color="grey25") +
  geom_ribbon(fill="#a6cee3", alpha=0.5) +
  geom_line(color="#a6cee3") +
  theme_minimal() +
  ylim(0, 1)


# modelling error in the angle -------------------------------------------------
# compile the model
model <- cmdstan_model("../models/golf_angle.stan")

# fit
fit_angle <- model$sample(
  data = stan_data,
  seed = 1
)

# traceplot
mcmc_trace(fit_angle$draws("sigma"))

# summary
fit_angle$summary()

# extract samples
df_angle_samples <- as_draws_df(fit_angle$draws())

# print key parameters
mcse(df_angle_samples$sigma)
mcse(df_angle_samples$sigma_degrees)

# extract probabilities
X <- as_draws_matrix(fit_angle$draws())
X <- X[,4:(4+nrow(data)-1)]

# data frame for storing probabilities
df_angle <- data.frame(x=numeric(), p=numeric(), q5=numeric(), q95=numeric())

# calculate mean and quantiles for each distance
for (i in 1:ncol(X)) {
  # get column
  column <- X[, i]
  
  # calcuate
  p <- mean(column)
  q5 <- quantile(column, 0.05)
  q95 <- quantile(column, 0.95)
  
  # bind
  df_angle <- df_angle %>%
    add_row(x=data$distance[i], p=p, q5=q5, q95=q95)
}

# visualize data and results
ggplot(df_angle,
       aes(x=x, y=p, ymin=q5, ymax=q95)) +
  geom_errorbar(df_golf,
                mapping=aes(x=x, y=p, ymin=q5, ymax=q95),
                color="grey75") +
  geom_point(df_golf,
             mapping=aes(x=x, y=p), 
             color="grey25") +
  geom_ribbon(df_binomial,
              mapping=aes(x=x, y=p, ymin=q5, ymax=q95),
              fill="#a6cee3",
              alpha=0.5) +
  geom_line(df_binomial,
            mapping=aes(x=x, y=p),
            color="#a6cee3") +
  geom_ribbon(fill="#1f78b4", alpha=0.5) +
  geom_line(color="#1f78b4") +
  theme_minimal() +
  ylim(0, 1)


# modelling error in the angle and in the distance -----------------------------
# compile the model
model <- cmdstan_model("../models/golf_angle_distance.stan")

# fit
fit_distance <- model$sample(
  data = stan_data,
  seed = 1
)

# traceplot
mcmc_trace(fit_distance$draws(c("sigma_a", "sigma_d")))

# summary
fit_distance$summary()

# extract samples
df_distance_samples <- as_draws_df(fit_distance$draws())

# print key parameters
mcse(df_distance_samples$sigma_a)
mcse(df_distance_samples$sigma_a_degrees)
mcse(df_distance_samples$sigma_d)

# extract probabilities
X <- as_draws_matrix(fit_distance$draws())
X <- X[,5:(5+nrow(data)-1)]

# data frame for storing probabilities
df_distance <- data.frame(x=numeric(), p=numeric(), q5=numeric(), q95=numeric())

# calculate mean and quantiles for each distance
for (i in 1:ncol(X)) {
  # get column
  column <- X[, i]
  
  # calcuate
  p <- mean(column)
  q5 <- quantile(column, 0.05)
  q95 <- quantile(column, 0.95)
  
  # bind
  df_distance <- df_distance %>%
    add_row(x=data$distance[i], p=p, q5=q5, q95=q95)
}
# visualize data and results
ggplot(df_distance,
       aes(x=x, y=p, ymin=q5, ymax=q95)) +
  geom_errorbar(df_golf,
                mapping=aes(x=x, y=p, ymin=q5, ymax=q95),
                color="grey75") +
  geom_point(df_golf,
             mapping=aes(x=x, y=p), 
             color="grey25") +
  geom_ribbon(df_binomial,
              mapping=aes(x=x, y=p, ymin=q5, ymax=q95),
              fill="#a6cee3",
              alpha=0.5) +
  geom_line(df_binomial,
            mapping=aes(x=x, y=p),
            color="#a6cee3") +
  geom_ribbon(df_angle,
              mapping=aes(x=x, y=p, ymin=q5, ymax=q95),
              fill="#1f78b4",
              alpha=0.5) +
  geom_line(df_angle,
            mapping=aes(x=x, y=p),
            color="#1f78b4") +
  geom_ribbon(fill="#b2df8a", alpha=0.5) +
  geom_line(color="#b2df8a") +
  theme_minimal() +
  ylim(0, 1)
