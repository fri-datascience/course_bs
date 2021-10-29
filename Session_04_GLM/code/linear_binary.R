# libraries --------------------------------------------------------------------
library(cmdstanr)
library(ggplot2)
library(bayesplot)
library(posterior)
library(tidyverse)


# modelling and data prep ------------------------------------------------------
# compile the model
model <- cmdstan_model("../models/simple_linear.stan")

# load the data
data <- read.csv("./data/weight_height_gender.csv")

# cast gender to 0..1
data$Gender <- as.factor(data$Gender)
data$GenderNumeric <- as.numeric(data$Gender) - 1

# prep the data for Stan
n <- nrow(data)
x <- data$Height
y <- data$GenderNumeric
stan_data <- list(n=n, x=x, y=y)

# fit
fit <- model$sample(
  data = stan_data,
  parallel_chains = 4,
  seed = 1
)


# diagnostics ------------------------------------------------------------------
# traceplot
mcmc_trace(fit$draws())

# summary
fit$summary()

# analysis ---------------------------------------------------------------------
# extract draws
df <- as_draws_df(fit$draws())

# visualize data points and regression lines
# plot only 100 random regression lines
df_100 <- data.frame(alpha=df$a, beta=df$b)
df_100 <- sample_n(df_100, 100)

# plot
ggplot() + 
  geom_point(data = data,
             aes(x = Height, y = GenderNumeric),
             alpha = 0.2, size = 3, shape = 16) +
  geom_abline(data = df_100,
              aes(slope = beta, intercept = alpha),
              color = "skyblue", alpha = 0.2, size = 1) +
  theme_minimal() +
  xlim(145, 195) +
  ylim(-0.5, 1.5) +
  ylab("Gender")
