# libraries --------------------------------------------------------------------
library(cmdstanr)
library(ggplot2)
library(ggdist)
library(bayesplot)
library(posterior)
library(tidyverse)
library(mcmcse)

# data wrangling ---------------------------------------------------------------
# load the data
data <- read.csv("../data/temperature.csv", sep = ";")

# remove month
data <- data %>% select(-month)

# mean per year
data <- data %>%
  group_by(year) %>%
  summarise(temperature = mean(temperature))

# scale
min_temperature <- min(data$temperature)
data <- data %>% mutate(year_s = as.vector(scale(year)),
                        temperature_0 = (temperature - min_temperature))

# train/test split
train <- data %>% filter(year < 1995)
test <- data %>% filter(year >= 1995)

# stan_data
n_train <- nrow(train)
x_train <- train$year_s
y_train <- train$temperature_0
n_test <- nrow(test)
x_test <- test$year_s
y_test <- test$temperature_0

stan_data <- list(n_train = n_train,
                  x_train = x_train,
                  y_train = y_train,
                  n_test = n_test,
                  x_test = x_test,
                  y_test = y_test)

# model ------------------------------------------------------------------------
# compile the model
model <- cmdstan_model("../models/polynomial.stan")

# polynomial modeling ----------------------------------------------------------
# max order
max_order <- 4

# year_s boundaries
min_year_s <- floor(min(data$year_s))
max_year_s <- ceiling(max(data$year_s))
years_s <- seq(from = min_year_s, to = max_year_s, length.out = 200)

# required for unscaling
mean_year <- mean(data$year)
sd_year <- sd(data$year)

# mse storage
df_mse_train <- data.frame(mse = numeric(), order = factor())
df_mse_test <- data.frame(mse = numeric(), order = factor())

# fit posterior predictive storage
df_fit <- data.frame(temperature = numeric(),
                     year = numeric(),
                     order = factor(),
                     iteration = numeric())

for (p in 0:max_order) {
  # set order
  stan_data$p <- p

  # fit
  fit <- model$sample(
    data = stan_data,
    parallel_chains = 4,
    seed = 1
  )

  # uncomment lines below for diagnostic purposes
  # traceplot
  #mcmc_trace(fit$draws(c("b", "sigma")))
  # summary
  #fit$summary(c("b", "sigma"))

  # extract mse
  df <- as_draws_df(fit$draws(c("mse_train", "mse_test", "b")))

  # remove unwanted columns
  # also cast to regular data frame to avoid some warnings later on
  df <- data.frame(df %>% select(-.chain, -.iteration, -.draw))

  # store mse
  df_mse_train <- rbind(df_mse_train,
                        data.frame(mse = df$mse_train, order = as.factor(p)))
  df_mse_test <- rbind(df_mse_test,
                       data.frame(mse = df$mse_test, order = as.factor(p)))

  # posterior predictive checking
  n <- 20
  betas <- sample_n(data.frame(df[, 3:(3 + p)]), n)

  for (i in 1:n) {
    for (j in years_s) {
      # 0 shifted temperature
      x <- betas[i, 1]
      for (k in 2:p + 1) {
        x <- c(x, j^(k - 1) * betas[i, k])
      }
      temperature_0 <- sum(x)

      # unshift
      temperature <- temperature_0 + min_temperature
      year <- (j * sd_year) + mean_year

      # store
      df_fit <- rbind(df_fit,
                      data.frame(temperature = temperature,
                                 year = year,
                                 order = as.factor(p),
                                 iteration = i))
    }
  }
}

# posterior predictive check ---------------------------------------------------
ggplot() +
  geom_point(data = data,
             aes(x = year, y = temperature),
             alpha = 0.3, size = 1, shape = 16) +
  geom_line(data = df_fit,
            aes(x = year, y = temperature, group = iteration),
            alpha = 0.3) +
  facet_grid(. ~ order) +
  xlab("Year") +
  ylab("T [°C]") +
  ylim(6, 14)

ggsave("../figs/cross_validation_posterior.png", width = 12, height = 4)

# compare ----------------------------------------------------------------------
# plot
ggplot(data = df_mse_train, aes(y = order, x = mse)) +
  stat_eye(fill = "skyblue", alpha = 0.75) +
  xlab("MSE") +
  ylab("Order of the polynomial")

ggplot(data = df_mse_test, aes(y = order, x = mse)) +
  stat_eye(fill = "skyblue", alpha = 0.75) +
  xlab("MSE") +
  ylab("Order of the polynomial") +
  coord_cartesian(xlim = c(0, 10))

# numerical info
df_mse_test %>%
  group_by(order) %>%
  summarize(mean_mse = mcse(mse)$est,
            mcse_mse = mcse(mse)$se,
            hdi5 = hdi(mse, credMass = 0.90)[1],
            hdi95 = hdi(mse, credMass = 0.90)[2])
