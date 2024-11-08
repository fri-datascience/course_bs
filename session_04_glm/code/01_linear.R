# libraries --------------------------------------------------------------------
library(cmdstanr)
library(ggplot2)
library(posterior)
library(bayesplot)
library(mcmcse)
library(tidyverse)
library(HDInterval)

# modelling and data prep ------------------------------------------------------
# compile the model
model <- cmdstan_model("./session_04_glm/models/simple_linear.stan")

# prepare the data
data <- read.csv("./session_04_glm/data/temperature.csv", sep = ";")

# remove month
data <- data %>% select(-month)

# mean per year
data <- data %>%
  group_by(year) %>%
  summarise(temperature = mean(temperature))

# x shift for numerical stability
min_year <- min(data$year)
x <- data$year - min_year
y <- data$temperature

# prepare input for Stan
stan_data <- list(n = nrow(data), x = x, y = y)

# fit
fit <- model$sample(
  data = stan_data,
  seed = 1
)

# diagnostics ------------------------------------------------------------------
# traceplot
mcmc_trace(fit$draws())

# summary
fit$summary()

# analysis ---------------------------------------------------------------------
# params
df <- as_draws_df(fit$draws())
mcse(df$a)
mcse(df$b)
mcse(df$b > 0)

# plot only 100 random regression lines
df_100 <- sample_n(df, 100)

# x axis
x_breaks <- seq(from = 0, to = 120, length.out = 5)
x_labels <- x_breaks + min_year

# visualize data points with regression lines in the background
ggplot() +
  geom_point(
    data = data,
    aes(x = x, y = y),
    shape = 16,
    color = "skyblue"
  ) +
  geom_abline(
    data = df_100,
    aes(slope = b, intercept = a),
    alpha = 0.,
    linewidth = 1,
    color = "skyblue"
  ) +
  theme_minimal() +
  labs(x = "Year", y = "Temperature") +
  scale_x_continuous(breaks = x_breaks, labels = x_labels)

# prediction 2100
predictions <- vector()
for (i in 1:4000) {
  predictions[i] <- df$a[i] + df$b[i] * (2100 - min_year)
}
hdi(predictions, prob = 0.9)

# year 2015
data %>% filter(year == 2015)
