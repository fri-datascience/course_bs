# libraries --------------------------------------------------------------------
library(cmdstanr)
library(ggplot2)
library(posterior)
library(bayesplot)
library(mcmcse)

# modelling and data prep ------------------------------------------------------
# compile the model
model_std <- cmdstan_model("./session_04_glm/models/simple_linear.stan")
model_robust <- cmdstan_model("./session_04_glm/models/simple_linear_robust.stan")

# generate the data for a simple linear regression
set.seed(1)
n <- 100
x <- seq(0, 10, length.out = n)
y <- 2 * x + rnorm(n, sd = 1)

# add a couple of outliers - extremelly small values for higher x
outlier_indices <- sample(which(x > 7.5), 10)
y[outlier_indices] <- y[outlier_indices] - runif(10, min = 10, max = 20)

# data
data <- data.frame(x = x, y = y)

# prepare input for Stan
stan_data <- list(n = nrow(data), x = data$x, y = data$y)

# fit
fit_std <- model_std$sample(
  data = stan_data,
  seed = 1
)

fit_robust <- model_robust$sample(
  data = stan_data,
  seed = 1
)

# diagnostics ------------------------------------------------------------------
# traceplot
mcmc_trace(fit_std$draws())
mcmc_trace(fit_robust$draws())

# summary
fit_std$summary()
fit_robust$summary()

# analysis ---------------------------------------------------------------------
# params
df_std <- as_draws_df(fit_std$draws())
df_robust <- as_draws_df(fit_robust$draws())

# plot only 100 random regression lines
df_100_std <- sample_n(df_std, 100)
df_100_robust <- sample_n(df_robust, 100)

# visualize data points with regression lines in the background
# combine data for faceting
df_100_combined <- bind_rows(
  df_100_std %>% mutate(model = "Standard"),
  df_100_robust %>% mutate(model = "Robust")
)

ggplot() +
  geom_point(
    data = data,
    aes(x = x, y = y),
    shape = 16,
    color = "skyblue"
  ) +
  geom_abline(
    data = df_100_combined,
    aes(slope = b, intercept = a),
    alpha = 0.1,
    linewidth = 1,
    color = "skyblue"
  ) +
  facet_wrap(~model) +
  theme_minimal() +
  labs(x = "x", y = "y")
