# libraries --------------------------------------------------------------------
library(cmdstanr) # for interfacing Stan
library(ggplot2) # for visualizations
library(tidyverse) # for data manipulations
library(posterior) # for extracting samples
library(bayesplot) # for some quick MCMC visualizations
library(mcmcse) # for comparing samples and calculating MCSE
library(ggdist) # for visualizing distributions

# data -------------------------------------------------------------------------
# load the data (simulated with 14_sharks_simulation.R)
data <- read.csv("./session_04_glm/data/sharks.csv")

# correlations between variables
round(cor(data), 2)

# shark attacks against ice cream sales, fill color denotes the temperature
# on a diverging scale around the average temperature (25 °C), blue for colder
# and red for hotter days, days below 15 °C or above 35 °C get the end colors;
# both variables are counts, so we jitter the points and draw them with white
# outlines to keep overlapping points apart
ggplot(data, aes(x = icecream_sales, y = shark_attacks, fill = temperature)) +
  geom_point(
    shape = 21,
    size = 2,
    stroke = 0.25,
    color = "white",
    position = position_jitter(width = 0.35, height = 0.35, seed = 1)
  ) +
  scale_fill_gradient2(
    low = "#2166ac",
    mid = "grey80",
    high = "#b2182b",
    midpoint = 25,
    limits = c(15, 35),
    oob = scales::squish,
    breaks = seq(15, 35, by = 5),
    labels = c("≤ 15", "20", "25", "30", "≥ 35")
  ) +
  theme_minimal() +
  labs(
    x = "Ice cream sales",
    y = "Shark attacks",
    fill = "Temperature [°C]"
  )

ggsave("./session_04_glm/figs/sharks_data.png",
  width = 8, height = 4.5, dpi = 120, bg = "white"
)

# modelling --------------------------------------------------------------------
# compile the Poisson regression model
model <- cmdstan_model("./session_04_glm/models/poisson.stan")

# fit the Poisson regression model of shark attacks
# with the given independent variables
fit_model <- function(variables) {
  design_matrix <- data[, variables, drop = FALSE]
  stan_data <- list(
    n = nrow(data),
    m = ncol(design_matrix),
    X = design_matrix,
    y = data$shark_attacks
  )

  model$sample(
    data = stan_data,
    parallel_chains = 4,
    seed = 1
  )
}

# model I: shark attacks ~ ice cream sales
fit_1 <- fit_model("icecream_sales")

# model II: shark attacks ~ ice cream sales + temperature
fit_2 <- fit_model(c("icecream_sales", "temperature"))

# model III: shark attacks ~ ice cream sales + temperature
#                            + emergency interventions
fit_3 <- fit_model(
  c("icecream_sales", "temperature", "emergency_interventions")
)

# diagnostics ------------------------------------------------------------------
# traceplots
mcmc_trace(fit_1$draws(c("alpha", "beta")))
mcmc_trace(fit_2$draws(c("alpha", "beta")))
mcmc_trace(fit_3$draws(c("alpha", "beta")))

# summaries
fit_1$summary(c("alpha", "beta"))
fit_2$summary(c("alpha", "beta"))
fit_3$summary(c("alpha", "beta"))

# analysis ---------------------------------------------------------------------
# convert draws of beta coefficients to data frames
df_1 <- as_draws_df(fit_1$draws("beta"))
df_2 <- as_draws_df(fit_2$draws("beta"))
df_3 <- as_draws_df(fit_3$draws("beta"))

# beta coefficients, named after the independent variables
df_betas_1 <- data.frame(
  model = "Model I",
  icecream_sales = df_1$`beta[1]`
)
df_betas_2 <- data.frame(
  model = "Model II",
  icecream_sales = df_2$`beta[1]`,
  temperature = df_2$`beta[2]`
)
df_betas_3 <- data.frame(
  model = "Model III",
  icecream_sales = df_3$`beta[1]`,
  temperature = df_3$`beta[2]`,
  emergency_interventions = df_3$`beta[3]`
)

# bind together and transform to the long format,
# drop variables that are not in a particular model
df_betas <- bind_rows(df_betas_1, df_betas_2, df_betas_3) |>
  pivot_longer(-model, names_to = "variable", values_to = "value") |>
  drop_na()

# mean, MCSE, 5th and 95th percentiles of each coefficient
df_betas |>
  group_by(model, variable) |>
  summarise(
    mean = mean(value),
    mcse = mcse(value)$se,
    q5 = quantile(value, 0.05),
    q95 = quantile(value, 0.95),
    .groups = "drop"
  )

# fit to the data: root mean squared error between the observed shark attacks
# and the posterior means of lambda (expected shark attacks) for each day
rmse <- function(fit) {
  lambda <- colMeans(as_draws_matrix(fit$draws("lambda")))
  sqrt(mean((data$shark_attacks - lambda)^2))
}

rmse(fit_1)
rmse(fit_2)
rmse(fit_3)

# visualize the coefficients ---------------------------------------------------
# readable labels and order
variable_labels <- c(
  icecream_sales = "Ice cream sales",
  temperature = "Temperature",
  emergency_interventions = "Emergency interventions"
)

df_betas <- df_betas |>
  mutate(
    variable = factor(variable_labels[variable], levels = variable_labels),
    model = factor(model, levels = c("Model III", "Model II", "Model I"))
  )

# the true causal effects of independent variables on shark attacks
# (on the log scale)
df_true <- data.frame(
  variable = factor(variable_labels, levels = variable_labels),
  value = c(0, 0.1, 0)
)

# posteriors of coefficients with medians and intervals between the 5th and
# 95th percentiles, dashed lines denote the true causal effects
ggplot(df_betas, aes(x = value, y = model)) +
  stat_eye(fill = "skyblue", alpha = 0.75, .width = 0.9) +
  geom_vline(
    data = df_true,
    aes(xintercept = value),
    linetype = "dashed",
    color = "grey40"
  ) +
  facet_wrap(~variable, scales = "free_x") +
  theme_minimal() +
  theme(
    panel.spacing = unit(2, "lines"),
    plot.margin = margin(5.5, 15, 5.5, 5.5)
  ) +
  labs(x = "Coefficient value", y = "")

ggsave("./session_04_glm/figs/sharks_coefficients.png",
  width = 8, height = 4.5, dpi = 120, bg = "white"
)
