# libraries --------------------------------------------------------------------
library(cmdstanr)
library(ggplot2)
library(bayesplot)
library(posterior)
library(tidyverse)
library(HDInterval)

# data prep and exploratory analysis -------------------------------------------
df <- read.table("../data/SO2.csv", header = TRUE, sep = ";")
df$Type <- "Raw"

# log
df_log <- data.frame(WeekNumber = df$WeekNumber,
                     Concentration = log(df$Concentration + 0.01),
                     Type = "Log")

# bind
df_merged <- rbind(df, df_log)

# plot raw vs log
ggplot(df_merged, aes(x = WeekNumber, y = Concentration)) +
  geom_line() +
  facet_wrap(~ Type, scales = "free_y", ncol = 1)

ggplot(df_merged, aes(x = Concentration)) +
  geom_histogram(stat = "bin") +
  facet_wrap(. ~ Type, scales = "free")

# decomposition with harmonic regression ---------------------------------------
model <- cmdstan_model("../models/harmonic.stan")

# seasonality frequency
omega <- array((2 * pi) / c(52.25, 52.25 / 12))

# prep data for stan
stan_data <- list(y = df_log$Concentration,
                  t = df_log$WeekNumber,
                  n = nrow(df_log),
                  omega = omega,
                  k = length(omega))

# fit - very small number of iterations for speed purposes
# increase iterations for production grade analyses
fit <- model$sample(
  data = stan_data,
  parallel_chains = 4,
  seed = 1,
  iter_warmup = 200,
  iter_sampling = 100,
  max_treedepth = 15
)

# diagnostics
mcmc_trace(fit$draws())
fit$summary()

# samples
df_s <- as_draws_df(fit$draws())
df_s <- df_s %>% select(-lp__, -.draw, -.chain, -.iteration)

# decomposition results --------------------------------------------------------
# plot 5 random samples
idx <- sample(seq_len(nrow(df_s)), 5, rep = FALSE)

# our time stamps
x <- df_log$WeekNumber

# storage data frame
df_decomposed <- data.frame(idx = character(),
                            Type = character(),
                            Week = integer(),
                            Concentration = numeric())

for (i in idx) {
  # original
  df_decomposed <- df_decomposed %>%
    add_row(data.frame(idx = as.character(i),
                       Type = "Original",
                       Week = df_log$WeekNumber,
                       Concentration = df_log$Concentration))

  # ssn
  ssn1 <- df_s$`beta_cos[1]`[i] * cos(omega[1] * x) +
          df_s$`beta_sin[1]`[i] * sin(omega[1] * x)
  ssn2 <- df_s$`beta_cos[2]`[i] * cos(omega[2] * x) +
          df_s$`beta_sin[2]`[i] * sin(omega[2] * x)
  ssn <- ssn1 + ssn2
  df_decomposed <- df_decomposed %>%
    add_row(data.frame(idx = as.character(i),
                       Type = "Seasonality",
                       Week = df_log$WeekNumber,
                       Concentration = ssn))

  # trend
  trend <- df_s$beta[i] * x + df_s$alpha[i]
  df_decomposed <- df_decomposed %>%
    add_row(data.frame(idx = as.character(i),
                       Type = "Trend",
                       Week = df_log$WeekNumber,
                       Concentration = trend))

  # reminder
  reminder <- df_log$Concentration - ssn - trend
  df_decomposed <- df_decomposed %>%
    add_row(data.frame(idx = as.character(i),
                       Type = "Reminder",
                       Week = df_log$WeekNumber,
                       Concentration = reminder))
}

# plot
ggplot(df_decomposed, aes(x = Week,
                          y = Concentration,
                          group = idx,
                          colour = idx)) +
  geom_path() +
  facet_wrap(. ~ Type, ncol = 1, scales = "free_y")
