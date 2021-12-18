# libraries --------------------------------------------------------------------
library(cmdstanr)
library(ggplot2)
library(bayesplot)
library(posterior)
library(tidyverse)
library(HDInterval)


# data prep and exploratory analysis -------------------------------------------
df <- read.csv("../data/restaurants.csv")

# only last 10 years
df <- df %>% filter(month > (nrow(df) - 120))

# reindex months
df$month <- 1:nrow(df)


# ar ---------------------------------------------------------------------------
model <- cmdstan_model("../models/ar.stan")

# prep data for stan
K = 6
stan_data <- list(y = df$spending, 
                  n = nrow(df),
                  K = K)

# fit
fit <- model$sample(
  data = stan_data,
  parallel_chains = 4,
  seed = 1
)

# diagnostics
mcmc_trace(fit$draws())
fit$summary()

# samples
df_s <- as_draws_df(fit$draws())
df_s <- df_s %>% select(-lp__, -.draw, -.chain, -.iteration)


# plot fit ---------------------------------------------------------------------
# get a subsample of 20 random samples
df_ss <- df_s[sample(1:nrow(df_s), 20, rep = F), ]

df_plot <-data.frame(idx = character(),
                     Month = integer(),
                     S = numeric())

for (i in 1:nrow(df_ss)) {
  # alpha and betas
  alpha <- df_ss[i, ]$alpha
  betas <- df_ss[i,2:(2+K-1)]
  
  # init spending
  s <- df$spending
  
  for (j in (K+1):nrow(df)) {
    s[j] <- alpha + sum(betas * df$spending[(j-K):(j-1)])
  }
  
  df_plot <- df_plot %>%
    add_row(data.frame(idx = as.character(i),
                       Month = df$month,
                       S = s))
}

# get mean and HDI
df_plot <- df_plot %>%
  group_by(Month) %>%
  summarize(Spending=mean(S),
            hdi5=hdi(S, credMass=0.90)[1],
            hdi95=hdi(S, credMass=0.90)[2])

# plot
ggplot(data=df_plot, aes(x=Month, y=Spending), group=ix) +
  geom_line(data=df, aes(x=month, y=spending), color="skyblue") +
  geom_line() +
  geom_ribbon(aes(ymin=hdi5, ymax=hdi95), alpha=0.25) +
  theme_minimal()
