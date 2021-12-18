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
model <- cmdstan_model("../models/ma.stan")

# prep data for stan
Q = 6
stan_data <- list(y = df$spending, 
                  n = nrow(df),
                  Q = Q)

# fit
fit <- model$sample(
  data = stan_data,
  parallel_chains = 4,
  seed = 1,
  adapt_delta = 0.99
)

# diagnostics
mcmc_trace(fit$draws(c("mu", "sigma", "theta")))
fit$summary()

# samples
df_s <- as_draws_df(fit$draws(c("mu", "sigma", "theta", "epsilon")))
df_s <- df_s %>% select(-.draw, -.chain, -.iteration)


# plot fit ---------------------------------------------------------------------
# get a subsample of 20 random samples
df_ss <- df_s[sample(1:nrow(df_s), 20, rep = F), ]

df_plot <- data.frame(idx = character(),
                      Month = integer(),
                      S = numeric())

for (i in 1:nrow(df_ss)) {
  # mu, thetas and epsilons
  mu <- df_ss[i, ]$mu
  thetas <- df_ss[i,3:(3+Q-1)]
  epsilons <- df_ss[i,(3+Q):ncol(df_ss)]
  
  # init spending
  s <- df$spending
  
  for (j in (Q+1):nrow(df)) {
    s[j] <- mu + sum(thetas * epsilons[(j-Q):(j-1)])
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
