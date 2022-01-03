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

# use acf to get the p parameter
acf(df$spending, lag.max = 50)

# set q
q = 12

# prep data for stan
stan_data <- list(y = df$spending, 
                  n = nrow(df),
                  q = q)

# fit
fit <- model$sample(
  data = stan_data,
  parallel_chains = 4,
  seed = 1,
  adapt_delta = 0.9
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

# forecast n_f months
n_f <- 6
n_t <- nrow(df)
t <- 1:(n_t + n_f)

for (i in 1:nrow(df_ss)) {
  # mu, thetas and epsilons
  mu <- df_ss[i, ]$mu
  thetas <- as.numeric(df_ss[i,3:(3+q-1)])
  epsilons <- as.numeric(df_ss[i,(3+q):ncol(df_ss)])
  
  # init spending
  s <- df$spending
  
  # model
  for (j in (q+1):n_t) {
    s[j] <- mu + sum(thetas * epsilons[(j-q):(j-1)])
  }
  
  # forecast
  for (j in (n_t+1):(n_t+n_f)) {
    # spending
    s[j] <- mu + sum(thetas * epsilons[(j-q):(j-1)])
    
    # epsilon
    epsilons[j] <- s[j] - (mu + sum(thetas * epsilons[(j-1-q):(j-2)]))
  }
  
  df_plot <- df_plot %>%
    add_row(data.frame(idx = as.character(i),
                       Month = t,
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
