# libraries --------------------------------------------------------------------
library(cmdstanr)
library(ggplot2)
library(bayesplot)
library(posterior)
library(tidyverse)
library(HDInterval)


# model ------------------------------------------------------------------------
# compile the model
model <- cmdstan_model("./models/linear_deviance_cv.stan")


# modeling ---------------------------------------------------------------------
# number of observations
n <- 200

# max number of independent variables
m_max <- 6

# generate the data
# predictors
X_train <- matrix(nrow = n, ncol = m_max + 1)

# intercept
X_train[,1] <- 1

# test set
X_test <- X_train

# other terms
for (i in 2:(m_max+1)) {
  X_train[,i] <- runif(n, -1, 1)
  X_test[,i] <- runif(n, -2, 2)
}

# generate data
y_train <- rnorm(n, 1 + 1.5 * X_train[,2] - 2.5 * X_train[,3], 1)
y_test <- rnorm(n, 1 + 1.5 * X_test[,2] - 2.5 * X_test[,3], 1)

# stan_data
stan_data <- list(n=n,
                  m_max=m_max,
                  X_train=X_train,
                  X_test=X_test,
                  y_train=y_train,
                  y_test=y_test)

# log-score storage
df_s_train <- data.frame(s=numeric(), Order=factor())
df_s_test <- data.frame(s=numeric(), Order=factor())

for (m in 0:m_max) {
  # set order
  stan_data$m <- m;
  
  # fit
  fit <- model$sample(
    data = stan_data,
    parallel_chains = 4,
    iter_warmup = 500,
    iter_sampling = 500,
  seed = 1
  )
  
  # uncomment lines below for diagnostic purposes
  # traceplot
  #mcmc_trace(fit$draws(c("b", "sigma")))
  # summary
  fit$summary(c("b", "sigma"))
  
  # extract
  df_ll_train <- as_draws_df(fit$draws(c("log_lik_train")))
  df_ll_test <- as_draws_df(fit$draws(c("log_lik_test")))
  
  # remove unwanted columns
  # also cast to regular data frame to avoid some warnings later on
  df_ll_train <- data.frame(df_ll_train %>% select(-.chain, -.iteration, -.draw))
  df_ll_test <- data.frame(df_ll_test %>% select(-.chain, -.iteration, -.draw))
  
  # average per row and store
  df_s_train <- rbind(df_s_train,
                      data.frame(s=-2*rowSums(df_ll_train),
                                 Order=as.factor(m)))
  df_s_test <- rbind(df_s_test,
                     data.frame(s=-2*rowSums(df_ll_test),
                                Order=as.factor(m)))
}

# plot -------------------------------------------------------------------------
df_s_train$Type <- "In-sample"
df_s_test$Type <- "Out-of-sample"

df <- rbind(df_s_train, df_s_test)

df_summary <- df %>% group_by(Type, Order) %>%
  summarize(mean_s=mean(s),
            hdi5=hdi(s, credMass=0.9)[1],
            hdi95=hdi(s, credMass=0.9)[2])

# plot
ggplot(data=df_summary, aes(x=Order, y=mean_s)) +
  geom_point(shape=16, size=2) +
  geom_linerange(aes(ymin = hdi5, ymax = hdi95), alpha=0.3) +
  xlab("Number of predictors") +
  ylab("Deviance") +
  facet_grid(. ~ Type)
    
# difference plot
df_difference <- df_summary %>% group_by(Order) %>%
  summarize(diff=mean_s[2] - mean_s[1])
  
ggplot(data=df_difference, aes(x=Order, y=diff)) +
  geom_bar(stat="identity", fill="skyblue") +
  xlab("Number of predictors") +
  ylab("Deviance difference") +
  theme_minimal()
