# libraries --------------------------------------------------------------------
library(cmdstanr)
library(ggplot2)
library(bayesplot)
library(posterior)
library(tidyverse)
library(HDInterval)
library(cowplot)


# data prep and model compilation ----------------------------------------------
# load data
data <- read.csv("../data/piglets.csv")

# model
model_n <- cmdstan_model("../models/normal.stan")
model_g <- cmdstan_model("../models/groups_normal.stan")
model_h <- cmdstan_model("../models/hierarhical_normal.stan")


# fit --------------------------------------------------------------------------
# normal model
stan_data <- list(n=nrow(data),
                  y=data$piglet_weight)

fit_n <- model_n$sample(
  data = stan_data,
  parallel_chains = 4,
  seed = 1
)

# group model
stan_data <- list(n=nrow(data),
                  m=max(data$mama_pig),
                  y=data$piglet_weight,
                  g=data$mama_pig)

fit_g <- model_g$sample(
  data = stan_data,
  parallel_chains = 4,
  seed = 1
)

# hierarchical model
stan_data <- list(n=nrow(data),
                  m=max(data$mama_pig),
                  y=data$piglet_weight,
                  g=data$mama_pig)

fit_h <- model_h$sample(
  data = stan_data,
  parallel_chains = 4,
  seed = 1
)


# diagnostics ------------------------------------------------------------------
mcmc_trace(fit_n$draws())
mcmc_trace(fit_g$draws())
mcmc_trace(fit_h$draws())

# summary of top level parameters
fit_n$summary()
fit_g$summary()
fit_h$summary()


# extract samples --------------------------------------------------------------
df_n <- as_draws_df(fit_n$draws(c("sigma", "mu")))
df_n <- df_n %>% select(-.draw, -.chain, -.iteration)
df_g <- as_draws_df(fit_g$draws(c("sigma", "mu")))
df_g <- df_g %>% select(-.draw, -.chain, -.iteration)
df_h <- as_draws_df(fit_h$draws(c("sigma", "mu", "mu_mu", "sigma_mu")))
df_h <- df_h %>% select(-.draw, -.chain, -.iteration)


# visual posterior check configuration -----------------------------------------
# generate n_dist distributions
n_dist <- 20

# we will plot weights and distributions from 0 to 6
x <- seq(0, 6, length.out=1000)

# number of mama pigs
n_mamas <- max(data$mama_pig)


# traditional normal model visual posterior check ------------------------------
# use only a subsample
df_sample_n <- sample_n(df_n, n_dist)

# data frame for storing generated data
df_generated_n <- data.frame(x=numeric(), y=numeric(), iteration=numeric())
for (i in 1:n_dist) {
  y <- dnorm(x,
             mean=df_sample_n$mu[i],
             sd=df_sample_n$sigma[i])
  
  # bind
  df_generated_n <- rbind(df_generated_n,
                          data.frame(x=x, y=y, iteration=i))
}

# plot
ggplot() +
  geom_density(data=data, aes(x=piglet_weight),
               fill="skyblue", alpha=0.75, color=NA) +
  geom_line(data=df_generated_n,
            aes(x=x, y=y, group=iteration), alpha=0.2, size=1) +
  theme_minimal() +
  xlab("Weight") +
  ylab("Density")


# group normal model visual posterior check ------------------------------------
# use only n_dist distributions
df_sample_g <- sample_n(df_g, n_dist)

# prep for plotting
df_generated_g <- data.frame(x=numeric(),
                             y=factor(),
                             iteration=numeric(),
                             mama_pig=numeric())

for (i in 1:n_mamas) {
  for (j in 1:n_dist) {
    # mu for piglet i is in column i+1
    # sigma is always in the first column
    y <- dnorm(x,
               mean=df_sample_g[j,i+1][[1]],
               sd=df_sample_g[j,]$sigma)
    
    df_generated_g <- rbind(df_generated_g,
                            data.frame(x=x, y=y, iteration=j, mama_pig=i))
  }
}

# plot
ggplot() +
  geom_density(data=data, aes(x=piglet_weight),
               fill="skyblue", alpha=0.75, color=NA) +
  geom_line(data=df_generated_g,
            aes(x=x, y=y, group=iteration), alpha=0.1, size=1) +
  facet_wrap(. ~ mama_pig, ncol=4) +
  xlim(0, 6) +
  xlab("Weight") +
  ylab("Density")
  

# hierarhical normal model visual posterior check ------------------------------
# use only n_dist distributions
df_sample_h <- sample_n(df_h, n_dist)

# prep for plotting
df_generated_h <- data.frame(x=numeric(),
                             y=factor(),
                             iteration=numeric(),
                             mama_pig=numeric())

for (i in 1:n_mamas) {
  for (j in 1:n_dist) {
    # mu for piglet i is in column i+1
    # sigma is always in the first column
    y <- dnorm(x,
               mean=df_sample_h[j,i+1][[1]],
               sd=df_sample_h[j,]$sigma)
    
    df_generated_h <- rbind(df_generated_h,
                            data.frame(x=x, y=y, iteration=j, mama_pig=i))
  }
}

# plot
ggplot() +
  geom_density(data=data, aes(x=piglet_weight),
               fill="skyblue", alpha=0.75, color=NA) +
  geom_line(data=df_generated_h,
            aes(x=x, y=y, group=iteration), alpha=0.1, size=1) +
  facet_wrap(. ~ mama_pig, ncol=4) +
  xlim(0, 6) +
  xlab("Weight") +
  ylab("Density")


# compare top level means ------------------------------------------------------
df_top <- data.frame(Mean=numeric(),
                     Q5=numeric(),
                     Q95=numeric(),
                     Model=character())

# sample
sample_mean <- mean(data$piglet_weight)
df_top <- rbind(df_top, data.frame(Mean=sample_mean,
                                   Q5=sample_mean,
                                   Q95=sample_mean,
                                   Model="Sample"))

# normal model
normal_mean <- mean(df_n$mu)
normal_90HDI <- hdi(df_n$mu, credMass=0.9) 
df_top <- rbind(df_top, data.frame(Mean=normal_mean,
                                   Q5=normal_90HDI[1],
                                   Q95=normal_90HDI[2],
                                   Model="Normal"))

# hierarchical model
hierarchical_mean <- mean(df_h$mu_mu)
hierarchical_90HDI <- hdi(df_h$mu_mu, credMass=0.9) 
df_top <- rbind(df_top, data.frame(Mean=hierarchical_mean,
                                   Q5=hierarchical_90HDI[1],
                                   Q95=hierarchical_90HDI[2],
                                   Model="Hierarhical"))


# plot
ggplot(data=df_top,
       aes(x=Model,
           y=Mean,
           ymin=Q5,
           ymax=Q95,
           colour=Model)) +
  geom_point() + 
  geom_errorbar(width=0.2) +
  scale_color_brewer(palette="Set1") +
  ylim(0, 6) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())


# compare group level means ----------------------------------------------------
df_group <- data.frame(Mean=numeric(),
                       Q5=numeric(),
                       Q95=numeric(),
                       Model=character(),
                       mama_pig=numeric())

# sample means
df_mu_sample <- data %>%
  group_by(mama_pig) %>%
  summarise(mean_weight=mean(piglet_weight))
df_group <- rbind(df_group, data.frame(Mean=df_mu_sample$mean_weight,
                                       Q5=df_mu_sample$mean_weight,
                                       Q95=df_mu_sample$mean_weight,
                                       Model="Sample",
                                       mama_pig=seq(1:n_mamas)))

# group means
df_mu_g <- df_g %>% select(2:(1+n_mamas))
g_means <- colMeans(df_mu_g)
g_hdi90 <- apply(df_mu_g, 2, hdi, credMass=0.9)
df_group <- rbind(df_group, data.frame(Mean=g_means,
                                       Q5=g_hdi90[1,],
                                       Q95=g_hdi90[2,],
                                       Model="Group",
                                       mama_pig=seq(1:n_mamas)))

# hierarchical means
df_mu_h <- df_h %>% select(2:(1+n_mamas))
h_means <- colMeans(df_mu_h)
h_hdi90 <- apply(df_mu_h, 2, hdi, credMass=0.9)
df_group <- rbind(df_group, data.frame(Mean=h_means,
                                       Q5=h_hdi90[1,],
                                       Q95=h_hdi90[2,],
                                       Model="Hierarchical",
                                       mama_pig=seq(1:n_mamas)))

# plot
ggplot(data=df_group,
       aes(x=Model,
           y=Mean,
           ymin=Q5,
           ymax=Q95,
           colour=Model)) +
  geom_hline(yintercept=mean(data$piglet_weight), color="grey75") +
  geom_point() + 
  geom_errorbar(width=0.2) +
  scale_color_brewer(palette="Set1") +
  ylim(0, 6) +
  facet_wrap(. ~ mama_pig, ncol=4) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
