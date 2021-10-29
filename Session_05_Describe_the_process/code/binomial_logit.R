# libraries --------------------------------------------------------------------
library(cmdstanr)  # for interfacing Stan
library(ggplot2)   # for visualizations
library(posterior) # for extracting samples
library(bayesplot) # for some quick MCMC visualizations
library(mcmcse)    # for comparing samples and calculating MCSE
library(tidyverse) # for data manipulations


# modelling and data prep ------------------------------------------------------
# compile the model
model <- cmdstan_model("../models/linear.stan")

# load the data
data <- read.csv("../data/toy.csv")

# create dummy x2
data$x2 <- data$x + rnorm(nrow(data), 0, 2)

# independent variables only
X <- data %>% select(-y)

# correlation
# plot correlation of independent variables
pairs.panels(X, 
             method = "pearson", # correlation method
             hist.col = "skyblue",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

# prepare data for stan
stan_data <- list(n = nrow(data), m = ncol(X), X = X, y = data$y)

# fit
fit <- model$sample(
  data = stan_data
)

# diagnostics ------------------------------------------------------------------
# traceplot
mcmc_trace(fit$draws())

# summary
fit$summary()

# analysis ---------------------------------------------------------------------
# lines and confidence in lines
df <- as_draws_df(fit$draws())

# number of lines
n_lines <- 100
# precision
precision <- 100
# heights sequence
x <- seq(from=0, to=max(data$x), length.out=precision)
# draw only n_lines
df_subsample <- sample_n(df, n_lines)

# create these lines
lines <- data.frame(x=numeric(), y=numeric(), line=numeric())
for (i in 1:n_lines) {
  # extract values
  b1 <- df_subsample$`b[1]`[i]
  b2 <- df_subsample$`b[2]`[i]
  alpha <- df_subsample$a[i]
  
  # calculate probability
  y <- alpha + b1 * x + b2 * x
  
  # add to lines
  temp <- data.frame(x=x, y=y, line=i)
  lines <- rbind(lines, temp)
}

# visualize data points with regression lines in the background
ggplot() + 
  geom_line(data=lines,
            aes(x=x, y=y, group=line),
            color="skyblue", alpha=0.2, size=1) +
  geom_point(data = data,
             aes(x = x, y = y),
             shape = 16) +
  theme_minimal()
