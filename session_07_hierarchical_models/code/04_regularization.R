# libraries --------------------------------------------------------------------
library(ggplot2)
library(cmdstanr)
library(tidyverse)
library(posterior)
library(randomForest) # for random forest
library(glmnet) # for L2 (ridge) and L1 (lasso) regressions

# load and prepare data --------------------------------------------------------
data <- read.csv("./session_07_hierarchical_models/data/ozone.csv")

# use 500 to train the rest to test
idx <- sample(seq_len(nrow(data)), 500, rep = FALSE)

# in practice, test data should not be used
# to scale all data, but we do it to simplify
data[, -1] <- scale(data[, -1])

# train test split
dat_train <- data[idx, ]
dat_test <- data[-idx, ]

# compare different models -----------------------------------------------------
res_tr <- NULL
res_te <- NULL
options(stringsAsFactors = FALSE)

# linear regression ------------------------------------------------------------
my_lm <- lm(target ~ ., dat_train)

res_tr <- rbind(res_tr, data.frame(
  Method = "lm",
  Row = seq_len(nrow(dat_train)),
  Predicted = predict(my_lm),
  True = dat_train$target
))
res_te <- rbind(res_te, data.frame(
  Method = "lm",
  Row = seq_len(nrow(dat_test)),
  Predicted = predict(my_lm, newdata = dat_test),
  True = dat_test$target
))

# random forests ---------------------------------------------------------------
my_rf <- randomForest(target ~ ., dat_train, ntree = 1000)

res_tr <- rbind(res_tr, data.frame(
  Method = "rf",
  Row = seq_len(nrow(dat_train)),
  Predicted = predict(my_rf),
  True = dat_train$target
))
res_te <- rbind(res_te, data.frame(
  Method = "rf",
  Row = seq_len(nrow(dat_test)),
  Predicted = predict(my_rf, newdata = dat_test),
  True = dat_test$target
))

# L2-regularized regression, cross-validated reg. parameter --------------------
cvfit <- glmnet::cv.glmnet(
  x = as.matrix(dat_train[, -1]), y = dat_train[, 1],
  alpha = 0
)

res_tr <- rbind(res_tr, data.frame(
  Method = "L2",
  Row = seq_len(nrow(dat_train)),
  Predicted = c(predict(cvfit, newx = as.matrix(dat_train[, -1]), s = "lambda.1se")),
  True = dat_train$target
))
res_te <- rbind(res_te, data.frame(
  Method = "L2",
  Row = seq_len(nrow(dat_test)),
  Predicted = c(predict(cvfit, newx = as.matrix(dat_test[, -1]), s = "lambda.1se")),
  True = dat_test$target
))
cvfit_l2 <- cvfit

# L1-regularized regression, cross-validated reg. parameter --------------------
cvfit <- glmnet::cv.glmnet(
  x = as.matrix(dat_train[, -1]), y = dat_train[, 1],
  alpha = 1
)

res_tr <- rbind(res_tr, data.frame(
  Method = "L1",
  Row = seq_len(nrow(dat_train)),
  Predicted = c(predict(cvfit, newx = as.matrix(dat_train[, -1]), s = "lambda.1se")),
  True = dat_train$target
))
res_te <- rbind(res_te, data.frame(
  Method = "L1",
  Row = seq_len(nrow(dat_test)),
  Predicted = c(predict(cvfit, newx = as.matrix(dat_test[, -1]), s = "lambda.1se")),
  True = dat_test$target
))
cvfit_l1 <- cvfit

# Bayesian lm no regularization ------------------------------------------------
model <- cmdstan_model("./session_07_hierarchical_models/models/regularization_none.stan")

y <- dat_train[, 1]
design_matrix <- dat_train[, -1]
design_matrix_test <- dat_test[, -1]
stan_data <- list(
  y = y, X = design_matrix, n = nrow(design_matrix), k = ncol(design_matrix), m = nrow(design_matrix_test),
  X_test = design_matrix_test
)

# fit
fit <- model$sample(
  data = stan_data,
  parallel_chains = 4,
  iter_warmup = 250,
  iter_sampling = 250,
  seed = 1
)

pred <- as_draws_df(fit$draws("pred"))
pred <- pred |> select(-.chain, -.iteration, -.draw)
pred_test <- as_draws_df(fit$draws("pred_test"))
pred_test <- pred_test |> select(-.chain, -.iteration, -.draw)
lm_beta <- as_draws_df(fit$draws("beta"))
lm_beta <- lm_beta |> select(-.chain, -.iteration, -.draw)

res_tr <- rbind(res_tr, data.frame(
  Method = "lmBayes",
  Row = seq_len(nrow(dat_train)),
  Predicted = colMeans(pred),
  True = dat_train$target
))
res_te <- rbind(res_te, data.frame(
  Method = "lmBayes",
  Row = seq_len(nrow(dat_test)),
  Predicted = colMeans(pred_test),
  True = dat_test$target
))

# Bayesian lm sceptic prior = 1 ------------------------------------------------
model <- cmdstan_model("./session_07_hierarchical_models/models/regularization_parameter.stan")

y <- dat_train[, 1]
design_matrix <- dat_train[, -1]
design_matrix_test <- dat_test[, -1]
stan_data <- list(
  y = y, X = design_matrix, n = nrow(design_matrix), k = ncol(design_matrix), m = nrow(design_matrix_test),
  X_test = design_matrix_test, sigma_beta = 1
)

# fit
fit <- model$sample(
  data = stan_data,
  parallel_chains = 4,
  iter_warmup = 250,
  iter_sampling = 250,
  seed = 1
)

pred <- as_draws_df(fit$draws("pred"))
pred <- pred |> select(-.chain, -.iteration, -.draw)
pred_test <- as_draws_df(fit$draws("pred_test"))
pred_test <- pred_test |> select(-.chain, -.iteration, -.draw)
lm1_beta <- as_draws_df(fit$draws("beta"))
lm1_beta <- lm1_beta |> select(-.chain, -.iteration, -.draw)

res_tr <- rbind(res_tr, data.frame(
  Method = "lmBayes[1]",
  Row = seq_len(nrow(dat_train)),
  Predicted = colMeans(pred),
  True = dat_train$target
))
res_te <- rbind(res_te, data.frame(
  Method = "lmBayes[1]",
  Row = seq_len(nrow(dat_test)),
  Predicted = colMeans(pred_test),
  True = dat_test$target
))

# Bayesian lm sceptic prior = 0.1 ----------------------------------------------
model <- cmdstan_model("./session_07_hierarchical_models/models/regularization_parameter.stan")

y <- dat_train[, 1]
design_matrix <- dat_train[, -1]
design_matrix_test <- dat_test[, -1]
stan_data <- list(
  y = y, X = design_matrix, n = nrow(design_matrix), k = ncol(design_matrix), m = nrow(design_matrix_test),
  X_test = design_matrix_test, sigma_beta = 0.1
)

# fit
fit <- model$sample(
  data = stan_data,
  parallel_chains = 4,
  iter_warmup = 250,
  iter_sampling = 250,
  seed = 1
)

pred <- as_draws_df(fit$draws("pred"))
pred <- pred |> select(-.chain, -.iteration, -.draw)
pred_test <- as_draws_df(fit$draws("pred_test"))
pred_test <- pred_test |> select(-.chain, -.iteration, -.draw)
lm01_beta <- as_draws_df(fit$draws("beta"))
lm01_beta <- lm01_beta |> select(-.chain, -.iteration, -.draw)

res_tr <- rbind(res_tr, data.frame(
  Method = "lmBayes[0.1]",
  Row = seq_len(nrow(dat_train)),
  Predicted = colMeans(pred),
  True = dat_train$target
))
res_te <- rbind(res_te, data.frame(
  Method = "lmBayes[0.1]",
  Row = seq_len(nrow(dat_test)),
  Predicted = colMeans(pred_test),
  True = dat_test$target
))

# L1 regularized Bayesian (with hyperprior) ------------------------------------
model <- cmdstan_model("./session_07_hierarchical_models/models/regularization_hyperprior_l1.stan")

y <- dat_train[, 1]
design_matrix <- dat_train[, -1]
design_matrix_test <- dat_test[, -1]
stan_data <- list(
  y = y, X = design_matrix, n = nrow(design_matrix), k = ncol(design_matrix), m = nrow(design_matrix_test),
  X_test = design_matrix_test
)

# fit
fit <- model$sample(
  data = stan_data,
  parallel_chains = 4,
  iter_warmup = 250,
  iter_sampling = 250,
  seed = 1
)

pred <- as_draws_df(fit$draws("pred"))
pred <- pred |> select(-.chain, -.iteration, -.draw)
pred_test <- as_draws_df(fit$draws("pred_test"))
pred_test <- pred_test |> select(-.chain, -.iteration, -.draw)
l1_beta <- as_draws_df(fit$draws("beta"))
l1_beta <- l1_beta |> select(-.chain, -.iteration, -.draw)

res_tr <- rbind(res_tr, data.frame(
  Method = "L1Bayes",
  Row = seq_len(nrow(dat_train)),
  Predicted = colMeans(pred),
  True = dat_train$target
))
res_te <- rbind(res_te, data.frame(
  Method = "L1Bayes",
  Row = seq_len(nrow(dat_test)),
  Predicted = colMeans(pred_test),
  True = dat_test$target
))

# L2 regularized Bayesian (with hyperprior) ------------------------------------
model <- cmdstan_model("./session_07_hierarchical_models/models/regularization_hyperprior_l2.stan")

y <- dat_train[, 1]
design_matrix <- dat_train[, -1]
design_matrix_test <- dat_test[, -1]
stan_data <- list(
  y = y, X = design_matrix, n = nrow(design_matrix), k = ncol(design_matrix), m = nrow(design_matrix_test),
  X_test = design_matrix_test
)

# fit
fit <- model$sample(
  data = stan_data,
  parallel_chains = 4,
  iter_warmup = 250,
  iter_sampling = 250,
  seed = 1
)

pred <- as_draws_df(fit$draws("pred"))
pred <- pred |> select(-.chain, -.iteration, -.draw)
pred_test <- as_draws_df(fit$draws("pred_test"))
pred_test <- pred_test |> select(-.chain, -.iteration, -.draw)
l2_beta <- as_draws_df(fit$draws("beta"))
l2_beta <- l2_beta |> select(-.chain, -.iteration, -.draw)

res_tr <- rbind(res_tr, data.frame(
  Method = "L2Bayes",
  Row = seq_len(nrow(dat_train)),
  Predicted = colMeans(pred),
  True = dat_train$target
))
res_te <- rbind(res_te, data.frame(
  Method = "L2Bayes",
  Row = seq_len(nrow(dat_test)),
  Predicted = colMeans(pred_test),
  True = dat_test$target
))

# compare the quality ----------------------------------------------------------
se_lb <- function(x) {
  mean(x) - sd(x) / sqrt(length(x))
}
se_ub <- function(x) {
  mean(x) + sd(x) / sqrt(length(x))
}
tmp <- rbind(
  data.frame(res_te, Data = "test"),
  data.frame(res_tr, Data = "train")
)

# plot
ggplot(tmp, aes(x = Method, y = (Predicted - True)^2, colour = Method)) +
  facet_wrap(~Data) +
  stat_summary(fun.data = "mean_cl_boot", linewidth = 0.75)

# compare coefficients ---------------------------------------------------------
coeff_lm <- my_lm$coefficients[-1]
coeff_lm[is.na(coeff_lm)] <- 0
coeff_lmb <- colMeans(lm_beta)
coeff_lmb1 <- colMeans(lm1_beta)
coeff_lmb01 <- colMeans(lm01_beta)
coeff_l1 <- as.matrix(glmnet::coef.glmnet(cvfit_l1, s = "lambda.1se"))[-1]
coeff_l1b <- colMeans(l1_beta)
coeff_l2 <- as.matrix(glmnet::coef.glmnet(cvfit_l2, s = "lambda.1se"))[-1]
coeff_l2b <- colMeans(l2_beta)

tmp <- data.frame(
  Idx = rep(1:300, 8),
  Coefficient = c(
    (abs(coeff_lm)),
    (abs(coeff_lmb)),
    (abs(coeff_lmb1)),
    (abs(coeff_lmb01)),
    (abs(coeff_l1)),
    (abs(coeff_l1b)),
    (abs(coeff_l2)),
    (abs(coeff_l2b))
  ),
  Method = rep(
    c(
      "lm",
      "lmBayes",
      "lmBayes[1]",
      "lmBayes[0.1]",
      "L1",
      "L1Bayes",
      "L2",
      "L2Bayes"
    ),
    each = 300
  )
)

# plot
ggplot(tmp, aes(x = Idx, y = (Coefficient), group = Method, fill = Method)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Method, ncol = 2, scales = "free_y") +
  xlab("|Coefficient|") +
  ylab("Input variable index")
