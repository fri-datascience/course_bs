data {
  int<lower=0> n;           // train set size
  int<lower=0> m;           // test set size
  int<lower=0> k;           // number of independent variables
  matrix[n, k] X;           // train set
  matrix[m, k] X_test;      // test set
  vector[n] y;              // dependent variable
  real<lower=0> sigma_beta; // regularization parameter
}

parameters {
  real alpha;                // intercept
  vector[k] beta;            // beta coefficients
  real<lower=0> sigma;       // sd
}

model {
  // regularization
  beta ~ normal(0, sigma_beta);

  y ~ normal(X * beta + alpha, sigma);
}

generated quantities {
  // calculate predictions on test and train
  vector[n] pred;
  vector[m] pred_test;
  pred = X * beta + alpha;
  pred_test = X_test * beta + alpha;
}
