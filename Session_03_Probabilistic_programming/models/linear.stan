data {
  int<lower=1> n; // total number of data points
  int<lower=1> k; // number of predictors
  matrix[n, k] X; // X values
  vector[n] y;    // y values
}

parameters {
  real a;              // alpha
  vector[k] b;         // betas
  real<lower=0> sigma; // stdev
}

model {
  // beta prior
  b ~ normal(0, 1);
  
  // model
  y ~ normal(a + X * b, sigma);
}
