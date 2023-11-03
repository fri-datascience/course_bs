data {
  int<lower=1> n; // total number of data points
  int<lower=1> m; // number of predictors
  matrix[n, m] X; // independent variables
  vector[n] y;    // dependent variable
}

parameters {
  real a;              // intercept
  vector[m] b;         // slope
  real<lower=0> sigma; // stdev
}

model {
  // model
  y ~ normal(a + X * b, sigma);
}
