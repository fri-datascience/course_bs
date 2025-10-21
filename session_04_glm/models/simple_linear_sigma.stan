data {
  int<lower=1> n; // total number of data points
  vector[n] x;    // independent variable
  vector[n] y;    // dependent variable
}

parameters {
  real mu;        // mean
  real b;         // slope
}

model {
  // model
  y ~ normal(mu, x * b);
}
