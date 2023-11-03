data {
  int<lower=1> n; // total number of data points
  vector[n] x;    // x values
  vector[n] y;    // y values
}

parameters {
  real a;              // alpha
  real b;              // beta
  real<lower=0> sigma; // stdev
}

model {
  // model
  y ~ normal(a + x * b, sigma);
}
