data {
  int<lower=1> n; // total number of data points
  vector[n] x;    // independent variable
  vector[n] y;    // dependent variable
}

parameters {
  real a;              // intercept
  real b;              // slope
  real<lower=0> sigma; // stdev
}

model {
  // model
  y ~ normal(a + x * b, sigma);
}
