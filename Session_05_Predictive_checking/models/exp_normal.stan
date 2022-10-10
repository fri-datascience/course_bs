data {
  int<lower=1> n; // total number of data points
  vector[n] y;    // dependent variable
}

parameters {
  real mu;              // mean
  real<lower=0> sigma;  // stdev
  real<lower=0> lambda; // rate
}

model {
  y ~ exp_mod_normal(mu, sigma, lambda);
}
