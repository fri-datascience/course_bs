data {
  int<lower=0> n; // number of observations
  vector[n] y;    // observations
}

parameters {
  real<lower=0> sigma;
  real mu;
}

model {
  // normal model
  y ~ normal(mu, sigma);
}
