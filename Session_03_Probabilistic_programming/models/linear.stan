data {
  int<lower=1> n; // total number of data points
  vector[n] x;    // x values
  vector[n] y;    // y values
}

parameters {
  real a;              // intercept
  real b;              // slope
  real<lower=0> sigma; // stdev
}

model {
  // priors
  a ~ normal(0, 20);
  b ~ normal(0, 20);
  sigma ~ cauchy(0, 20);
  
  // model
  y ~ normal(a + b * x, sigma);
}
