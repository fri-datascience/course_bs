data {
  int<lower=1> n; // number of data points
  array[n] int<lower=0,upper=1> y; // result (0/1)
}

parameters {
  real<lower=0,upper=1> theta; // theta parameter
}

model {
  // uniform prior from 0 to 1
  // same as default (no prior specification) since theta is limited from 0 to 1
  theta ~ beta(1, 1);

  // the model
  y ~ bernoulli(theta);
}
