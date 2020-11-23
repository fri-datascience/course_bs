data {
  int<lower=1> n; // total number of data points
  vector[n] y;    // data points
}

parameters {
  real mu;             // mean
  real<lower=0> sigma; // stdev
}

model {
  // priors
  mu ~ normal(0, 10);
  sigma ~ cauchy(0, 10);
  
  // model
  //y ~ normal(mu, sigma);
  
  // or
  for (i in 1:n) {
    target += normal_lpdf(y[i] | mu, sigma);
  }
}
