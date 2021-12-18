data {
  int<lower=1> n; // number of observations
  vector[n] y;    // time-series
  int<lower=1> K; // the K parameter
} 

parameters {
  real alpha;
  vector[K] beta;
  real sigma;
}

model {
  for (t in (K+1):n) {
    
    // the autoregressive part
    real mu = alpha;
    for (i in 1:K) {
      mu += beta[i] * y[t-i];
    }
    
    // likelihood
    y[t] ~ normal(mu, sigma);
  }
}
