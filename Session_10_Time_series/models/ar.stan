data {
  int<lower=1> n; // number of observations
  vector[n] y;    // time-series
  int<lower=1> p; // the p parameter
} 

parameters {
  real alpha;
  vector[p] beta;
  real sigma;
}

model {
  for (t in (p+1):n) {
    
    // the autoregressive part
    real mu = alpha;
    for (i in 1:p) {
      mu += beta[i] * y[t-i];
    }
    
    // likelihood
    y[t] ~ normal(mu, sigma);
  }
}
