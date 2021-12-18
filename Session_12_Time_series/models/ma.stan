data {
  int<lower=1> n; // number of observations
  vector[n] y;    // time-series
  int<lower=1> Q; // the Q parameter
}

parameters {
  real mu;                            // mean
  real<lower=0> sigma;                // error scale
  vector<lower=-1, upper=1>[Q] theta; // error coeffifcients
}

transformed parameters {
  // error term at time t
  vector[n] epsilon;    
  
  // calculate
  for (t in 1:n) {
    epsilon[t] = y[t] - mu;
    for (q in 1:min(t - 1, Q)) {
      epsilon[t] = epsilon[t] - theta[q] * epsilon[t - q];
    }
  }
}

model {
  // eta
  vector[n] eta;

  // priors
  // overall mean parameter
  mu ~ cauchy(0, 2.5);
  // regression coefficients
  theta ~ cauchy(0, 2.5);
  // variance
  sigma ~ cauchy(0, 2.5);

  for (t in 1:n) {
    eta[t] = mu;
    for (q in 1:min(t - 1, Q)) {
      eta[t] = eta[t] + theta[q] * epsilon[t - q];
    }
  }
  
  // likelihood
  y ~ normal(eta, sigma);
}
