data {
  int<lower=1> n; // number of observations
  vector[n] y;    // time-series
  int<lower=1> q; // the q parameter
}

parameters {
  real mu;                            // mean
  vector<lower=-1, upper=1>[q] theta; // error coeffifcients
  real<lower=0> sigma;                // error scale
}

transformed parameters {
  // error term at time t
  vector[n] epsilon;    
  
  // calculate
  for (t in 1:n) {
    epsilon[t] = y[t] - mu;
    for (i in 1:min(t - 1, q)) {
      epsilon[t] = epsilon[t] - theta[i] * epsilon[t - i];
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
    for (i in 1:min(t - 1, q)) {
      eta[t] = eta[t] + theta[i] * epsilon[t - i];
    }
  }
  
  // likelihood
  y ~ normal(eta, sigma);
}
