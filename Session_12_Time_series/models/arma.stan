data {
  int<lower=1> n; // num observations
  vector[n] y;    // observed outputs
  int<lower=1> p; // the p AR parameter
  int<lower=1> q; // the q MA parameter 
}

parameters {
  real mu;                            // mean coeff
  vector[p] beta;                     // autoregression coeff
  vector<lower=-1, upper=1>[q] theta; // moving avg coeff
  real<lower=0> sigma;                // noise scale
}

transformed parameters {
  // prediction for time t
  vector[n] nu; 
  // error for time t
  vector[n] epsilon;
  // storages
  real ar;
  real ma;
  
  for (t in 1:n) {
    // ar
    ar = 0;
    for (j in 1:p) {
      if (t - j > 0) {
        ar = ar + beta[j] * y[t - j];
      }
    }
    
    // ma
    ma = 0;
    for (j in 1:q) {
      if (t - j > 0) {
        ma = ma + theta[j] * epsilon[t - j];
      }
    }
    
    nu[t] = mu + ar + ma;
    epsilon[t] = y[t] - nu[t];
  }
}

model {
  // priors
  mu ~ normal(0, 10);
  beta ~ normal(0, 2);
  theta ~ normal(0, 2);
  sigma ~ cauchy(0, 5);
  
  // likelihood
  epsilon ~ normal(0, sigma);
}
