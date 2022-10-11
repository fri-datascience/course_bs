data {
  int<lower=1> n;       // total number of observation
  int<lower=0> m;       // number of independent variables
  int<lower=0> m_max;   // max number of independent variables
  matrix[n, m_max+1] X; // independent variables
  vector[n] y;          // observations
}

transformed data {
  int mm = m + 1;
}

parameters {
  vector[mm] b;    // betas
  real<lower=0> sigma; // stdev
}

model {
  for (i in 1:n) {
    // storage for linear terms
    vector[mm] mu;
    
    // calculate terms
    for (j in 1:mm) {
      mu[j] = X[i,j] * b[j];
    }
    
    // model
    y[i] ~ normal(sum(mu), sigma);
  }
}

generated quantities {
  // log-likelihood
  vector[n] log_lik;
  
  for (i in 1:n) {
    // mu
    vector[mm] mu;
     
    // calculate terms
    for (j in 1:mm) {
      mu[j] = X[i,j] * b[j];
    }
     
    log_lik[i] = normal_lpdf(y[i] | sum(mu), sigma);
  }
}
