data {
  int<lower=1> n;             // total number of observation
  int<lower=0> m;             // number of independent variables
  int<lower=0> m_max;         // max number of independent variables
  matrix[n, m_max+1] X_train; // train set independent variables
  vector[n] y_train;          // train set observations
  matrix[n, m_max+1] X_test;  // test set independent variables
  vector[n] y_test;           // test set observations 
}

transformed data {
  int mm = m + 1;
}

parameters {
  vector[mm] b;        // betas
  real<lower=0> sigma; // stdev
}

model {
  for (i in 1:n) {
    // storage for linear terms
    vector[mm] mu;

    // calculate terms
    for (j in 1:mm) {
      mu[j] = X_train[i,j] * b[j];
    }

    // model
    y_train[i] ~ normal(sum(mu), sigma);
  }
}

generated quantities {
  // log-likelihood
  vector[n] log_lik_train;
  vector[n] log_lik_test; 

  for (i in 1:n) {
    // mu
    vector[mm] mu_train;
    vector[mm] mu_test;

    // calculate terms
    for (j in 1:mm) {
      mu_train[j] = X_train[i,j] * b[j];
      mu_test[j] = X_test[i,j] * b[j];
    }

    log_lik_train[i] = normal_lpdf(y_train[i] | sum(mu_train), sigma);
    log_lik_test[i] = normal_lpdf(y_test[i] | sum(mu_test), sigma);
  }
}
