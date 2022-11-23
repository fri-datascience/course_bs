data {
  int<lower=1> n_train;    // total number of data points
  vector[n_train] x_train; // test data predictors
  vector[n_train] y_train; // train data predictors
  int<lower=1> n_test;     // total number of data points
  vector[n_test] x_test;   // test data predictors
  vector[n_test] y_test;   // train data predictors
  int<lower=0> p;          // order of the polynomial
}

transformed data {
  // transform polynom order since b[0] is the intercept
  int p_order = p + 1;
}

parameters {
  vector[p_order] b;   // poly terms
  real<lower=0> sigma; // stdev
}

model {
  // x 
  vector[p_order] mu;
  
  // priors
  for (j in 2:p_order)  {
    b[j] ~ cauchy(0, 1);
  }
  
  // model
  for (i in 1:n_train) {
    // calculate terms
    for (j in 1:p_order) {
      mu[j] = pow(x_train[i], j - 1) * b[j];
    }
    
    // model
    y_train[i] ~ normal(sum(mu), sigma);
  }
}

generated quantities {
  real x[p_order];
  vector[n_train] y_pred_train;
  real mse_train = 0;
  vector[n_test] y_pred_test;
  real mse_test = 0;
  
  // in sample mse
  for (i in 1:n_train) {
    // calculate terms
    for (j in 1:p_order) {
      x[j] = pow(x_train[i], j - 1) * b[j];
    }
    
    // sum polynomial terms together
    y_pred_train[i] = sum(x);
    
    // mse calculation
    mse_train = mse_train + square(y_train[i] - y_pred_train[i]);
  }
  // final mse division by n
  mse_train = mse_train / n_train;
  
  // out of sample mse
  for (i in 1:n_test) {
    // calculate terms
    for (j in 1:p_order) {
      x[j] = pow(x_test[i], j - 1) * b[j];
    }
    
    // sum polynomial terms together
    y_pred_test[i] = sum(x);
    
    // mse calculation
    mse_test = mse_test + square(y_test[i] - y_pred_test[i]);
  }
  // final mse division by n
  mse_test = mse_test / n_test;
}
