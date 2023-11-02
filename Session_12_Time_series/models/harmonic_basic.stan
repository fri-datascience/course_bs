data {
  int<lower=0> n; // number of observations
  vector[n] t;    // time stamps
  vector[n] y;    // time-series
  real omega;     // seasonality frequency
}

parameters {
  // seasonality parameters
  real beta_cos;
  real beta_sin;
  
  // trend parameters
  real alpha;
  real beta;
  
  // variance
  real<lower=0> sigma;
}

model {
  real ssn;
  
  for (i in 1:n) {
    // seasonality
    ssn = beta_cos * cos(omega * t[i]) + beta_sin * sin(omega * t[i]);
    
    // regression model
    y[i] ~ normal(alpha + beta * t[i] + ssn, sigma);
  }
}
