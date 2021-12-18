data {
  int<lower=1> n; // num observations
  vector[n] y;    // observed outputs
}

parameters {
  real mu;                       // mean coefficient
  real beta;                     // ar coefficient
  real<lower=-1, upper=1> theta; // ma coefficient
  real<lower=0> sigma;           // noise
}

transformed parameters {
  // prediction for time t
  vector[n] nu; 
  // error for time t
  vector[n] err;
  
  // arma(1, 1)
  nu[1] = mu + beta * mu;     
  err[1] = y[1] - nu[1];
  for (t in 2:n) {
    nu[t] = mu + beta * y[t - 1] + theta * err[t - 1];
    err[t] = y[t] - nu[t];
  }
}

model {
  // priors
  mu ~ normal(0, 10);
  beta ~ normal(0, 2);
  theta ~ normal(0, 2);
  sigma ~ cauchy(0, 5);
  
  // likelihood
  err ~ normal(0, sigma);
}
