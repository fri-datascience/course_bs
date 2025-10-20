data {
  int<lower=1> n;               // number of data points
  vector<lower=0>[n] y;         // data points
  vector<lower=1,upper=2>[n] g; // group identifier
}

parameters {
  real<lower=50,upper=300> mu1;  // mean for the first group
  real<lower=50,upper=300> mu2;  // mean for the second group
  real<lower=0,upper=100> sigma; // stdev
}

model {
  // priors, here they are redundant
  // Stan's default prior is flat/uniform
  // because of parameter constraints we already have such a prior here
  mu1 ~ uniform(50, 300);
  mu2 ~ uniform(50, 300);
  sigma ~ uniform(0, 100);

  // model
  for (i in 1:n) {
    if (g[i] == 1)
      y[i] ~ normal(mu1, sigma);
    else
      y[i] ~ normal(mu2, sigma);
  }
}

generated quantities {
  // calculate difference
  real diff;
  diff = mu2 - mu1;
}
