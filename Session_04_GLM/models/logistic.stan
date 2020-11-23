data {
  int<lower=1> n;             // number of measurements
  vector<lower=0>[n] x;       // height
  int<lower=0, upper=1> y[n]; // gender 
}

parameters {
  real alpha;
  real beta;
}

model {
  // beta priors
  beta ~ cauchy(0, 2.5);
 
  // bernoulli logit
  y ~ bernoulli_logit(alpha + beta * x);
 
  // alternatively we could use
  //y ~ bernoulli(inv_logit(alpha + beta * x));
  
  // or even
  //for (i in 1:n)
  //  y[i] ~ bernoulli(1 / (1 + exp(-(alpha + x[i] * beta))));
}
