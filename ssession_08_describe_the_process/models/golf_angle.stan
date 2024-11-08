data {
  int N;          // number of data points
  array[N] int n; // number of putts at each data point
  vector[N] x;    // distance for each data point
  array[N] int y; // number of successful hits at each data point
}

transformed data {
  // golf ball radius 2.135 cm
  real r = 2.135 / 100;
  // golf hole radius
  real R = 5.398 / 100;
  
  // trehshold angle
  vector[N] threshold_angle = atan((R-r) ./ x);
}

parameters {
  real<lower=0> sigma;
}

model {
  // probabilities
  vector[N] p = 2*Phi(threshold_angle / sigma) - 1;
  
  // prior
  sigma ~ cauchy(0, 2.5);
  
  y ~ binomial(n, p);
}

generated quantities {
  // rad to deg converter
  real sigma_degrees = sigma * 180 / pi();
  
  // recalculate and return probabilities
  vector[N] p = 2*Phi(threshold_angle / sigma) - 1;
}
