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
  
  // distance parameters
  real o = 0.25;  // overshot
  real d_t = 0.75; // distance tolerance
}

parameters {
  real<lower=0> sigma_a;
  real<lower=0> sigma_d;
}

model {
  // angular probabilities
  vector[N] p_angle = 2*Phi(threshold_angle / sigma_a) - 1;

  // distance probabilities
  vector[N] p_distance = Phi((d_t - o) ./ ((x + o)*sigma_d)) -
               Phi((- o) ./ ((x + o)*sigma_d));
               
  // probabilities
  vector[N] p = p_angle .* p_distance;
  
  // priors
  sigma_a ~ cauchy(0, 2.5);
  sigma_d ~ cauchy(0, 2.5);
  
  y ~ binomial(n, p);
}

generated quantities {
    // rad to deg converter
  real sigma_a_degrees = sigma_a * 180 / pi();
  
  // recalculate and return probabilities
  vector[N] p = (2*Phi(threshold_angle / sigma_a) - 1) .*
                (Phi((d_t - o) ./ ((x + o)*sigma_d)) -
                Phi((- o) ./ ((x + o)*sigma_d)));
}
