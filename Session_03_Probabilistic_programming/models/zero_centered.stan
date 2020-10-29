functions {
  real zero_centered_lpdf(real x, real sigma) {
    real result = 0.0;
    result = 1 / (sigma * sqrt(2 * pi()));
    result = result * (e() ^ (-1.0/2.0 * ((x / sigma)^2)));
    return log(result);
  }
}

data {
  int<lower=0> n; // total number of data points
  vector[n] y;    // data points
}

parameters {
  real<lower=0> sigma; // stdev
}

model {
  // custom lpdf
  for (i in 1:n) {
    target += zero_centered_lpdf(y[i] | sigma);
    //y[i] ~ custom(sigma);
  }
}
