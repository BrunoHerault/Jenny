data {
  int<lower=0> N;
  real pbray[N]; 
  real clay[N];
}

parameters {
  real<lower=0, upper=10> theta_1;
  real<lower=0, upper=10> theta_2;
  real<lower=0.001, upper=2> sigma;
}

model{
  real mu[N];
  for (i in 1:N)
  {
    mu[i]=theta_1*exp(-theta_2*clay[i]);
  }
  pbray ~ normal(mu, sigma);
  sigma ~ gamma(0.01, 0.01);
}
