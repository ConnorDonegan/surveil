// multiple random-walks, uncorrelated
data {
  int<lower=1> TT;   // length of time series
  int<lower=1> K;   // J outcomes
  int y[K, TT]; // outcome data
  vector[TT] log_E[K];
  int population[K, TT];
  int is_poisson;
  int is_binomial;
  vector[K] prior_eta_1_location;
  vector[K] prior_eta_1_scale;  
  vector[K] prior_sigma_location;
  vector[K] prior_sigma_scale;
}

parameters {
  vector<upper=0>[TT] eta[K];      // annual risk per group
  vector<lower=0>[K] sigma; // scale per group
}

transformed parameters {
  vector[is_poisson ? TT : 0] mu[is_poisson ? K : 0];
  if (is_poisson) for (j in 1:K) mu[j] = log_E[j] + eta[j];
}

model {
  target += normal_lpdf(sigma | prior_sigma_location, prior_sigma_scale);    
  for (j in 1:K) {
    if (is_poisson) target += poisson_log_lpmf(y[j] | mu[j]);    
    if (is_binomial) target += binomial_logit_lpmf(y[j] | population[j], eta[j]);    
    target += normal_lpdf(eta[j, 1] | prior_eta_1_location[j], prior_eta_1_scale[j]);
    target += normal_lpdf(eta[j, 2:TT] | eta[j, 1:(TT-1)], sigma[j]);
  }
}

generated quantities {
  vector[TT] rate[K];
  vector[TT] log_lik[K];
  for (j in 1:K) {
    if (is_poisson) {
    rate[j] = exp( eta[j] );
    for (t in 1:TT) log_lik[j, t] = poisson_log_lpmf(y[j, t] | mu[j, t]);    
    }
    if (is_binomial) {
    rate[j] = inv_logit( eta[j] );
    for (t in 1:TT) log_lik[j, t] = binomial_logit_lpmf(y[j, t] | population[j, t], eta[j, t]); 
    }
  }
}

