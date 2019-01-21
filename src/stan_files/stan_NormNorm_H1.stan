// Normal-Normal 1D model
//
// For benchmarking.
//

data {
   int<lower=1> n_ref;    // number of observations

   real d_ref[n_ref];     // observations

   real mu_mu0;           // Normal hyperparameter: mean of mean
   real mu_sigma0;        // Normal hyperparameter: standard deviation of mean
   real<lower=0> sigma_mu0;        // Normal hyperparameter: mean of standard deviation
   real<lower=0> sigma_sigma0;     // Normal hyperparameter: standard deviation of standard deviation
}
parameters {
   real mu_ref;           // Normal prior: mean
   real<lower=0> sigma_ref;        // Normal prior: standard deviation
}
model {
   for (i in 1:n_ref) {
      target += normal_lpdf(d_ref[i] | mu_ref, sigma_ref);
   }
   target += normal_lpdf(mu_ref | mu_mu0, mu_sigma0);
   target += normal_lpdf(sigma_ref | sigma_mu0, sigma_sigma0) + normal_lcdf(0 | sigma_mu0, sigma_sigma0);
}
generated quantities {
   // Prior predictive distribution
   real sim_mu_ref;
   real<lower=0> sim_sigma_ref;
   real sim_d_ref;

   sim_mu_ref = normal_rng(mu_mu0, mu_sigma0);
   sim_sigma_ref = normal_rng(sigma_mu0, sigma_sigma0);
   sim_d_ref = normal_rng(sim_mu_ref, fabs(sim_sigma_ref));

   // Posterior predictive distribution
}
