// Normal-Normal 1D model
//
// For benchmarking.
//

data {
   int<lower=1> n_ref;    // number of observations
   int<lower=1> n_quest;  // number of observations

   real d_ref[n_ref];     // observations
   real d_quest[n_quest]; // observations

   real mu_mu0;           // Normal hyperparameter: mean of mean
   real mu_sigma0;        // Normal hyperparameter: standard deviation of mean
   real<lower=0> sigma_mu0;        // Normal hyperparameter: mean of standard deviation
   real<lower=0> sigma_sigma0;     // Normal hyperparameter: standard deviation of standard deviation
}
parameters {
   real mu_ref;           // Normal prior: mean
   real<lower=0> sigma_ref;        // Normal prior: standard deviation
   real mu_quest;         // Normal prior: mean
   real<lower=0> sigma_quest;      // Normal prior: standard deviation
}
model {
   for (i in 1:n_ref) {
      target += normal_lpdf(d_ref[i] | mu_ref, sigma_ref);
   }
   for (i in 1:n_quest) {
      target += normal_lpdf(d_quest[i] | mu_quest, sigma_quest);
   }
   target += normal_lpdf(mu_ref | mu_mu0, mu_sigma0);
   target += normal_lpdf(sigma_ref | sigma_mu0, sigma_sigma0) + normal_lcdf(0 | sigma_mu0, sigma_sigma0);
   target += normal_lpdf(mu_quest | mu_mu0, mu_sigma0);
   target += normal_lpdf(sigma_quest | sigma_mu0, sigma_sigma0) + normal_lcdf(0 | sigma_mu0, sigma_sigma0);
}
generated quantities {
   // Prior predictive distribution
   real sim_mu_ref;
   real<lower=0> sim_sigma_ref;
   real sim_mu_quest;
   real<lower=0> sim_sigma_quest;
   real sim_d_ref;
   real sim_d_quest;

   sim_mu_ref = normal_rng(mu_mu0, mu_sigma0);
   sim_sigma_ref = normal_rng(sigma_mu0, sigma_sigma0);
   sim_mu_quest = normal_rng(mu_mu0, mu_sigma0);
   sim_sigma_quest = normal_rng(sigma_mu0, sigma_sigma0);

   sim_d_ref = normal_rng(sim_mu_ref, fabs(sim_sigma_ref));
   sim_d_quest = normal_rng(sim_mu_quest, fabs(sim_sigma_quest));

   // Posterior predictive distribution
}
