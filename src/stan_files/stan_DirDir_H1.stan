data {
   int<lower=1> n_ref;        // number of observations
   int<lower=1> p;            // number of turns
   
   simplex[p] d_ref[n_ref];   // observations (Dirichlet likelihood)
   vector<lower=0>[p] alpha;  // Dirichlet hyperparameter
}
parameters {
   simplex[p] theta_ref;      // Dirichlet prior
}
model {
   for (i in 1:n_ref) {
      target += dirichlet_lpdf(d_ref[i] | theta_ref);
   }
   target += dirichlet_lpdf(theta_ref | alpha);
}
generated quantities {
   // Prior predictive distribution
   simplex[p] sim_d_ref;
   vector<lower=0>[p] sim_theta_ref;
   
   sim_theta_ref = dirichlet_rng(alpha);
   sim_d_ref = dirichlet_rng(sim_theta_ref);


   // Posterior predictive distribution
   
}
