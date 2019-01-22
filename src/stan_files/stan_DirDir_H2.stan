data {
   int<lower=1> n_ref;          // number of observations
   int<lower=1> n_quest;        // number of observations

   int<lower=1> p;              // number of turns

   simplex[p] d_ref[n_ref];     // observations (Dirichlet likelihood)
   simplex[p] d_quest[n_quest]; // observations (Dirichlet likelihood)
   vector<lower=0>[p] alpha;    // Dirichlet hyperparameter
}
parameters {
   simplex[p] theta_ref;        // Dirichlet prior
   simplex[p] theta_quest;      // Dirichlet prior
}
model {
   for (i in 1:n_ref) {
      target += dirichlet_lpdf(d_ref[i] | theta_ref);
   }
   for (i in 1:n_quest) {
      target += dirichlet_lpdf(d_quest[i] | theta_quest);
   }
   target += dirichlet_lpdf(theta_ref | alpha);
   target += dirichlet_lpdf(theta_quest | alpha);
}
generated quantities {
   // Prior predictive distribution
   simplex[p] sim_d_ref;
   simplex[p] sim_d_quest;
   simplex[p] sim_theta_ref;
   simplex[p] sim_theta_quest;
   // Posterior predictive distribution
   simplex[p] pred_d_ref;
   simplex[p] pred_d_quest;

   // Prior predictive distribution
   sim_theta_ref = dirichlet_rng(alpha);
   sim_theta_quest = dirichlet_rng(alpha);
   sim_d_ref = dirichlet_rng(sim_theta_ref);
   sim_d_quest = dirichlet_rng(sim_theta_quest);

   // Posterior predictive distribution
   pred_d_ref = dirichlet_rng(theta_ref);
   pred_d_quest = dirichlet_rng(theta_quest);
}
