context("test-hyperpriors")

testthat::skip_if_not_installed('rsamplestudy')

# Generate dummy data which should appear to work
n <- 20
m <- 4
p <- 5
alpha <- rep(1, p)
list_pop <- rsamplestudy::fun_rdirichlet_population(n, m, p, alpha = alpha)

# sample dataset: make a split using rsamplestudy
k_ref <- 10
k_quest <- 10
list_samples <- rsamplestudy::make_dataset_splits(list_pop$df_pop, k_ref, k_quest)

df_background <- list_samples$df_background

# Hyperprior estimation ---------------------------------------------------

# TODO

# Parameter checking ------------------------------------------------------



# Model checking --------------------------------------------

test_that('model is checked', {

   # GOOD
   expect_silent(stanBF_elicit_hyperpriors(df_background, model = 'DirDir', mode_hyperparameter = 'vague'))

   list_hyper <- stanBF_elicit_hyperpriors(df_background, model = 'DirDir', mode_hyperparameter = 'vague')
   expect_true(is.list(list_hyper) && 'alpha' %in% names(list_hyper))
   expect_length(list_hyper$alpha, p)

   expect_error(stanBF_elicit_hyperpriors(df_background, model = 'FAKEMODEL', mode_hyperparameter = 'vague'))
})





# Mode hyperparameter checking --------------------------------------------

test_that('mode_hyperparameter is checked', {
   expect_error(stanBF_elicit_hyperpriors(df_background, model = 'DirDir', mode_hyperparameter = 'v'))
   expect_silent(stanBF_elicit_hyperpriors(df_background, model = 'DirDir', mode_hyperparameter = 'vague'))
})

stanBF_elicit_hyperpriors(df_background, model = 'DirDir', mode_hyperparameter = 'ML')

# Vague tests for background-free models
models <- c('DirFNorm', 'DirDirGamma', 'NormNorm')

for (m in models ){
   test_that('mode_hyperparameter is checked', {
      expect_error(stanBF_elicit_hyperpriors(df_background, model = m, mode_hyperparameter = 'v'))
      expect_silent(stanBF_elicit_hyperpriors(df_background, model = m, mode_hyperparameter = 'vague'))
   })
}

