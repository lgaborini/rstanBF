context("test-stanBF")

# Generate dummy data which should appear to work
n <- 20
p <- 5
alpha <- c(0.2, 0.5, 1, 0.1, 2)
mtx <- as.matrix(rsamplestudy::fun_rdirichlet(n, alpha))
idx.ref <- 1:(n/2)
idx.quest <- (n/2 + 1):n


list_data_OK <- list(mtx = mtx, idx.ref = idx.ref, idx.quest = idx.quest)
model_OK <- 'DirDir'
list_hyperpriors_OK <- list(alpha = seq(p))


# Models load correctly ----------------------------------------------------

test_that('Models are checked: missing', expect_error(compute_BF_Stan(list_data_OK, model = 'FAKEMODEL', hyperpriors = list_hyperpriors_OK)))


# Input errors ------------------------------------------------------------

test_that('Missing data is checked', {
   expect_error(compute_BF_Stan(list_data_OK[c(1,2)], 'DirDir', hyperpriors = list_hyperpriors_OK))
   expect_error(compute_BF_Stan(list_data_OK[c(2,3)], 'DirDir', hyperpriors = list_hyperpriors_OK))
   expect_error(compute_BF_Stan(list_data_OK[c(1,3)], 'DirDir', hyperpriors = list_hyperpriors_OK))
})


test_that('Intersecting samples are detected', {
   list_data <- list(mtx = mtx, idx.ref = idx.ref, idx.quest = idx.ref)
   expect_error(compute_BF_Stan(list_data, 'DirDir', hyperpriors = list_hyperpriors_OK))
})

test_that('Missing hyperparameters are checked', {
   expect_error(compute_BF_Stan(list_data_OK, 'DirDir', hyperpriors = list()))
})



# Output interface --------------------------------------------------------

fields <- c('model_name', 'stanmodel', 'stanfit', 'stanbridge', 'BF')

obj <- compute_BF_Stan(list_data_OK, model_OK, hyperpriors = list_hyperpriors_OK)

test_that('Models are checked: good', {
   skip_on_cran()
   expect_is(obj, 'stanBF')
   expect_true(is.stanBF(obj))
   expect_true(all(fields %in% names(obj)), info = 'stanBF is missing some fields!')
})


test_that('plot_posteriors works', {
   expect_true(ggplot2::is.ggplot(plot_posteriors(obj)))
})


test_that('samples works', {
   expect_is(samples(obj), 'data.frame')
})

test_that('prior_pred works', {
   expect_is(prior_pred(obj), 'data.frame')
})

test_that('posterior_pred works', {
   expect_is(prior_pred(obj), 'data.frame')
})
