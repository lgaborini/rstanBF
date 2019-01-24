context("test-dirichlet")


# Test that Dirichlet functions work
n <- 100000
alpha_target_1 <- c(1, 2, 3, 4, 5)
alpha_target_2 <- c(0.1, 0.2, 3, 4, 0.5)
alpha_target_3 <- c(0.1, 0.1, 0.1, 0.1, 0.1)
df_1 <- rsamplestudy::fun_rdirichlet(n, alpha_target_1)
df_2 <- rsamplestudy::fun_rdirichlet(n, alpha_target_2)
df_3 <- rsamplestudy::fun_rdirichlet(n, alpha_target_3)
p <- length(alpha_target_1)

# Bind the dataframes
df_1_s <- df_1
df_2_s <- df_2
df_3_s <- df_3
df_1_s$source <- 1
df_2_s$source <- 2
df_3_s$source <- 3
df <- dplyr::bind_rows(df_1_s, df_2_s, df_3_s)


# Single source -----------------------------------------------------------

expect_equal_tol <- function(...) expect_equal(..., tolerance = 0.05, scale = NULL)

test_that("Single source: ML converges", {
   expect_equal_tol(as.numeric(fun_estimate_Dirichlet_from_single_source(df_1, use = 'ML', eps = 1e-14)), as.numeric(alpha_target_1))
   expect_equal_tol(as.numeric(fun_estimate_Dirichlet_from_single_source(df_2, use = 'ML', eps = 1e-14)), as.numeric(alpha_target_2))
   expect_equal_tol(as.numeric(fun_estimate_Dirichlet_from_single_source(df_3, use = 'ML', eps = 1e-14)), as.numeric(alpha_target_3))
})

test_that("Single source: naive converges", {
   expect_equal_tol(as.numeric(fun_estimate_Dirichlet_from_single_source(df_1, use = 'naive')), as.numeric(alpha_target_1))
   expect_equal_tol(as.numeric(fun_estimate_Dirichlet_from_single_source(df_2, use = 'naive')), as.numeric(alpha_target_2))
   expect_equal_tol(as.numeric(fun_estimate_Dirichlet_from_single_source(df_3, use = 'naive')), as.numeric(alpha_target_3))
})

test_that('Single source: wrong parameters', {
   expect_error(fun_estimate_Dirichlet_from_single_source(df_1, use = 'ZZZZ'))
   expect_error(fun_estimate_Dirichlet_from_single_source(df_1, use = '3141'))
})

# Multiple sources --------------------------------------------------------

df_item <- df %>% dplyr::rename(item = source)

test_that('Multiple sources: standard', {
          expect_silent(fun_estimate_Dirichlet_from_samples(df, use = 'ML'))
          expect_equal(nrow(fun_estimate_Dirichlet_from_samples(df, use = 'ML')), 3)
          expect_silent(fun_estimate_Dirichlet_from_samples(df_item, use = 'ML', col_source = 'item'))
})

test_that('Multiple sources: source column checks', {
          expect_silent(fun_estimate_Dirichlet_from_samples(df_item, use = 'ML', col_source = 'item'))
          expect_error(fun_estimate_Dirichlet_from_samples(df_item, use = 'ML', col_source = 'AAAA'))
          expect_error(fun_estimate_Dirichlet_from_samples(df_item, use = 'ML'))

          expect_silent(fun_estimate_Dirichlet_from_samples(df, use = 'ML', col_source = 'source'))
          expect_error(fun_estimate_Dirichlet_from_samples(df, use = 'ML', col_source = item))
          expect_error(fun_estimate_Dirichlet_from_samples(df, use = 'ML', col_source = 'item'))

          expect_identical(
            fun_estimate_Dirichlet_from_samples(df, use = 'ML', col_source = 'source') %>% dplyr::select(-source),
            fun_estimate_Dirichlet_from_samples(df_item, use = 'ML', col_source = 'item') %>% dplyr::select(-item)
       )
})


# Multiple sources: source estimates --------------------------------------

test_that('Multiple sources: source estimates are correct', {
   df_1_single <- fun_estimate_Dirichlet_from_single_source(df_1, use = 'ML')
   df_1_multiple <- fun_estimate_Dirichlet_from_samples(df, use = 'ML') %>% filter(source == 1) %>% select(-source)

   expect_identical(df_1_single, df_1_multiple)
})



# Hyperparameter ----------------------------------------------------------

test_that('Multiple sources: DirDir hyperparameter ML estimation does not fail', {
   res <- expect_silent(fun_estimate_Dirichlet_hyperparameter(df, method = 'ML', col_source = 'source'))
   expect_length(res, p)
   expect_is(res, 'numeric')
})
