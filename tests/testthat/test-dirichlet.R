context("test-dirichlet")


# Test that Dirichlet functions work
n <- 10000
alpha_target_1 <- c(1, 2, 3, 4, 5)
alpha_target_2 <- c(0.1, 0.2, 3, 4, 0.5)
alpha_target_3 <- c(0.1, 0.1, 0.1, 0.1, 0.1)
df_1 <- rsamplestudy::fun_rdirichlet(n, alpha_target_1)
df_2 <- rsamplestudy::fun_rdirichlet(n, alpha_target_2)
df_3 <- rsamplestudy::fun_rdirichlet(n, alpha_target_3)

# Bind the dataframes
df_1_s <- df_1
df_2_s <- df_2
df_3_s <- df_3
df_1_s$source <- 1
df_2_s$source <- 2
df_3_s$source <- 3
df <- dplyr::bind_rows(df_1_s, df_2_s, df_3_s)


# Single source -----------------------------------------------------------

expect_equal_tol <- function(...) expect_equal(..., tolerance = 0.01, scale = NULL)

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
