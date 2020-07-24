# tune grid tests for each method
test_that("tune grid works for regLogistic", {
  hyperparams_df <- mikRopML::default_hyperparams %>%
    validate_hyperparams_df("regLogistic")
  hyperparams_lst <- hyperparams_df %>% get_hyperparams_list()
  grid <- expand.grid(
    cost = hyperparams_lst$cost,
    epsilon = hyperparams_lst$epsilon,
    loss = hyperparams_lst$loss
  )
  expect_equal(get_tuning_grid(hyperparams_df), grid)
})
test_that("tune grid works for svmRadial", {
  hyperparams_df <- mikRopML::default_hyperparams %>%
    validate_hyperparams_df("svmRadial")
  hyperparams_lst <- hyperparams_df %>% get_hyperparams_list()
  grid <- expand.grid(
    C = hyperparams_lst$C,
    sigma = hyperparams_lst$sigma
  )
  expect_equal(get_tuning_grid(hyperparams_df), grid)
})
test_that("tune grid works for rpart2", {
  hyperparams_df <- mikRopML::default_hyperparams %>%
    validate_hyperparams_df("rpart2")
  hyperparams_lst <- hyperparams_df %>% get_hyperparams_list()
  grid <- expand.grid(maxdepth = hyperparams_lst$maxdepth)
  expect_equal(get_tuning_grid(hyperparams_df), grid)
})
test_that("tune grid works for rf", {
  hyperparams_df <- mikRopML::default_hyperparams %>%
    validate_hyperparams_df("rf")
  hyperparams_lst <- hyperparams_df %>% get_hyperparams_list()
  grid <- expand.grid(mtry = hyperparams_lst$mtry)
  expect_equal(get_tuning_grid(hyperparams_df), grid)
})
test_that("tune grid works for xgbTree", {
  hyperparams_df <- mikRopML::default_hyperparams %>%
    validate_hyperparams_df("xgbTree")
  hyperparams_lst <- hyperparams_df %>% get_hyperparams_list()
  grid <- expand.grid(
    colsample_bytree = hyperparams_lst$colsample_bytree,
    eta = hyperparams_lst$eta,
    gamma = hyperparams_lst$gamma,
    max_depth = hyperparams_lst$max_depth,
    min_child_weight = hyperparams_lst$min_child_weight,
    nrounds = hyperparams_lst$nrounds,
    subsample = hyperparams_lst$subsample
  )
  expect_equal(get_tuning_grid(hyperparams_df), grid)
})

# check l2logit hyperparams
l2logit_required <- dplyr::tibble(param = c('loss', 'epsilon'),
                                  value = c('L2_primal', '0.01'))
l2_warning <- "For L2-normalized Logistic Regression, `loss`` must be 'L2_primal' and `epsilon` must be '0.01',"
test_that('check_l2logit_hyperparams prints warning', {
  wrong_loss <- dplyr::tibble(param = c('loss', 'epsilon'),
                              value = c('L1_primal', '0.01'))
  expect_warning(check_l2logit_hyperparams(wrong_loss), l2_warning)
  wrong_epsilon <- dplyr::tibble(param = c('loss', 'epsilon'),
                                 value = c('L2_primal', '1'))
  expect_warning(check_l2logit_hyperparams(wrong_epsilon), l2_warning)
})
test_that('check_l2logit_hyperparams returns nothing', {
  expect_true(is.null(check_l2logit_hyperparams(l2logit_required)))
})
