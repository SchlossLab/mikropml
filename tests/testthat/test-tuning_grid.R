# tune grid tests for each method
test_that("tune grid works for regLogistic", {
  hyperparams_lst <- mikRopML::default_hyperparams %>%
    check_hyperparams_df("regLogistic") %>%
    get_hyperparams_list()
  grid <- expand.grid(
    cost = hyperparams_lst$cost,
    epsilon = hyperparams_lst$epsilon,
    loss = hyperparams_lst$loss
  ) %>% mutate_all_types()
  expect_equal(get_tuning_grid(mikRopML::default_hyperparams, "regLogistic"), grid)
})
test_that("tune grid works for svmRadial", {
  hyperparams_lst <- mikRopML::default_hyperparams %>%
    check_hyperparams_df("svmRadial") %>%
    get_hyperparams_list()
  grid <- expand.grid(
    C = hyperparams_lst$C,
    sigma = hyperparams_lst$sigma
  ) %>% mutate_all_types()
  expect_equal(get_tuning_grid(mikRopML::default_hyperparams, "svmRadial"), grid)
})
test_that("tune grid works for rpart2", {
  hyperparams_lst <- mikRopML::default_hyperparams %>%
    check_hyperparams_df("rpart2") %>%
    get_hyperparams_list()
  grid <- expand.grid(maxdepth = hyperparams_lst$maxdepth) %>% mutate_all_types()
  expect_equal(get_tuning_grid(mikRopML::default_hyperparams, "rpart2"), grid)
})
test_that("tune grid works for rf", {
  hyperparams_lst <- mikRopML::default_hyperparams %>%
    check_hyperparams_df("rf") %>%
    get_hyperparams_list()
  grid <- expand.grid(mtry = hyperparams_lst$mtry) %>% mutate_all_types()
  expect_equal(get_tuning_grid(mikRopML::default_hyperparams, "rf"), grid)
})
test_that("tune grid works for xgbTree", {
  hyperparams_lst <- mikRopML::default_hyperparams %>%
    check_hyperparams_df("xgbTree") %>%
    get_hyperparams_list()
  grid <- expand.grid(
    colsample_bytree = hyperparams_lst$colsample_bytree,
    eta = hyperparams_lst$eta,
    gamma = hyperparams_lst$gamma,
    max_depth = hyperparams_lst$max_depth,
    min_child_weight = hyperparams_lst$min_child_weight,
    nrounds = hyperparams_lst$nrounds,
    subsample = hyperparams_lst$subsample
  ) %>% mutate_all_types()
  expect_equal(get_tuning_grid(mikRopML::default_hyperparams, "xgbTree"), grid)
})

# get_hyperparams_list
df1 <- dplyr::tibble(
  param = c("cost", "cost", "loss", "epsilon"),
  value = c(1, 0.1, "L2_primal", 0.01)
)
result <- list(
  cost = c("1", "0.1"),
  epsilon = c("0.01"),
  loss = c("L2_primal")
)
test_that("get_hyperparams_list works", {
  expect_equal(get_hyperparams_list(df1), result)
})

# check_l2logit_hyperparams
l2logit_required <- dplyr::tibble(
  param = c("loss", "epsilon"),
  value = c("L2_primal", "0.01"),
  method = c("regLogistic", "regLogistic")
)
l2_warning <-
  "For L2-normalized Logistic Regression, `loss`` must be 'L2_primal' and `epsilon` must be '0.01',"
wrong_loss <- dplyr::tibble(
  param = c("loss", "epsilon"),
  value = c("L1_primal", "0.01"),
  method = c("regLogistic", "regLogistic")
)
wrong_epsilon <- dplyr::tibble(
  param = c("loss", "epsilon"),
  value = c("L2_primal", "1"),
  method = c("regLogistic", "regLogistic")
)
test_that("check_l2logit_hyperparams prints warning", {
  expect_warning(check_l2logit_hyperparams(wrong_loss), l2_warning)
  expect_warning(check_l2logit_hyperparams(wrong_epsilon), l2_warning)
})
test_that("check_l2logit_hyperparams returns nothing on success", {
  expect_true(is.null(check_l2logit_hyperparams(l2logit_required)))
})

# check_hyperparams_df
error_msg <-
  "`hyperparameters` must be a dataframe with columns `param` and `value`"
test_that("check_hyperparams_df errors if non-dataframe given", {
  expect_error(
    check_hyperparams_df(
      list(cost = c(0.1)),
      "regLogistic"
    ),
    error_msg
  )
})
df2 <- dplyr::tibble(
  param = c("cost", "cost", "mtry"),
  value = c(1, 2, 3),
  method = c("a_method", "a_method", "rf")
)
test_that("check_hyperparams_df errors if dataframe doesn't have correct columns", {
  expect_error(
    check_hyperparams_df(
      df2 %>%
        dplyr::rename(params = param),
      "a_method"
    ),
    error_msg
  )
  expect_error(
    check_hyperparams_df(
      df2 %>%
        dplyr::rename(values = value),
      "a_method"
    ),
    error_msg
  )
  expect_error(
    check_hyperparams_df(
      df2 %>%
        dplyr::mutate(newcol = 1:3),
      "rf"
    ),
    error_msg
  )
})
test_that("check_hyperparams_df filters by method if exists in columns", {
  expect_equal(
    check_hyperparams_df(df2, "a_method"),
    dplyr::tibble(
      param = c("cost", "cost"),
      value = c(1, 2)
    )
  )
})
test_that("check_hyperparams_df calls check_l2logit_hyperparams", {
  expect_warning(
    check_hyperparams_df(wrong_loss, "regLogistic"),
    l2_warning
  )
  expect_warning(
    check_hyperparams_df(wrong_epsilon, "regLogistic"),
    l2_warning
  )
  expect_message(
    check_hyperparams_df(l2logit_required, "regLogistic"),
    "Using L2 normalization for Logistic Regression"
  )
})
