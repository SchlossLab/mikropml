default_hyperparams <- structure(list(
  param = c(
    "cost", "cost", "cost", "cost", "cost",
    "cost", "cost", "cost", "cost", "cost", "cost", "cost", "cost",
    "loss", "epsilon", "sigma", "sigma", "sigma", "sigma", "sigma",
    "sigma", "sigma", "sigma", "C", "C", "C", "C", "C", "C", "C",
    "C", "C", "maxdepth", "maxdepth", "maxdepth", "maxdepth", "maxdepth",
    "maxdepth", "nrounds", "gamma", "eta", "eta", "eta", "eta", "max_depth",
    "colsample_bytree", "min_child_weight", "subsample", "subsample",
    "subsample", "subsample", "mtry", "mtry"
  ),
  value = c(
    "1e-6",
    "1e-5", "1e-4", "1e-3", "0.0025", "0.005", "0.01", "0.05", "0.1",
    "0.25", "0.5", "1", "10", "L2_primal", "0.01", "0.00000001",
    "0.0000001", "0.000001", "0.00001", "0.0001", "0.001", "0.01",
    "0.1", "0.0000001", "0.000001", "0.00001", "0.0001", "0.001",
    "0.01", "0.1", "1", "10", "1", "2", "3", "4", "5", "6", "500",
    "0", "0.001", "0.01", "0.1", "1", "8", "0.8", "1", "0.4", "0.5",
    "0.6", "0.7", "500", "1000"
  ),
  method = c(
    "regLogistic", "regLogistic",
    "regLogistic", "regLogistic", "regLogistic", "regLogistic", "regLogistic",
    "regLogistic", "regLogistic", "regLogistic", "regLogistic", "regLogistic",
    "regLogistic", "regLogistic", "regLogistic", "svmRadial", "svmRadial",
    "svmRadial", "svmRadial", "svmRadial", "svmRadial", "svmRadial",
    "svmRadial", "svmRadial", "svmRadial", "svmRadial", "svmRadial",
    "svmRadial", "svmRadial", "svmRadial", "svmRadial", "svmRadial",
    "rpart2", "rpart2", "rpart2", "rpart2", "rpart2", "rpart2", "xgbTree",
    "xgbTree", "xgbTree", "xgbTree", "xgbTree", "xgbTree", "xgbTree",
    "xgbTree", "xgbTree", "xgbTree", "xgbTree", "xgbTree", "xgbTree", "rf", "rf"
  )
),
class = c("spec_tbl_df", "tbl_df", "tbl", "data.frame"),
row.names = c(NA, -53L),
spec = structure(list(
  cols = list(
    param = structure(list(), class = c("collector_character", "collector")),
    val = structure(list(), class = c("collector_character", "collector")),
    method = structure(list(), class = c("collector_character", "collector"))
  ),
  default = structure(list(), class = c("collector_guess", "collector")), skip = 1
),
class = "col_spec"
)
)

# tune grid tests for each method
test_that("tune grid works for regLogistic", {
  hyperparams_lst <- default_hyperparams %>%
    get_hyperparams_from_df("regLogistic")
  grid <- expand.grid(
    cost = hyperparams_lst$cost,
    epsilon = hyperparams_lst$epsilon,
    loss = hyperparams_lst$loss
  ) %>% mutate_all_types()
  expect_equal(get_tuning_grid(hyperparams_lst, "regLogistic"), grid)
})
test_that("tune grid works for svmRadial", {
  hyperparams_lst <- default_hyperparams %>%
    get_hyperparams_from_df("svmRadial")
  grid <- expand.grid(
    C = hyperparams_lst$C,
    sigma = hyperparams_lst$sigma
  ) %>% mutate_all_types()
  expect_equal(get_tuning_grid(hyperparams_lst, "svmRadial"), grid)
})
test_that("tune grid works for rpart2", {
  hyperparams_lst <- default_hyperparams %>%
    get_hyperparams_from_df("rpart2")
  grid <- expand.grid(maxdepth = hyperparams_lst$maxdepth) %>% mutate_all_types()
  expect_equal(get_tuning_grid(hyperparams_lst, "rpart2"), grid)
})
test_that("tune grid works for rf", {
  hyperparams_lst <- default_hyperparams %>%
    get_hyperparams_from_df("rf")
  grid <- expand.grid(mtry = hyperparams_lst$mtry) %>% mutate_all_types()
  expect_equal(get_tuning_grid(hyperparams_lst, "rf"), grid)
})
test_that("tune grid works for xgbTree", {
  hyperparams_lst <- default_hyperparams %>%
    get_hyperparams_from_df("xgbTree")
  grid <- expand.grid(
    colsample_bytree = hyperparams_lst$colsample_bytree,
    eta = hyperparams_lst$eta,
    gamma = hyperparams_lst$gamma,
    max_depth = hyperparams_lst$max_depth,
    min_child_weight = hyperparams_lst$min_child_weight,
    nrounds = hyperparams_lst$nrounds,
    subsample = hyperparams_lst$subsample
  ) %>% mutate_all_types()
  expect_equal(get_tuning_grid(hyperparams_lst, "xgbTree"), grid)
})

# get_hyperparams_list
test_that("get_hyperparams_list works for all models", {
  expect_equal(
    get_hyperparams_list(otu_mini, "regLogistic"),
    list(
      cost = c(1e-04, 0.001, 0.01, 0.1, 1, 10),
      epsilon = 0.01,
      loss = "L2_primal"
    )
  )
  expect_equal(
    get_hyperparams_list(otu_mini, "rf"),
    list(mtry = c(1, 2))
  )
  expect_equal(
    get_hyperparams_list(otu_small, "rf"),
    list(mtry = c(4, 8, 16))
  )
  expect_equal(
    get_hyperparams_list(data.frame(a = 1:10, b = 4:13), "rf"),
    list(mtry = 1)
  )
  expect_equal(
    get_hyperparams_list(otu_small, "rpart2"),
    list(maxdepth = c(1, 2, 4, 8, 16, 30))
  )
  expect_equal(
    get_hyperparams_list(data.frame(a = 1:10, b = 4:13), "rpart2"),
    list(maxdepth = c(1, 2, 4, 8))
  )
  expect_equal(
    get_hyperparams_list(otu_mini, "svmRadial"),
    list(
      C = c(0.001, 0.01, 0.1, 1, 10, 100),
      sigma = c(1e-06, 1e-05, 1e-04, 0.001, 0.01, 0.1)
    )
  )
  expect_equal(
    get_hyperparams_list(otu_mini, "xgbTree"),
    list(
      nrounds = 100, gamma = 0, eta = c(0.001, 0.01, 0.1, 1),
      max_depth = c(1, 2, 4, 8, 16, 30), colsample_bytree = 0.8,
      min_child_weight = 1, subsample = c(0.4, 0.5, 0.6, 0.7)
    )
  )
})
test_that("get_hyperparams_list throws error for unsupported method", {
  expect_error(
    get_hyperparams_list(otu_mini, "not_a_method"),
    "method 'not_a_method' is not supported."
  )
})

# check_hyperparams
l2logit_required <- list(
  epsilon = c(0.01),
  loss = c("L2_primal")
)
l2_warning <-
  "For L2-normalized Logistic Regression, `loss` must be 'L2_primal' and `epsilon` must be `0.01`,"
wrong_loss <- list(
  epsilon = c(0.01),
  loss = c("L1_primal")
)
wrong_epsilon <- list(
  epsilon = c(0.1),
  loss = c("L2_primal")
)
test_that("check_hyperparams prints warning for l2logit", {
  expect_warning(check_hyperparams(wrong_loss, "regLogistic"), l2_warning)
  expect_warning(check_hyperparams(wrong_epsilon, "regLogistic"), l2_warning)
})
test_that("check_hyperparams returns nothing on success for l2logit", {
  expect_true(is.null(check_hyperparams(l2logit_required, "regLogistic")))
})
