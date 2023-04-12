# TODO: add tests for if impution param is set to true
library(dplyr)
options(
  warnPartialMatchArgs = FALSE,
  warnPartialMatchAttr = FALSE,
  warnPartialMatchDollar = FALSE
)
# Without this, underlying code in either stats or base R causes this warning in several places:
#   warning: get_predictions works
#   partial argument match of 'contrasts' to 'contrasts.arg'


get_all_but_model <- function(ml_results) {
  return(ml_results[names(ml_results) != "trained_model"])
}

expect_equal_ml_results <- function(result1, result2, tolerance = 1e-5) {
  return(
    eval(bquote(
      expect_equal(get_all_but_model(result1),
        get_all_but_model(result2),
        tolerance = tolerance
      )
    ))
  )
}

otu_mini_group <- c(
  "B", "F", "E", "D", "A", "F", "F", "D", "E", "B", "F", "F",
  "E", "A", "B", "A", "E", "A", "D", "A", "D", "A", "C", "A", "B",
  "B", "E", "F", "F", "A", "E", "B", "F", "C", "D", "D", "C", "A",
  "E", "E", "B", "B", "F", "A", "F", "C", "D", "D", "F", "D", "D",
  "A", "D", "F", "C", "A", "D", "D", "B", "F", "C", "F", "E", "C",
  "F", "F", "E", "C", "B", "D", "B", "D", "F", "C", "F", "B", "B",
  "B", "B", "B", "B", "B", "F", "C", "D", "D", "E", "A", "F", "E",
  "D", "E", "A", "D", "F", "A", "E", "E", "C", "B", "B", "E", "B",
  "F", "C", "F", "C", "D", "A", "F", "A", "F", "E", "E", "C", "F",
  "A", "C", "E", "A", "D", "A", "C", "D", "E", "E", "A", "A", "B",
  "F", "E", "C", "B", "B", "C", "C", "D", "C", "E", "E", "F", "F",
  "E", "C", "E", "F", "D", "D", "B", "B", "B", "E", "E", "A", "A",
  "A", "B", "D", "B", "D", "F", "F", "F", "B", "B", "B", "F", "F",
  "D", "B", "B", "D", "B", "D", "C", "C", "B", "F", "A", "F", "A",
  "F", "D", "C", "E", "E", "F", "A", "B", "B", "A", "B", "A", "B",
  "E", "A", "C", "E", "F", "A"
)

test_that("run_ml works for logistic regression", {
  expect_equal_ml_results(
    run_ml(
      otu_mini_bin,
      # use built-in hyperparameters
      "glmnet",
      outcome_colname = "dx",
      find_feature_importance = FALSE,
      seed = 2019,
      cv_times = 2
    ),
    otu_mini_bin_results_glmnet
  ) %>%
    expect_warning("`caret::train\\(\\)` issued the following warning:") %>%
    suppressMessages()
})

test_that("run_ml works for linear regression", {
  skip_on_cran()
  expect_equal_ml_results(
    run_ml(
      otu_mini_bin[, 2:11], # use built-in hyperparameters
      "glmnet",
      outcome_colname = "Otu00001",
      find_feature_importance = TRUE,
      cv_times = 2,
      seed = 2019
    ),
    otu_mini_cont_results_glmnet
  ) %>%
    expect_warning("Data is being considered numeric") %>%
    suppressWarnings() %>%
    suppressMessages()
})

test_that("run_ml works for random forest with grouping & feature importance", {
  skip_on_cran()
  expect_equal_ml_results(
    mikropml::run_ml(otu_mini_bin,
      "rf",
      outcome_colname = "dx",
      find_feature_importance = TRUE,
      seed = 2019,
      cv_times = 2,
      groups = otu_mini_group
    ),
    otu_mini_bin_results_rf,
    tolerance = 1e-3
  ) %>%
    suppressWarnings() %>%
    suppressMessages()
})

test_that("run_ml works for svmRadial", {
  skip_on_cran()
  expect_equal_ml_results(
    mikropml::run_ml(otu_mini_bin,
      "svmRadial",
      outcome_colname = "dx",
      find_feature_importance = FALSE,
      seed = 2019,
      cv_times = 2
    ),
    otu_mini_bin_results_svmRadial
  ) %>%
    expect_warning() %>%
    suppressMessages()
})

test_that("run_ml works for xgbTree", {
  skip_on_cran()
  skip_on_os(c("linux", "windows")) # bug in xgboost package: https://discuss.xgboost.ai/t/colsample-by-tree-leads-to-not-reproducible-model-across-machines-mac-os-windows/1709
  expect_equal_ml_results(
    mikropml::run_ml(
      otu_mini_bin,
      "xgbTree",
      outcome_colname = "dx",
      find_feature_importance = FALSE,
      seed = 2019,
      cv_times = 2
    ),
    otu_mini_bin_results_xgbTree,
    tolerance = 1e-3
  ) %>%
    suppressWarnings() %>%
    suppressMessages()
})

test_that("run_ml works for rpart2", {
  skip_on_cran()
  expect_equal_ml_results(
    mikropml::run_ml(otu_mini_bin,
      "rpart2",
      outcome_colname = "dx",
      find_feature_importance = FALSE,
      seed = 2019,
      cv_times = 2
    ),
    otu_mini_bin_results_rpart2
  ) %>%
    suppressMessages()
})

test_that("run_ml uses a custom cross-validation scheme", {
  skip_on_cran()
  expect_equal_ml_results(
    run_ml(otu_mini_bin[, 2:11],
      "glmnet",
      outcome_colname = "Otu00001",
      seed = 2019,
      hyperparameters = list(lambda = c(1e-04), alpha = 0),
      cross_val = caret::trainControl(method = "none"),
      calculate_performance = FALSE
    ),
    otu_mini_cont_results_nocv
  ) %>%
    expect_warning("Data is being considered numeric") %>%
    suppressWarnings() %>%
    suppressMessages()
})

test_that("run_ml errors for unsupported method", {
  run_ml(
    otu_small,
    "not_a_method"
  ) %>%
    expect_error("Method 'not_a_method' is not officially supported by mikropml")
})

test_that("run_ml errors if outcome_colname not in dataframe", {
  expect_error(
    run_ml(
      otu_small,
      "rf",
      outcome_colname = "not_a_colname"
    ),
    "Outcome 'not_a_colname' not in column names of data."
  ) %>%
    suppressMessages()
})

test_that("run_ml works for multiclass outcome", {
  skip_on_cran()
  expect_equal_ml_results(
    run_ml(otu_mini_multi,
      "glmnet",
      outcome_colname = "dx",
      find_feature_importance = TRUE,
      seed = 2019,
      cv_times = 2,
      groups = otu_mini_multi_group
    ),
    otu_mini_multi_results_glmnet
  ) %>%
    expect_message("Using 'dx' as the outcome column") %>%
    expect_warning("`caret::train\\(\\)` issued the following warning:") %>%
    suppressMessages()
})

test_that("run_ml uses custom training indices when provided", {
  set.seed(2019)
  n_obs <- otu_mini_bin %>% nrow()
  training_size <- 0.8 * n_obs
  training_rows <- sample(n_obs, training_size)
  expect_warning(
    results_custom_train <- run_ml(otu_mini_bin,
      "glmnet",
      kfold = 2,
      cv_times = 5,
      training_frac = training_rows,
      seed = 2019
    )
  ) %>% suppressMessages()
  expect_true(dplyr::all_equal(
    results_custom_train$test_data,
    otu_mini_bin[-training_rows, ]
  ))
})

test_that("run_ml uses custom group partitions", {
  set.seed(2019)
  grps <- sample(LETTERS[1:8], nrow(otu_mini_bin), replace = TRUE)
  group_part <- list(train = c("A", "B"), test = c("C", "D"))
  expect_warning(
    expect_message(
      results_grp_part <- run_ml(otu_mini_bin,
        "glmnet",
        cv_times = 2,
        training_frac = 0.8,
        groups = grps,
        group_partitions = group_part,
        seed = 2019
      ),
      "Groups in the training set: A B E F G H"
    )
  )
  set.seed(2019)
  train_ind <- create_grouped_data_partition(grps,
    group_partitions = group_part,
    training_frac = 0.8
  )
  expect_true(dplyr::all_equal(
    results_grp_part$test_data,
    otu_mini_bin[-train_ind, ]
  ))
})

test_that("run_ml catches bad training_frac values", {
  expect_error(
    run_ml(otu_mini_bin,
      "glmnet",
      outcome_colname = "dx",
      training_frac = 0
    ),
    "`training_frac` must be a numeric between 0 and 1."
  )
  expect_error(
    run_ml(otu_mini_bin,
      "glmnet",
      outcome_colname = "dx",
      training_frac = 1
    ),
    "`training_frac` must be a numeric between 0 and 1."
  )
})

test_that("models use repeatedcv", {
  expect_equal(otu_mini_bin_results_glmnet$trained_model$control$method, "repeatedcv")
  expect_equal(otu_mini_bin_results_glmnet$trained_model$control$repeats, 2)
})

test_that("models use case weights when provided", {
  skip_on_cran()
  set.seed(20221014)
  case_weights_dat <- otu_mini_bin %>%
    count(dx) %>%
    mutate(p = n / sum(n)) %>%
    select(dx, p)
  train_weights <- otu_mini_bin %>%
    inner_join(case_weights_dat, by = "dx") %>%
    mutate(
      in_train = sample(
        c(TRUE, FALSE),
        size = nrow(otu_mini_bin),
        replace = TRUE,
        prob = c(0.8, 0.2)
      ),
      row_num = row_number()
    ) %>%
    filter(in_train) %>%
    select(p, row_num)
  expect_warning(
    results_custom_train <- run_ml(
      otu_mini_bin,
      "glmnet",
      kfold = 2,
      cv_times = 5,
      training_frac = train_weights %>% pull(row_num),
      seed = 20221014,
      weights = train_weights %>% pull(p)
    ),
    "simpleWarning in nominalTrainWorkflow"
  )
  expect_true("weights" %in% colnames(results_custom_train$trained_model$pred))
  expect_false("weights" %in% colnames(otu_mini_bin_results_glmnet$trained_model$pred))
})

test_that("make sure impute function on train data set works", {
  train_data <- data.frame(outcome = c("normal", "normal", "cancer", NA),
                           var1 = 1:4,
                           var2 = c("a", "b", "c", "d"),
                           var3 = c("no", "yes", "no", "no"),
                           var4 = c(0, 1, 0, 0),
                           var5 = c(0, 0, 0, 0),
                           var6 = c("no", "no", "no", "no"),
                           var7 = c(1, 1, 0, 0),
                           var8 = c(5, 6, NA, 7),
                           var9 = c(NA, "x", "y", "z"),
                           var10 = c(1, 0, NA, NA))
  test_data <- data.frame(outcome = c("normal", "normal", "cancer", NA), var11 = c(1, 1, NA, NA),
                          var12 = c("1", "2", "3", "4"))
  train_data_output <- dplyr::tibble(
      outcome = c("normal", "normal", "cancer"),
      var1 = c(-1, 0, 1), var8 = c(-0.70710678, 0.70710678, 0), var3_yes = c(0, 1, 0), var7_1=c(1, 1, 0), var2_a = c(1, 0, 0), var2_b=c(0, 1, 0), var2_c=c(0, 0, 1), var9_x=c(0, 1, 0), var9_y = c(0, 0, 1), var6=c(0, 0, 0))
  results_output <- prep_data(train_data, 'outcome', 1, c("center", "scale"), TRUE, TRUE)
  final_results <- results_output$processed_feats
  split_dat <- results_output$split_dat
  train_df <- dplyr::bind_cols(split_dat$outcome, final_results) %>%
    dplyr::as_tibble()
  expect_equal(train_df, train_data_output)
})

test_that("make sure impute function on test data set works", {
  train_data <- data.frame(outcome = c("normal", "normal", "cancer", NA),
                           var1 = 1:4,
                           var2 = c("a", "b", "c", "d"),
                           var3 = c("no", "yes", "no", "no"),
                           var4 = c(0, 1, 0, 0),
                           var5 = c(0, 0, 0, 0),
                           var6 = c("no", "no", "no", "no"),
                           var7 = c(1, 1, 0, 0),
                           var8 = c(5, 6, NA, 7),
                           var9 = c(NA, "x", "y", "z"),
                           var10 = c(1, 0, NA, NA))
  test_data <- data.frame(outcome = c("normal", "normal", "cancer", NA), var11 = c(1, 1, NA, NA),
                          var12 = c("1", "2", "3", "4"))
  test_data_output <- dplyr::tibble(
    outcome = c("normal", "normal", "cancer"),
    var12 = c(-1, 0, 1), var11 = c(1, 1, 1))
  results_output <- prep_data(test_data, 'outcome', 1, c("center", "scale"), TRUE, TRUE)
  final_results <- results_output$processed_feats
  split_dat <- results_output$split_dat
  test_df <- dplyr::bind_cols(split_dat$outcome, final_results) %>%
    dplyr::as_tibble()
  expect_equal(test_df, test_data_output)
})

temp_df <- otu_mini_bin
Otu00011 <- c(6, 6, 6, NA, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, NA, 6, 6, 6, 6, 6, 6, 6, 
              6, NA, 6, 6, 6, 6, 6, 6, 6, 6, 6, NA, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 
              6, 6, 6, 6, 6, 6, 6, NA, 6, 6, 6, 6, 6, NA, 6, 6, 6, 6, 6, 6, 6, 6, 
              6, 6, NA, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 
              6, 6, 6, 6, 6, 6, 6, 6, 6, NA, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 
              6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, NA, 6, 6, 6, 6, 6, 6, 
              6, 6, 6, 6, NA, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 
              6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 
              6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 
              5, 5)
mini_bin_with_nas <- otu_mini_bin %>% mutate(Otu00011)

test_that("data gets imputed when impute_in_training is set to TRUE", {
  results <- run_ml(mini_bin_with_nas, "glmnet", 'dx', NULL, FALSE, TRUE, 5, 100, NULL, 0.5, NULL, NULL, NULL, NULL, 1, 2019, TRUE)
  temp <- colSums(is.na(results$test_data)) 
  num_nas <- sum(temp)
  expect_equal(0, num_nas) %>% suppressMessages() 
  temp <- colSums(is.na(results$trained_model$trainingData)) 
  num_nas <- sum(temp)
  expect_equal(0, num_nas) %>% suppressMessages()
})

test_that("data is not imputed when impute_in_training is set to FALSE", {
  expect_error(run_ml(mini_bin_with_nas, "glmnet", 'dx', NULL, FALSE, TRUE, 5, 100, NULL, 0.5, NULL, NULL, NULL, NULL, 1, 2019, FALSE),NULL) %>% suppressMessages()
})

test_that("data is not imputed when impute_in_training is not set", {
  expect_error(run_ml(mini_bin_with_nas, "glmnet", 'dx', NULL, FALSE, TRUE, 5, 100, NULL, 0.5, NULL, NULL, NULL, NULL, 1, 2019),NULL) %>% suppressMessages()
})