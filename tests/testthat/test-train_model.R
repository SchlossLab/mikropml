test_hyperparams <- data.frame(
  param = c(
    "lambda", "lambda", "lambda", "alpha",
    "sigma", "sigma", "C", "C",
    "maxdepth", "maxdepth",
    "nrounds", "gamma", "eta", "max_depth", "colsample_bytree", "min_child_weight", "subsample",
    "mtry", "mtry"
  ),
  value = c(
    "1e-3", "1e-2", "1e-1", "1",
    "0.00000001", "0.0000001", "0.01", "0.1",
    "1", "2",
    "10", "0", "0.01", "1", "0.8", "1", "0.4",
    "1", "2"
  ),
  method = c(
    "glmnet", "glmnet", "glmnet", "glmnet",
    "svmRadial", "svmRadial", "svmRadial", "svmRadial",
    "rpart2", "rpart2",
    "xgbTree", "xgbTree", "xgbTree", "xgbTree", "xgbTree", "xgbTree", "xgbTree",
    "rf", "rf"
  )
)

tg_rpart2 <- get_tuning_grid(get_hyperparams_from_df(test_hyperparams, "rpart2"), "rpart2")
tg_rf <- get_tuning_grid(get_hyperparams_from_df(test_hyperparams, "rf"), "rf")
tg_lr <- get_tuning_grid(get_hyperparams_from_df(test_hyperparams, "glmnet"), "glmnet")

hparams_list <- list(lambda = c("1e-3", "1e-2", "1e-1"), alpha = "0.01")
cv <- define_cv(otu_mini_bin_results_glmnet$trained_model$trainingData %>%
  dplyr::rename(dx = .outcome),
"dx",
hparams_list,
perf_metric_function = caret::multiClassSummary,
class_probs = TRUE,
cv_times = 2
)


test_that("train_model works", {
  # NOTE: these tests pass when you run test(), but not when you run test_file()
  # Perhaps something weird is going on with random seeds.
  set.seed(2019)
  expect_equal(
    train_model(
      stats::as.formula(paste("dx", "~ .")),
      otu_mini_bin_results_glmnet$trained_model$trainingData %>%
        dplyr::rename(dx = .outcome),
      "rf",
      cv,
      "AUC",
      tg_rf,
      1000
    )$bestTune,
    data.frame(mtry = 1L)
  )

  set.seed(2019)
  expect_equal(
    train_model(
      stats::as.formula(paste("dx", "~ .")),
      otu_mini_bin_results_glmnet$trained_model$trainingData %>%
        dplyr::rename(dx = .outcome),
      "rpart2", cv, "AUC",
      tg_rpart2, NULL
    )$bestTune$maxdepth,
    2
  )

  set.seed(2019)
  expect_equal(
    expect_warning(
      train_model(
        stats::as.formula(paste("dx", "~ .")),
        otu_mini_bin_results_glmnet$trained_model$trainingData %>%
          dplyr::rename(dx = .outcome),
        "glmnet", cv, "AUC", tg_lr, NULL
      )$bestTune$lambda,
      "`caret::train\\(\\)` issued the following warning:"
    ),
    0.01
  )
})
