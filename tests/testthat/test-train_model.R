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

train_dat <- otu_mini_bin_results_glmnet$trained_model$trainingData %>%
    dplyr::rename(dx = .outcome)
split_dat <- split_outcome_features(train_dat, "dx")
outcomes_vctr <- split_dat$outcome %>% dplyr::pull("dx") %>% as.factor()
features_dat <- split_dat$features

hparams_list <- list(lambda = c("1e-3", "1e-2", "1e-1"), alpha = "0.01")
cv <- define_cv(
        train_dat,
        "dx",
        hparams_list,
        perf_metric_function = caret::multiClassSummary,
        class_probs = TRUE,
        cv_times = 2
    )


test_that("train_model works", {
  skip_on_cran() # this functionality is already tested in test-run_ml.R
  set.seed(2019)
  rf_model <- train_model(
    features_dat,
    outcomes_vctr,
    method = "rf",
    cv = define_cv(
        train_dat,
        "dx",
        get_hyperparams_list(train_dat, "rf"),
        perf_metric_function = caret::multiClassSummary,
        class_probs = TRUE,
        cv_times = 2
    ),
    perf_metric_name = "AUC",
    tune_grid = tg_rf,
    ntree = 1000,
    weights = NULL
  )
  auc <- rf_model$results %>%
    dplyr::filter(mtry == rf_model$bestTune$mtry) %>%
    dplyr::pull(AUC)
  expect_true(dplyr::near(auc, 0.68, tol = 10^-2))

  set.seed(2019)
  expect_equal(
    train_model(
      features_dat,
      outcomes_vctr,
      method = "rpart2",
      cv = cv,
      perf_metric_name = "AUC",
      tune_grid = tg_rpart2
    )$bestTune$maxdepth,
    2
  )

  set.seed(2019)
  expect_equal(
    train_model(
      features_dat,
      outcomes_vctr,
      method = "glmnet",
      cv = cv,
      perf_metric_name = "AUC",
      tune_grid = tg_lr,
      ntree = NULL
    )$bestTune$lambda,
    0.01
  ) %>% expect_warning("`caret::train\\(\\)` issued the following warning:")
})

test_that("case weights work", {
    case_weights_dat <- train_dat %>%
        dplyr::count(dx) %>%
        dplyr::mutate(p = n / sum(n)) %>%
        dplyr::select(dx, p)
    case_weights_vctr <- train_dat %>%
        dplyr::inner_join(case_weights_dat, by = 'dx') %>%
        dplyr::pull(p)
    expect_warning(
        lr_model_weighted <- train_model(
            features_dat,
            outcomes_vctr,
            method = "glmnet",
            cv = cv,
            perf_metric_name = "AUC",
            tune_grid = tg_lr,
            ntree = NULL,
            weights = case_weights_vctr
            ),
    "`caret::train\\(\\)` issued the following warning:")
    model_weights <- lr_model_weighted$pred %>%
        dplyr::select(obs, weights) %>%
        dplyr::distinct() %>%
        dplyr::rename(dx = obs, p = weights) %>%
        dplyr::mutate(dx = as.character(dx))

    expect_true(dplyr::all_equal(model_weights,
                                 case_weights_dat))
    expect_false("weights" %in% colnames(otu_mini_bin_results_glmnet$trained_model$pred))
})
