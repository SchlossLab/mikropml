## code to prepare `otu_mini` dataset
set.seed(2019)
otu_mini <- otu_small[, 1:4]
usethis::use_data(otu_mini, overwrite = TRUE)

test_hyperparams <- structure(list(
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
),
class = c("spec_tbl_df", "tbl_df", "tbl", "data.frame"), row.names = c(NA, -19L)
)

## code to prepare `otu_mini_results`
# includes grouping functionality & feature importance
# set.seed(2019)
# otu_mini_group <- sample(LETTERS[1:5], nrow(otu_small), replace = TRUE)

otu_mini_group <- c(
  "A", "E", "B", "E", "E", "A", "E", "D", "C", "B", "A", "E",
  "B", "A", "C", "C", "D", "E", "C", "D", "E", "A", "D", "A", "D",
  "D", "A", "B", "E", "D", "A", "D", "E", "B", "E", "A", "B", "A",
  "E", "A", "D", "A", "D", "A", "C", "A", "B", "B", "E", "A", "E",
  "B", "C", "D", "D", "C", "A", "E", "E", "B", "B", "A", "C", "D",
  "D", "D", "D", "A", "D", "C", "A", "D", "D", "B", "C", "E", "C",
  "E", "C", "B", "D", "B", "D", "C", "B", "B", "B", "B", "B", "B",
  "B", "C", "D", "D", "E", "A", "E", "D", "E", "A", "D", "A", "E",
  "E", "C", "B", "B", "E", "B", "C", "C", "D", "A", "A", "E", "E",
  "C", "A", "C", "E", "A", "D", "A", "C", "D", "E", "E", "A", "A",
  "B", "E", "C", "B", "B", "C", "C", "D", "C", "E", "E", "E", "C",
  "E", "D", "D", "B", "B", "B", "E", "E", "A", "A", "A", "B", "D",
  "B", "D", "B", "B", "B", "D", "B", "B", "D", "B", "D", "C", "C",
  "B", "A", "A", "D", "C", "E", "E", "A", "B", "B", "A", "B", "A",
  "B", "E", "A", "C", "E", "A", "A", "E", "C", "C", "C", "B", "D",
  "D", "B", "B", "E", "D", "D"
)

otu_mini_results_glmnet <- mikropml::run_ml(otu_mini, # use built-in hyperparams
  "glmnet",
  outcome_colname = "dx",
  find_feature_importance = FALSE,
  seed = 2019,
  kfold = 2,
  cv_times = 2
)
usethis::use_data(otu_mini_results_glmnet, overwrite = TRUE)

# cv_group <- sample(LETTERS[1:4], nrow(otu_mini_results1$trained_model$trainingData), replace = TRUE)
cv_group <- c(
  "A", "A", "B", "A", "A", "A", "D", "C", "A", "D", "C", "B",
  "A", "A", "B", "B", "A", "D", "C", "C", "B", "B", "C", "D", "C",
  "D", "C", "A", "C", "D", "A", "C", "C", "A", "D", "A", "B", "C",
  "C", "D", "B", "D", "B", "A", "C", "D", "B", "C", "D", "B", "A",
  "D", "C", "A", "B", "C", "B", "D", "A", "B", "B", "B", "A", "A",
  "D", "B", "A", "A", "A", "D", "A", "D", "C", "A", "D", "C", "A",
  "D", "B", "C", "C", "B", "D", "A", "B", "C", "B", "A", "A", "B",
  "D", "C", "B", "D", "C", "D", "D", "D", "C", "C", "A", "A", "A",
  "B", "B", "B", "A", "B", "D", "C", "C", "D", "D", "C", "B", "D",
  "D", "C", "A", "D", "B", "C", "A", "D", "D", "B", "C", "B", "D",
  "C", "B", "A", "C", "B", "B", "C", "A", "C", "B", "D", "B", "D",
  "C", "B"
)

hparams_list <- list(lambda = c("1e-3", "1e-2", "1e-1"), alpha = "0.01")
outcome_type <- get_outcome_type(otu_mini %>% dplyr::pull("dx"))
class_probs <- outcome_type != "numeric"
perf_metric_function <- get_perf_metric_fn(outcome_type)
set.seed(2019)
otu_mini_cv <- define_cv(otu_mini_results1$trained_model$trainingData,
  "dx",
  hparams_list,
  perf_metric_function = caret::multiClassSummary,
  class_probs = TRUE,
  cv_times = 2,
  group = cv2_group
)
usethis::use_data(otu_mini_cv, overwrite = TRUE)

# use built-in hyperparams function for this one
otu_mini_results_rf <- mikropml::run_ml(otu_mini,
  "rf",
  outcome_colname = "dx",
  find_feature_importance = TRUE,
  seed = 2019,
  kfold = 2,
  cv_times = 2,
  group = otu_mini_group
)
usethis::use_data(otu_mini_results_rf, overwrite = TRUE)

otu_mini_results_svmRadial <- mikropml::run_ml(otu_mini,
  "svmRadial",
  outcome_colname = "dx",
  hyperparameters = get_hyperparams_from_df(test_hyperparams, "svmRadial"),
  find_feature_importance = FALSE,
  seed = 2019,
  kfold = 2,
  cv_times = 2
)
usethis::use_data(otu_mini_results3, overwrite = TRUE)

otu_mini_results_xgbTree <- mikropml::run_ml(otu_mini,
  "xgbTree",
  outcome_colname = "dx",
  hyperparameters = get_hyperparams_from_df(test_hyperparams, "xgbTree"),
  find_feature_importance = FALSE,
  seed = 2019,
  kfold = 2,
  cv_times = 2
)
usethis::use_data(otu_mini_results_sgbTree, overwrite = TRUE)

otu_mini_results_rpart2 <- mikropml::run_ml(otu_mini,
  "rpart2",
  outcome_colname = "dx",
  find_feature_importance = FALSE,
  seed = 2019,
  kfold = 2,
  cv_times = 2
)
usethis::use_data(otu_mini_results_rpart2, overwrite = TRUE)
