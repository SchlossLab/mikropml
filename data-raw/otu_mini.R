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
set.seed(2019)
otu_mini_group <- sample(LETTERS[1:5], nrow(otu_small), replace = TRUE)
otu_mini_results1 <- mikropml::run_ml(otu_small[, 1:20], # use built-in hyperparams
  "glmnet",
  outcome_colname = "dx",
  find_feature_importance = FALSE,
  seed = 2019,
  kfold = 2,
  cv_times = 2
)
usethis::use_data(otu_mini_results1, overwrite = TRUE)

# cv2_group <- sample(LETTERS[1:4], nrow(otu_mini_results1$trained_model$trainingData), replace = TRUE)
cv2_group <- c(
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
otu_mini_cv2 <- define_cv(otu_mini_results1$trained_model$trainingData,
  "dx",
  hparams_list,
  perf_metric_function = caret::multiClassSummary,
  class_probs = TRUE,
  kfold = 2,
  cv_times = 2,
  group = cv2_group
)
usethis::use_data(otu_mini_cv2, overwrite = TRUE)

# use built-in hyperparams function for this one
otu_mini_results2 <- mikropml::run_ml(otu_mini,
  "rf",
  outcome_colname = "dx",
  find_feature_importance = TRUE,
  seed = 2019,
  kfold = 2,
  cv_times = 2,
  group = otu_mini_group
)
usethis::use_data(otu_mini_results2, overwrite = TRUE)

otu_mini_results3 <- mikropml::run_ml(otu_mini,
  "svmRadial",
  outcome_colname = "dx",
  hyperparameters = get_hyperparams_from_df(test_hyperparams, "svmRadial"),
  find_feature_importance = FALSE,
  seed = 2019,
  kfold = 2,
  cv_times = 2
)
usethis::use_data(otu_mini_results3, overwrite = TRUE)

otu_mini_results4 <- mikropml::run_ml(otu_mini,
  "xgbTree",
  outcome_colname = "dx",
  hyperparameters = get_hyperparams_from_df(test_hyperparams, "xgbTree"),
  find_feature_importance = FALSE,
  seed = 2019,
  kfold = 2,
  cv_times = 2
)
usethis::use_data(otu_mini_results4, overwrite = TRUE)

otu_mini_results5 <- mikropml::run_ml(otu_mini,
  "rpart2",
  outcome_colname = "dx",
  find_feature_importance = FALSE,
  seed = 2019,
  kfold = 2,
  cv_times = 2
)
usethis::use_data(otu_mini_results5, overwrite = TRUE)
