## code to prepare `otu_mini` dataset
set.seed(2019)
otu_mini <- otu_medium[, 1:4]
usethis::use_data(otu_mini, overwrite = TRUE)

test_hyperparams <- structure(list(
  param = c(
    "cost", "cost", "cost", "loss", "epsilon",
    "sigma", "sigma", "C", "C", "maxdepth", "maxdepth", "nrounds",
    "gamma", "eta", "max_depth", "colsample_bytree", "min_child_weight",
    "subsample", "mtry", "mtry"
  ),
  value = c(
    "1e-3", "1e-2", "1e-1", "L2_primal", "0.01", "0.00000001", "0.0000001", "0.01", "0.1",
    "1", "2", "10", "0", "0.01", "1", "0.8", "1", "0.4", "1", "2"
  ),
  method = c(
    "regLogistic", "regLogistic", "regLogistic", "regLogistic",
    "regLogistic", "svmRadial", "svmRadial", "svmRadial", "svmRadial",
    "rpart2", "rpart2", "xgbTree", "xgbTree", "xgbTree", "xgbTree",
    "xgbTree", "xgbTree", "xgbTree", "rf", "rf"
  )
),
class = c("spec_tbl_df", "tbl_df", "tbl", "data.frame"), row.names = c(NA, -20L)
)

## code to prepare `otu_mini_results`
# includes grouping functionality & feature importance
set.seed(2019)
# otu_mini_group <- sample(LETTERS[1:10], nrow(otu_mini), replace = TRUE)
otu_mini_group <- c(
  "I", "J", "E", "A", "H", "G", "C", "J", "I", "J", "A", "H",
  "C", "G", "H", "C", "D", "E", "C", "D", "E", "G", "I", "A", "G",
  "F", "F", "A", "J", "G", "F", "E", "A", "F", "E", "J", "F", "A",
  "B", "A", "A", "I", "I", "C", "A", "H", "J", "G", "G", "B", "F",
  "F", "I", "J", "H", "G", "F", "H", "H", "C", "I", "E", "B", "B",
  "I", "H", "G", "C", "G", "G", "I", "F", "I", "D", "J", "H", "C",
  "F", "C", "F", "E", "C", "B", "B", "D", "G", "F", "F", "J", "B",
  "B", "G", "G", "J", "B", "J", "J", "G", "D", "G", "H", "I", "H",
  "D", "G", "I", "F", "A", "E", "C", "B", "B", "E", "J", "F", "H",
  "C", "F", "C", "D", "I", "H", "A", "G", "E", "F", "A", "C", "E",
  "I", "D", "A", "C", "D", "H", "A", "A", "J", "F", "E", "C", "J",
  "J", "G", "C", "D", "H", "E", "E", "F", "G", "F", "C", "E", "F",
  "D", "D", "B", "J", "B", "H", "A", "A", "A", "B", "D", "J", "D",
  "F", "F", "B", "G", "J", "B", "F", "G", "F", "J", "B", "D", "B",
  "C", "C", "H", "B", "F", "I", "G", "I", "D", "G", "G", "E", "F",
  "I", "B", "B", "I", "J", "A"
)
otu_mini_results1 <- mikropml::run_ml(otu_mini, # use built-in hyperparams
  "regLogistic",
  outcome_colname = "dx",
  outcome_value = "cancer",
  find_feature_importance = TRUE,
  seed = 2019,
  kfold = 2,
  cv_times = 2,
  group = otu_mini_group
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
hparams_list <- get_hyperparams_from_df(test_hyperparams, "regLogistic")
outcome_type <- get_outcome_type(otu_mini %>% dplyr::pull("dx"))
class_probs <- outcome_type != "numeric"
perf_metric_function <- get_perf_metric_fn(outcome_type)
set.seed(2019)
otu_mini_cv2 <- define_cv(otu_mini_results1$trained_model$trainingData,
  "dx",
  hparams_list,
  perf_metric_function,
  class_probs,
  kfold = 2,
  cv_times = 2,
  group = cv2_group
)
usethis::use_data(otu_mini_cv2, overwrite = TRUE)

# use built-in hyperparams function for this one
otu_mini_results2 <- mikropml::run_ml(otu_mini,
  "rf",
  outcome_colname = "dx",
  outcome_value = "cancer",
  find_feature_importance = FALSE,
  seed = 2019,
  kfold = 2,
  cv_times = 5
)
usethis::use_data(otu_mini_results2, overwrite = TRUE)

otu_mini_results3 <- mikropml::run_ml(otu_mini,
  "svmRadial",
  outcome_colname = "dx",
  outcome_value = "cancer",
  hyperparameters = get_hyperparams_from_df(test_hyperparams, "svmRadial"),
  find_feature_importance = FALSE,
  seed = 2019,
  kfold = 2,
  cv_times = 5
)
usethis::use_data(otu_mini_results3, overwrite = TRUE)

otu_mini_results4 <- mikropml::run_ml(otu_mini,
  "xgbTree",
  outcome_colname = "dx",
  outcome_value = "cancer",
  hyperparameters = get_hyperparams_from_df(test_hyperparams, "xgbTree"),
  find_feature_importance = FALSE,
  seed = 2019,
  kfold = 2,
  cv_times = 5
)
usethis::use_data(otu_mini_results4, overwrite = TRUE)

otu_mini_results5 <- mikropml::run_ml(otu_mini,
  "rpart2",
  outcome_colname = "dx",
  outcome_value = "cancer",
  find_feature_importance = FALSE,
  seed = 2019,
  kfold = 2,
  cv_times = 5
)
usethis::use_data(otu_mini_results5, overwrite = TRUE)
