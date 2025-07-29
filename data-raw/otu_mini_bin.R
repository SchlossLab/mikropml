set.seed(2019)
library(dplyr)
library(usethis)

## code to prepare `otu_mini` dataset
otu_mini_bin <- otu_small[, 1:11]
use_data(otu_mini_bin, overwrite = TRUE)

otu_data_preproc <- preprocess_data(otu_mini_bin, "dx")
use_data(otu_data_preproc)

## code to prepare `otu_mini_results`
# includes grouping functionality & feature importance

# otu_mini_group <- sample(LETTERS[1:6], nrow(otu_mini_bin), replace = TRUE)
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

otu_mini_bin_results_glmnet <- mikropml::run_ml(otu_mini_bin, # use built-in hyperparams
  "glmnet",
  outcome_colname = "dx",
  pos_class = "cancer",
  find_feature_importance = FALSE,
  seed = 2019,
  cv_times = 2
)
use_data(otu_mini_bin_results_glmnet, overwrite = TRUE)

# cv_group <- sample(LETTERS[1:5], nrow(otu_mini_bin_results_glmnet$trained_model$trainingData), replace = TRUE)
cv_group <- c(
  "C", "D", "E", "C", "D", "E", "A", "D", "A", "D", "D", "A",
  "B", "E", "D", "A", "D", "E", "B", "E", "A", "B", "A", "E", "A",
  "D", "A", "D", "A", "C", "A", "B", "B", "E", "A", "E", "B", "C",
  "D", "D", "C", "A", "E", "E", "B", "B", "A", "C", "D", "D", "D",
  "D", "A", "D", "C", "A", "D", "D", "B", "C", "E", "C", "E", "C",
  "B", "D", "B", "D", "C", "B", "B", "B", "B", "B", "B", "B", "C",
  "D", "D", "E", "A", "E", "D", "E", "A", "D", "A", "E", "E", "C",
  "B", "B", "E", "B", "C", "C", "D", "A", "A", "E", "E", "C", "A",
  "C", "E", "A", "D", "A", "C", "D", "E", "E", "A", "A", "B", "E",
  "C", "B", "B", "C", "C", "D", "C", "E", "E", "E", "C", "E", "D",
  "D", "B", "B", "B", "E", "E", "A", "A", "A", "B", "D", "B", "D",
  "B", "B", "B", "D", "B", "B", "D", "B", "D", "C", "C", "B", "A",
  "A", "D", "C", "E", "E", "A"
)

hparams_list <- list(lambda = c("1e-3", "1e-2", "1e-1"), alpha = "0.01")
outcome_type <- get_outcome_type(otu_mini_bin %>% dplyr::pull("dx"))
class_probs <- outcome_type != "numeric"
perf_metric_function <- get_perf_metric_fn(outcome_type)
set.seed(2019)
otu_mini_cv <- define_cv(otu_mini_bin_results_glmnet$trained_model$trainingData,
  "dx",
  hparams_list,
  perf_metric_function = caret::multiClassSummary,
  class_probs = TRUE,
  cv_times = 2,
  groups = cv_group
)
use_data(otu_mini_cv, overwrite = TRUE)

# use built-in hyperparams function for this one
otu_mini_bin_results_rf <- mikropml::run_ml(otu_mini_bin,
  "rf",
  outcome_colname = "dx",
  pos_class = "cancer",
  find_feature_importance = TRUE,
  seed = 2019,
  cv_times = 2,
  groups = otu_mini_group
)
use_data(otu_mini_bin_results_rf, overwrite = TRUE)

otu_mini_bin_results_svmRadial <- mikropml::run_ml(otu_mini_bin,
  "svmRadial",
  outcome_colname = "dx",
  pos_class = "cancer",
  find_feature_importance = FALSE,
  seed = 2019,
  cv_times = 2
)
use_data(otu_mini_bin_results_svmRadial, overwrite = TRUE)

otu_mini_bin_results_xgbTree <- mikropml::run_ml(otu_mini_bin,
  "xgbTree",
  outcome_colname = "dx",
  pos_class = "cancer",
  find_feature_importance = FALSE,
  seed = 2019,
  cv_times = 2
)
use_data(otu_mini_bin_results_xgbTree, overwrite = TRUE)

otu_mini_bin_results_rpart2 <- mikropml::run_ml(otu_mini_bin,
  "rpart2",
  outcome_colname = "dx",
  pos_class = "cancer",
  find_feature_importance = FALSE,
  seed = 2019,
  cv_times = 2
)
use_data(otu_mini_bin_results_rpart2, overwrite = TRUE)
