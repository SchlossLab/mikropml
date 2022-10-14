## code to prepare `otu_small` dataset
otu_large <- read.delim(system.file("data-raw", "otu_large_bin.csv",
  package = "mikropml"
), sep = ",")
otu_small <- otu_large[1:200, 1:61]
usethis::use_data(otu_small, overwrite = TRUE)

## code to prepare models with the `otu_small` otu_small
set.seed(2019)
outcome_colname <- "dx"

inTraining <-
  caret::createDataPartition(otu_small[, outcome_colname], p = .80, list = FALSE)
train_data_sm <- otu_small[inTraining, ]
usethis::use_data(train_data_sm, overwrite = TRUE)
test_data_sm <- otu_small[-inTraining, ]
usethis::use_data(test_data_sm, overwrite = TRUE)

default_hyperparams <- structure(list(
  param = c(
    "lambda", "lambda", "lambda", "lambda", "lambda",
    "lambda", "lambda", "lambda", "lambda", "lambda", "lambda", "lambda", "lambda",
    "alpha", "sigma", "sigma", "sigma", "sigma", "sigma",
    "sigma", "sigma", "sigma", "C", "C", "C", "C", "C", "C", "C",
    "C", "C", "maxdepth", "maxdepth", "maxdepth", "maxdepth", "maxdepth",
    "maxdepth", "nrounds", "gamma", "eta", "eta", "eta", "eta", "max_depth",
    "colsample_bytree", "min_child_weight", "subsample", "subsample",
    "subsample", "subsample", "mtry", "mtry"
  ),
  value = c(
    "1e-6",
    "1e-5", "1e-4", "1e-3", "0.0025", "0.005", "0.01", "0.05", "0.1",
    "0.25", "0.5", "1", "10", "1", "0.00000001",
    "0.0000001", "0.000001", "0.00001", "0.0001", "0.001", "0.01",
    "0.1", "0.0000001", "0.000001", "0.00001", "0.0001", "0.001",
    "0.01", "0.1", "1", "10", "1", "2", "3", "4", "5", "6", "500",
    "0", "0.001", "0.01", "0.1", "1", "8", "0.8", "1", "0.4", "0.5",
    "0.6", "0.7", "500", "1000"
  ),
  method = c(
    "glmnet", "glmnet",
    "glmnet", "glmnet", "glmnet", "glmnet", "glmnet",
    "glmnet", "glmnet", "glmnet", "glmnet", "glmnet",
    "glmnet", "glmnet", "svmRadial", "svmRadial",
    "svmRadial", "svmRadial", "svmRadial", "svmRadial", "svmRadial",
    "svmRadial", "svmRadial", "svmRadial", "svmRadial", "svmRadial",
    "svmRadial", "svmRadial", "svmRadial", "svmRadial", "svmRadial",
    "rpart2", "rpart2", "rpart2", "rpart2", "rpart2", "rpart2", "xgbTree",
    "xgbTree", "xgbTree", "xgbTree", "xgbTree", "xgbTree", "xgbTree",
    "xgbTree", "xgbTree", "xgbTree", "xgbTree", "xgbTree", "xgbTree", "rf", "rf"
  )
),
class = c("spec_tbl_df", "tbl_df", "tbl", "data.frame"),
row.names = c(NA, -52L),
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

set.seed(2019)

otu_large_bin_svmRadial <- mikropml::run_ml(
    otu_small,
    'svmRadial',
    outcome_colname = 'dx',
    find_feature_importance = FALSE,
    kfold = 5,
    cv_times = 2,
    seed = 2019
)
