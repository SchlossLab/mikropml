## code to prepare `otu_small` dataset
otu_large <- read.delim(system.file("data-raw", "otu_large.csv",
  package = "mikropml"
), sep = ",")
otu_small <- otu_large[1:200, 1:61]
usethis::use_data(otu_small, overwrite = TRUE)

## code to prepare models with the `otu_small` otu_small
set.seed(2019)
outcome_colname <- "dx"
kfolds <- 2

inTraining <-
  caret::createDataPartition(otu_small[, outcome_colname], p = .80, list = FALSE)
train_data_sm <- otu_small[inTraining, ]
usethis::use_data(train_data_sm, overwrite = TRUE)
test_data_sm <- otu_small[-inTraining, ]
usethis::use_data(test_data_sm, overwrite = TRUE)

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

set.seed(2019)
hparams_list <- get_hyperparams_from_df(default_hyperparams, "regLogistic")
otu_sm_cv5 <- define_cv(train_data_sm, outcome_colname, hparams_list, perf_metric_function = twoClassSummary, class_probs = TRUE, 2, 5, seed = 2019)

trained_model_sm1 <- caret::train(
  stats::as.formula(paste(outcome_colname, "~ .")),
  data = train_data_sm,
  method = "regLogistic",
  trControl = otu_sm_cv5,
  metric = "ROC",
  tuneGrid = get_tuning_grid(hparams_list, "regLogistic"),
  family = "binomial"
)

## code to prepare `otu_sm_results1`
otu_sm_results1 <- mikropml::run_ml(otu_small,
  "regLogistic",
  outcome_colname = outcome_colname,
  find_feature_importance = FALSE,
  kfold = 2,
  cv_times = 5,
  seed = 2019
)
# usethis::use_data(otu_sm_results1, overwrite = TRUE)

# TODO: fix error:
# Error in { :
#     task 1 failed - "need at least two non-NA values to interpolate"
#   In addition: There were 50 or more warnings (use warnings() to see the first 50)
# otu_sm_results4 <- mikropml::run_ml(otu_small,
#  "rpart2",
#  outcome_colname = "dx",
#  outcome_value = "cancer",
#  hyperparameters = mikropml::default_hyperparams,
#  find_feature_importance = FALSE,
#  seed = 2019
# )
# usethis::use_data(otu_sm_results4)
