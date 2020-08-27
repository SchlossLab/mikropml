## code to prepare `otu_medium` dataset goes here
otu_large <- read.delim("data-raw/otu_large.csv", sep = ",")
otu_medium <- otu_large[1:200, 1:200]
usethis::use_data(otu_medium, overwrite = TRUE)


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


otu_med_results <- mikRopML::run_ml(otu_medium,
  "rpart2",
  outcome_colname = "dx",
  outcome_value = "cancer",
  hyperparameters = get_hyperparams_from_df(test_hyperparams, "rpart2"),
  find_feature_importance = FALSE,
  seed = 2019,
  kfold = as.integer(3)
)
usethis::use_data(otu_med_results, overwrite = TRUE)
