## code to prepare `otu_mini` dataset
set.seed(2019)
outcome_colname <- "Otu00001"
kfolds <- 2
otu_mini <- otu_small[, 1:4]
# usethis::use_data(otu_mini, overwrite = TRUE)

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


## code to prepare `otu_mini_cont_results1`
# includes grouping functionality & feature importance
set.seed(2019)
otu_mini_cont_results1 <- mikropml::run_ml(otu_mini[, 2:4], # use built-in hyperparams
  "glmnet",
  outcome_colname = outcome_colname,
  find_feature_importance = TRUE,
  seed = 2019,
  kfold = 2,
  cv_times = 2
)
usethis::use_data(otu_mini_cont_results1, overwrite = TRUE)

# use built-in hyperparams function for this one
otu_mini_cont_results2 <- mikropml::run_ml(otu_mini,
                                      "rf",
                                      outcome_colname = outcome_colname,
                                      find_feature_importance = FALSE,
                                      seed = 2019,
                                      kfold = 2,
                                      cv_times = 2
)

otu_mini_cont_results3 <- mikropml::run_ml(otu_mini,
                                      "svmRadial",
                                      outcome_colname = outcome_colname,
                                      hyperparameters = get_hyperparams_from_df(test_hyperparams, "svmRadial"),
                                      find_feature_importance = FALSE,
                                      seed = 2019,
                                      kfold = 2,
                                      cv_times = 2
)

otu_mini_cont_results4 <- mikropml::run_ml(otu_mini,
                                      "xgbTree",
                                      outcome_colname = "dx",
                                      hyperparameters = get_hyperparams_from_df(test_hyperparams, "xgbTree"),
                                      find_feature_importance = FALSE,
                                      seed = 2019,
                                      kfold = 2,
                                      cv_times = 2
)

otu_mini_cont_results5 <- mikropml::run_ml(otu_mini,
                                      "rpart2",
                                      outcome_colname = "dx",
                                      find_feature_importance = FALSE,
                                      seed = 2019,
                                      kfold = 2,
                                      cv_times = 2
)
