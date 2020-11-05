## code to prepare `otu_mini` dataset
set.seed(2019)
outcome_colname <- "Otu00001"
# TODO: recode dx column as continuous and use as outcome here
## code to prepare `otu_mini_cont_results1`
# includes grouping functionality & feature importance
set.seed(2019)
otu_mini_cont_results_glmnet <- mikropml::run_ml(otu_mini[, 2:4], # use built-in hyperparams
  "glmnet",
  outcome_colname = outcome_colname,
  find_feature_importance = TRUE,
  seed = 2019,
  cv_times = 2
)
usethis::use_data(otu_mini_cont_results_glmnet, overwrite = TRUE)
