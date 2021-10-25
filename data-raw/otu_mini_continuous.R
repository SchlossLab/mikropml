otu_mini_cont_results_glmnet <- mikropml::run_ml(otu_mini_bin[, 2:11],
  "glmnet",
  outcome_colname = "Otu00001",
  find_feature_importance = TRUE,
  seed = 2019,
  cv_times = 2
)
usethis::use_data(otu_mini_cont_results_glmnet, overwrite = TRUE)

otu_mini_cont_results_nocv <- mikropml::run_ml(
  otu_mini_bin[, 2:11],
  "glmnet",
  outcome_colname = "Otu00001",
  seed = 2019,
  hyperparameters = list(lambda = c(1e-04), alpha = 0),
  cross_val = caret::trainControl(method = "none"),
  calculate_performance = FALSE
)
usethis::use_data(otu_mini_cont_results_nocv, overwrite = TRUE)
