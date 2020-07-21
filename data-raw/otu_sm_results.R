## code to prepare `otu_sm_results`
otu_sm_results <- mikRopML::run_pipeline(otu_small,
  "L2_Logistic_Regression",
  outcome_colname = "dx",
  outcome_value = "cancer",
  hyperparameters = mikRopML::default_hyperparams,
  permute = FALSE,
  seed = 2019
)
usethis::use_data(otu_sm_results, overwrite = TRUE)
