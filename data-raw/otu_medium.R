## code to prepare `otu_medium` dataset goes here
otu_large <- read.delim("data-raw/otu_large.csv", sep = ",")
otu_medium <- otu_large[1:200, 1:200]
usethis::use_data(otu_medium, overwrite = TRUE)
otu_med_results <- mikRopML::run_ml(otu_medium,
  "rpart2",
  outcome_colname = "dx",
  outcome_value = "cancer",
  hyperparameters = mikRopML::test_hyperparams,
  find_feature_importance = FALSE,
  seed = 2019,
  kfold = as.integer(3)
)
usethis::use_data(otu_med_results, overwrite = TRUE)
