otu_large_multi <- read.delim("data-raw/otu_large_multi.csv", sep = ",")
otu_mini_multi <- otu_large_multi[, 1:11]
usethis::use_data(otu_mini_multi, overwrite = TRUE)

set.seed(2019)
otu_mini_multi_group <- sample(LETTERS[1:10], nrow(otu_mini_multi), replace = TRUE)
usethis::use_data(otu_mini_multi_group, overwrite = TRUE)

otu_mini_multi_results_glmnet <- mikropml::run_ml(otu_mini_multi, # use built-in hyperparams
  "glmnet",
  outcome_colname = "dx",
  find_feature_importance = TRUE,
  seed = 2019,
  cv_times = 2,
  group = otu_mini_multi_group
)
usethis::use_data(otu_mini_multi_results_glmnet, overwrite = TRUE)
