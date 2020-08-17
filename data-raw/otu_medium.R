## code to prepare `otu_medium` dataset goes here
otu_medium <- read.delim("data-raw/otu_medium.csv", sep = ",")
usethis::use_data(otu_medium, overwrite = TRUE)
otu_med_results4 <- mikRopML::run_ml(otu_medium,
                 "rpart2",
                 outcome_colname = "dx",
                 outcome_value = "cancer",
                 hyperparameters = mikRopML::default_hyperparams,
                 find_feature_importance = FALSE,
                 seed = 2019
)
