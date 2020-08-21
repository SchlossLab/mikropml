## code to prepare `otu_mini` dataset
set.seed(2019)
outcome_colname <- "dx"
kfolds <- 2
otu_mini <- otu_medium[, 1:4]
usethis::use_data(otu_mini, overwrite = TRUE)

inTraining <-
  caret::createDataPartition(otu_mini[, outcome_colname], p = .80, list = FALSE)
train_data_mini <- otu_mini[inTraining, ]
usethis::use_data(train_data_mini, overwrite = TRUE)

test_data_mini <- otu_mini[-inTraining, ]
usethis::use_data(test_data_mini, overwrite = TRUE)

hparams_list <- test_hyperparams %>%
  check_hyperparams_df('regLogistic') %>%
  get_hyperparams_list()
otu_mini_cv2 <- define_cv(train_data_mini, outcome_colname, hparams_list, kfolds, 100, 2019)
usethis::use_data(otu_mini_cv2, overwrite = TRUE)

trained_model_mini <- caret::train(
  stats::as.formula(paste(outcome_colname, "~ .")),
  data = train_data_mini,
  method = "regLogistic",
  trControl = otu_mini_cv2,
  metric = "ROC",
  tuneGrid = get_tuning_grid(hparams_list, 'regLogistic'),
  family = "binomial"
)
usethis::use_data(trained_model_mini, overwrite = TRUE)

## code to prepare `otu_mini_results`
otu_mini_results1 <- mikRopML::run_ml(otu_mini,
  "regLogistic",
  outcome_colname = "dx",
  outcome_value = "cancer",
  hyperparameters = mikRopML::test_hyperparams,
  find_feature_importance = FALSE,
  seed = 2019,
  kfold = kfolds
)
usethis::use_data(otu_mini_results1, overwrite = TRUE)

otu_mini_results2 <- mikRopML::run_ml(otu_mini,
  "rf",
  outcome_colname = "dx",
  outcome_value = "cancer",
  hyperparameters = mikRopML::test_hyperparams,
  find_feature_importance = FALSE,
  seed = 2019,
  kfold = 2
)
usethis::use_data(otu_mini_results2, overwrite = TRUE)

otu_mini_results3 <- mikRopML::run_ml(otu_mini,
  "svmRadial",
  outcome_colname = "dx",
  outcome_value = "cancer",
  hyperparameters = mikRopML::test_hyperparams,
  find_feature_importance = FALSE,
  seed = 2019,
  kfold = 2
)
usethis::use_data(otu_mini_results3, overwrite = TRUE)

otu_mini_results4 <- mikRopML::run_ml(otu_mini,
  "xgbTree",
  outcome_colname = "dx",
  outcome_value = "cancer",
  hyperparameters = mikRopML::test_hyperparams,
  find_feature_importance = FALSE,
  seed = 2019,
  kfold = 2
)
usethis::use_data(otu_mini_results4, overwrite = TRUE)
