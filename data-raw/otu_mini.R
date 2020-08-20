## code to prepare `otu_mini` dataset
otu_mini <- otu_medium[, 1:4]
usethis::use_data(otu_mini, overwrite = TRUE)

## code to prepare models with the `otu_mini` otu_mini
set.seed(2019)
outcome_colname <- "dx"

inTraining <-
  caret::createDataPartition(otu_mini[, outcome_colname], p = .80, list = FALSE)
train_data_mini <- otu_mini[inTraining, ]
test_data_mini <- otu_mini[-inTraining, ]

hyperparameters <- test_hyperparams[test_hyperparams$method == "regLogistic", ]
hyperparameters <- split(hyperparameters$value, hyperparameters$param)

folds <- 2
set.seed(2019)
cvIndex <- caret::createMultiFolds(factor(train_data_mini[, outcome_colname]),
  folds,
  times = 100
)
otu_mini_cv5 <- define_cv(train_data_mini, "dx", 2, 100, 2019)

grid <- expand.grid(
  cost = hyperparameters$cost,
  loss = "L2_primal",
  epsilon = 0.01
)
form <- stats::as.formula(paste(outcome_colname, "~ ."))
trained_model_mini <- caret::train(
  form,
  data = train_data_mini,
  method = "regLogistic",
  trControl = otu_mini_cv5,
  metric = "ROC",
  tuneGrid = grid,
  family = "binomial"
)

usethis::use_data(otu_mini_cv5, overwrite = TRUE)
usethis::use_data(train_data_mini, overwrite = TRUE)
usethis::use_data(test_data_mini, overwrite = TRUE)
usethis::use_data(trained_model_mini, overwrite = TRUE)

## code to prepare `otu_mini_results`
otu_mini_results1 <- mikRopML::run_ml(otu_mini,
  "regLogistic",
  outcome_colname = "dx",
  outcome_value = "cancer",
  hyperparameters = mikRopML::test_hyperparams,
  find_feature_importance = FALSE,
  seed = 2019,
  kfold = 2
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


otu_mini_results5 <- mikRopML::run_ml(otu_mini,
  "xgbTree",
  outcome_colname = "dx",
  outcome_value = "cancer",
  hyperparameters = mikRopML::test_hyperparams,
  find_feature_importance = FALSE,
  seed = 2019,
  kfold = 2
)
usethis::use_data(otu_mini_results5, overwrite = TRUE)
