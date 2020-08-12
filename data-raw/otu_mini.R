## code to prepare `otu_mini` dataset
otu_mini <- otu_medium[, 1:4]
usethis::use_data(otu_mini)

## code to prepare models with the `otu_mini` otu_mini
set.seed(2019)
outcome_colname <- "dx"

inTraining <-
  caret::createDataPartition(otu_mini[, outcome_colname], p = .80, list = FALSE)
train_data_mini <- otu_mini[inTraining, ]
test_data_mini <- otu_mini[-inTraining, ]

hyperparameters <- test_hyperparams[test_hyperparams$method == "regLogistic", ]
hyperparameters <- split(hyperparameters$value, hyperparameters$param)

folds <- 5
set.seed(2019)
cvIndex <- caret::createMultiFolds(factor(train_data_mini[, outcome_colname]),
  folds,
  times = 100
)
otu_mini_cv5 <- caret::trainControl(
  method = "repeatedcv",
  number = folds,
  index = cvIndex,
  returnResamp = "final",
  classProbs = TRUE,
  summaryFunction = caret::twoClassSummary,
  indexFinal = NULL,
  savePredictions = TRUE
)
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

usethis::use_data(otu_mini_cv5)
usethis::use_data(train_data_mini)
usethis::use_data(test_data_mini)
usethis::use_data(trained_model_mini)

## code to prepare `otu_mini_results`
otu_mini_results1 <- mikRopML::run_ml(otu_mini,
  "regLogistic",
  outcome_colname = "dx",
  outcome_value = "cancer",
  hyperparameters = mikRopML::test_hyperparams,
  find_feature_importance = FALSE,
  seed = 2019,
  nfolds = as.integer(2)
)
usethis::use_data(otu_mini_results1)

otu_mini_results2 <- mikRopML::run_ml(otu_mini,
                                      "rf",
                                      outcome_colname = "dx",
                                      outcome_value = "cancer",
                                      hyperparameters = mikRopML::test_hyperparams,
                                      find_feature_importance = FALSE,
                                      seed = 2019,
                                      nfolds = as.integer(2)
)
usethis::use_data(otu_mini_results2)

otu_mini_results3 <- mikRopML::run_ml(otu_mini,
                                      "svmRadial",
                                      outcome_colname = "dx",
                                      outcome_value = "cancer",
                                      hyperparameters = mikRopML::test_hyperparams,
                                      find_feature_importance = FALSE,
                                      seed = 2019,
                                      nfolds = as.integer(2)
)
usethis::use_data(otu_mini_results3)

# TODO: fix error:
# Error in { :
#     task 1 failed - "need at least two non-NA values to interpolate"
#   In addition: There were 50 or more warnings (use warnings() to see the first 50)
otu_mini_results4 <- mikRopML::run_ml(otu_mini,
                                      "rpart2",
                                      outcome_colname = "dx",
                                      outcome_value = "cancer",
                                      hyperparameters = mikRopML::test_hyperparams,
                                      find_feature_importance = FALSE,
                                      seed = 2019,
                                      nfolds = as.integer(2)
)
usethis::use_data(otu_mini_results4)

otu_mini_results5 <- mikRopML::run_ml(otu_mini,
                                      "xgbTree",
                                      outcome_colname = "dx",
                                      outcome_value = "cancer",
                                      hyperparameters = mikRopML::test_hyperparams,
                                      find_feature_importance = FALSE,
                                      seed = 2019,
                                      nfolds = as.integer(2)
)
usethis::use_data(otu_mini_results5)
