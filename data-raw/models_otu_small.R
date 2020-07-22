## code to prepare models with the `otu_small` dataset

set.seed(2019)

inTraining <-
  caret::createDataPartition(dataset[, outcome_colname], p = .80, list = FALSE)
train_data_sm <- dataset[inTraining, ]
test_data_sm <- dataset[-inTraining, ]

hyperparameters <- default_hyperparams[default_hyperparams$model == "regLogistic", ]
hyperparameters <- split(hyperparameters$val, hyperparameters$param)

folds <- 5
cvIndex <- caret::createMultiFolds(factor(train_data[, "dx"]), folds, times = 100)
cv <- caret::trainControl(
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
trained_model_sm <- caret::train(
  form,
  data = train_data,
  method = "regLogistic",
  trControl = cv,
  metric = "ROC",
  tuneGrid = grid,
  family = "binomial"
)

usethis::use_data(train_data_sm, overwrite = TRUE)
usethis::use_data(test_data_sm, overwrite = TRUE)
usethis::use_data(trained_model_sm, overwrite = TRUE)
