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

hyperparameters <- default_hyperparams[default_hyperparams$method == "regLogistic", ]
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
  hyperparameters = mikRopML::default_hyperparams,
  find_feature_importance = FALSE,
  seed = 2019,
  nfolds = as.integer(2)
)
usethis::use_data(otu_mini_results1)
