## code to prepare `otu_small` dataset
otu_small <- otu_medium[, 1:61]
usethis::use_data(otu_small, overwrite = TRUE)

## code to prepare models with the `otu_small` otu_small
set.seed(2019)
outcome_colname <- "dx"

inTraining <-
  caret::createDataPartition(otu_small[, outcome_colname], p = .80, list = FALSE)
train_data_sm <- otu_small[inTraining, ]
test_data_sm <- otu_small[-inTraining, ]

hyperparameters <- default_hyperparams[default_hyperparams$method == "regLogistic", ]
hyperparameters <- split(hyperparameters$value, hyperparameters$param)

folds <- 5
set.seed(2019)
cvIndex <- caret::createMultiFolds(factor(train_data_sm[, outcome_colname]),
                                   folds,
                                   times = 100
)
otu_sm_cv5 <- caret::trainControl(
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
trained_model_sm1 <- caret::train(
  form,
  data = train_data_sm,
  method = "regLogistic",
  trControl = otu_sm_cv5,
  metric = "ROC",
  tuneGrid = grid,
  family = "binomial"
)

usethis::use_data(otu_sm_cv5, overwrite = TRUE)
usethis::use_data(train_data_sm, overwrite = TRUE)
usethis::use_data(test_data_sm, overwrite = TRUE)
usethis::use_data(trained_model_sm1, overwrite = TRUE)

## code to prepare `otu_sm_results1`
otu_sm_results1 <- mikRopML::run_ml(otu_small,
  "regLogistic",
  outcome_colname = "dx",
  outcome_value = "cancer",
  hyperparameters = mikRopML::default_hyperparams,
  find_feature_importance = FALSE,
  seed = 2019
)
usethis::use_data(otu_sm_results1, overwrite = TRUE)

# TODO: fix error:
# Error in { :
#     task 1 failed - "need at least two non-NA values to interpolate"
#   In addition: There were 50 or more warnings (use warnings() to see the first 50)
#otu_sm_results4 <- mikRopML::run_ml(otu_small,
#  "rpart2",
#  outcome_colname = "dx",
#  outcome_value = "cancer",
#  hyperparameters = mikRopML::default_hyperparams,
#  find_feature_importance = FALSE,
#  seed = 2019
#)
#usethis::use_data(otu_sm_results4)
