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

folds <- 5
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

grid <- check_hyperparams_df(default_hyperparams, "regLogistic") %>% get_tuning_grid()

set.seed(2019)
trained_model_sm <- caret::train(
  stats::as.formula(paste(outcome_colname, "~ .")),
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
usethis::use_data(trained_model_sm, overwrite = TRUE)

## code to prepare `otu_sm_results`
otu_sm_results <- mikRopML::run_ml(otu_small,
  "regLogistic",
  outcome_colname = "dx",
  outcome_value = "cancer",
  hyperparameters = mikRopML::default_hyperparams,
  find_feature_importance = FALSE,
  seed = 2019
)
usethis::use_data(otu_sm_results, overwrite = TRUE)
