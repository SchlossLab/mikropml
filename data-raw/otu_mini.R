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

test_hyperparams <- structure(list(
  param = c(
    "cost", "cost", "cost", "loss", "epsilon",
    "sigma", "sigma", "C", "C", "maxdepth", "maxdepth", "nrounds",
    "gamma", "eta", "max_depth", "colsample_bytree", "min_child_weight",
    "subsample", "mtry", "mtry"
  ),
  value = c(
    "1e-3", "1e-2", "1e-1", "L2_primal", "0.01", "0.00000001", "0.0000001", "0.01", "0.1",
    "1", "2", "10", "0", "0.01", "1", "0.8", "1", "0.4", "1", "2"
  ),
  method = c(
    "regLogistic", "regLogistic", "regLogistic", "regLogistic",
    "regLogistic", "svmRadial", "svmRadial", "svmRadial", "svmRadial",
    "rpart2", "rpart2", "xgbTree", "xgbTree", "xgbTree", "xgbTree",
    "xgbTree", "xgbTree", "xgbTree", "rf", "rf"
  )
),
class = c("spec_tbl_df", "tbl_df", "tbl", "data.frame"), row.names = c(NA, -20L)
)

hparams_list <- get_hyperparams_from_df(test_hyperparams, "regLogistic")
otu_mini_cv2 <- define_cv(train_data_mini, outcome_colname, hparams_list, kfolds, 5, NULL, 2019)
usethis::use_data(otu_mini_cv2, overwrite = TRUE)

set.seed(0)
group <- sample(LETTERS[1:4], nrow(train_data_mini), replace = TRUE)
otu_mini_cv2_grp <- define_cv(train_data_mini, "dx", hparams_list, kfold = 2, cv_times = 2, seed = 2019, group = group)
usethis::use_data(otu_mini_cv2_grp, overwrite = TRUE)


trained_model_mini <- caret::train(
  stats::as.formula(paste(outcome_colname, "~ .")),
  data = train_data_mini,
  method = "regLogistic",
  hyperparameters = hparams_list,
  trControl = otu_mini_cv2,
  metric = "ROC",
  tuneGrid = get_tuning_grid(hparams_list, "regLogistic"),
  family = "binomial"
)
usethis::use_data(trained_model_mini, overwrite = TRUE)

## code to prepare `otu_mini_results`
otu_mini_results1 <- mikRopML::run_ml(otu_mini,
  "regLogistic",
  outcome_colname = "dx",
  outcome_value = "cancer",
  hyperparameters = get_hyperparams_from_df(test_hyperparams, "regLogistic"),
  find_feature_importance = FALSE,
  seed = 2019,
  kfold = 2,
  cv_times = 5
)
usethis::use_data(otu_mini_results1, overwrite = TRUE)

otu_mini_results1a <- mikRopML::run_ml(otu_mini,
                                      "regLogistic",
                                      outcome_colname = "dx",
                                      outcome_value = "cancer",
                                      hyperparameters = get_hyperparams_from_df(test_hyperparams, "regLogistic"),
                                      find_feature_importance = TRUE,
                                      seed = 2019,
                                      kfold = 2,
                                      cv_times = 5
)
usethis::use_data(otu_mini_results1a, overwrite = TRUE)

set.seed(0)
group <- sample(LETTERS[1:10], nrow(otu_mini), replace = TRUE)
otu_mini_results1_grp <- mikRopML::run_ml(otu_mini, # use built-in hyperparams
  "regLogistic",
  outcome_colname = "dx",
  outcome_value = "cancer",
  find_feature_importance = FALSE,
  seed = 2019,
  kfold = 2,
  cv_times = 2,
  group = group
)
usethis::use_data(otu_mini_results1_grp, overwrite = TRUE)

# use built-in hyperparams function for this one
otu_mini_results2 <- mikRopML::run_ml(otu_mini,
  "rf",
  outcome_colname = "dx",
  outcome_value = "cancer",
  find_feature_importance = FALSE,
  seed = 2019,
  kfold = 2,
  cv_times = 5
)
usethis::use_data(otu_mini_results2, overwrite = TRUE)

otu_mini_results3 <- mikRopML::run_ml(otu_mini,
  "svmRadial",
  outcome_colname = "dx",
  outcome_value = "cancer",
  hyperparameters = get_hyperparams_from_df(test_hyperparams, "svmRadial"),
  find_feature_importance = FALSE,
  seed = 2019,
  kfold = 2,
  cv_times = 5
)
usethis::use_data(otu_mini_results3, overwrite = TRUE)

otu_mini_results4 <- mikRopML::run_ml(otu_mini,
  "xgbTree",
  outcome_colname = "dx",
  outcome_value = "cancer",
  hyperparameters = get_hyperparams_from_df(test_hyperparams, "xgbTree"),
  find_feature_importance = FALSE,
  seed = 2019,
  kfold = 2,
  cv_times = 5
)
usethis::use_data(otu_mini_results4, overwrite = TRUE)

otu_mini_results5 <- mikRopML::run_ml(otu_mini,
  "rpart2",
  outcome_colname = "dx",
  outcome_value = "cancer",
  find_feature_importance = FALSE,
  seed = 2019,
  kfold = 2,
  cv_times = 5
)
usethis::use_data(otu_mini_results5, overwrite = TRUE)
