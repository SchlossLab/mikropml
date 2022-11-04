#' Train model using [caret::train()].
#'
#' @param train_data Training data. Expected to be a subset of the full dataset.
#' @param cv Cross-validation caret scheme from `define_cv()`.
#' @param tune_grid Tuning grid from `get_tuning_grid()`.#'
#' @inheritParams run_ml
#'
#' @return Trained model from [caret::train()].
#'
#' @export
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' \dontrun{
#' training_data <- otu_mini_bin_results_glmnet$trained_model$trainingData %>%
#'   dplyr::rename(dx = .outcome)
#' method <- "rf"
#' hyperparameters <- get_hyperparams_list(otu_mini_bin, method)
#' cross_val <- define_cv(training_data,
#'   "dx",
#'   hyperparameters,
#'   perf_metric_function = caret::multiClassSummary,
#'   class_probs = TRUE,
#'   cv_times = 2
#' )
#' tune_grid <- get_tuning_grid(hyperparameters, method)
#'
#' rf_model <- train_model(
#'   training_data,
#'   "dx",
#'   method,
#'   cross_val,
#'   "AUC",
#'   tune_grid,
#'   ntree = 1000
#' )
#' rf_model$results %>% dplyr::select(mtry, AUC, prAUC)
#' }
train_model <- function(train_data,
                        outcome_colname,
                        method,
                        cv,
                        perf_metric_name,
                        tune_grid,
                        ...) {
  withCallingHandlers(
    {
      if (startsWith(method, "svm")) {
        # https://github.com/topepo/caret/issues/809#issuecomment-875038420
        model_formula <- stats::as.formula(paste(outcome_colname, "~ ."))
        trained_model_caret <- caret::train(
          form = model_formula,
          data = train_data,
          method = method,
          metric = perf_metric_name,
          trControl = cv,
          tuneGrid = tune_grid,
          ...
        )
      } else {
        features_train <- train_data %>%
            dplyr::select(-dplyr::all_of(outcome_colname))
        outcomes_train <-
          train_data %>% dplyr::pull(outcome_colname)
        if (is.character(outcomes_train)) {
          outcomes_train <- outcomes_train %>% as.factor()
        }
        trained_model_caret <- caret::train(
          x = features_train,
          y = outcomes_train,
          method = method,
          metric = perf_metric_name,
          trControl = cv,
          tuneGrid = tune_grid,
          ...
        )
      }
    },
    warning = function(w) {
      if (conditionMessage(w) == "There were missing values in resampled performance measures.") {
        warning(
          "`caret::train()` issued the following warning:\n \n",
          w,
          "\n",
          "This warning usually means that the model didn't converge in some cross-validation folds ",
          "because it is predicting something close to a constant. ",
          "As a result, certain performance metrics can't be calculated. ",
          "This suggests that some of the hyperparameters chosen are doing very poorly."
        )
        invokeRestart("muffleWarning")
      }
    }
  )
  return(trained_model_caret)
}
