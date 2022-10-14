#' Train model using [caret::train()].
#'
#' @param model_formula Model formula, typically created with `stats::as.formula()`.
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
#'   stats::as.formula(paste("dx", "~ .")),
#'   training_data,
#'   method,
#'   cross_val,
#'   "AUC",
#'   tune_grid,
#'   1000
#' )
#' rf_model$results %>% dplyr::select(mtry, AUC, prAUC)
#' }
train_model <- function(features_dat,
                        outcomes_vctr,
                        method,
                        cv,
                        perf_metric_name,
                        tune_grid,
                        ...) {
    withCallingHandlers({
        trained_model_caret <- caret::train(
            features_dat,
            outcomes_vctr,
            method = method,
            metric = perf_metric_name,
            trControl = cv,
            tuneGrid = tune_grid,
            ...
        )
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
    })
    return(trained_model_caret)
}
