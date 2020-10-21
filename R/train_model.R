#' Train model
#'
#' @param model_formula model formula
#' @param train_data train_data
#' @param method method
#' @param cv cross-validation caret scheme
#' @param tune_grid tuning grid
#'
#' @inheritParams run_ml
#'
#' @return trained model
#' @export
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
train_model <- function(model_formula,
                        train_data,
                        method,
                        cv,
                        perf_metric_name,
                        tune_grid,
                        ntree) {
  withCallingHandlers(
    {
      if (method == "rf") {
        trained_model_caret <- caret::train(
          model_formula,
          data = train_data,
          method = method,
          trControl = cv,
          metric = perf_metric_name,
          tuneGrid = tune_grid,
          ntree = ntree
        )
      } else {
        trained_model_caret <- caret::train(
          model_formula,
          data = train_data,
          method = method,
          trControl = cv,
          metric = perf_metric_name,
          tuneGrid = tune_grid
        )
      }
    },
    warning = function(w) {
      if (conditionMessage(w) == "There were missing values in resampled performance measures.") {
        warning(
          "`caret::train()` issued the following warning:\n \n", w, "\n",
          "This warning usually means that the model didn't converge in some cross-validation folds",
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

#' Train model with warnings
#'
#' @param model_formula model formula
#' @param train_data train_data
#' @param method method
#' @param cv cross-validation caret scheme
#' @param tune_grid tuning grid
#'
#' @inheritParams run_ml
#'
#' @return trained model
#' @noRd
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
train_model_w_warnings <- function(model_formula,
                                   train_data,
                                   method,
                                   cv,
                                   perf_metric_name,
                                   tune_grid,
                                   ntree) {
  return(trained_model_caret)
}
