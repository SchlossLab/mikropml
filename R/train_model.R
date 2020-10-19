# train model

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
  return(trained_model_caret)
}
