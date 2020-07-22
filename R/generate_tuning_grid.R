#' Generate the Tuning Grid for Tuning Hyperparameters
#'
#' @param method method name (regLogistic, svmRadial, rpart2, rf, xgbTree)
#' @param hyperparameters Dataframe with hyperparameters. This dataframe should have the first column named as "param" which is the hyperparameter name and the second column "val" which are the values to be tested and third column "model" which is the model being used.
#'
#' @return The tuning grid
#' @export
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#'
#'
#' @examples
#' generate_tuning_grid("L2_Logistic_Regression", default_hyperparams)
generate_tuning_grid <- function(method, hyperparameters) {
  hyperparameters <- hyperparameters[hyperparameters$model == method, ]
  hyperparameters <- split(hyperparameters$val, hyperparameters$param)

  # TODO: named list instead of if/else block? could probably use a quosure to delay evaluation
  if (method == "regLogistic") {
    grid <- expand.grid(
      cost = hyperparameters$cost,
      loss = "L2_primal",
      # This chooses type=0 for liblinear R package
      # which is logistic loss, primal solve for L2 regularized logistic regression
      epsilon = 0.01
    ) # default epsilon recommended from liblinear
  }
  else if (method == "svmRadial") {
    grid <- expand.grid(
      sigma = hyperparameters$sigma,
      C = hyperparameters$C
    )
  }
  else if (method == "rpart2") {
    grid <- expand.grid(maxdepth = hyperparameters$maxdepth)
  }
  else if (method == "rf") {
    grid <- expand.grid(mtry = hyperparameters$mtry)
  }
  else if (method == "xgbTree") {
    grid <- expand.grid(
      nrounds = hyperparameters$nrounds,
      gamma = hyperparameters$gamma,
      eta = hyperparameters$eta,
      max_depth = hyperparameters$max_depth,
      colsample_bytree = hyperparameters$colsample_bytree,
      min_child_weight = hyperparameters$min_child_weight,
      subsample = hyperparameters$subsample
    )
  }
  else {
    message("Method not available")
  }
  # Return:
  #     1. the hyper-parameter grid to tune
  #     2. the caret function to train with
  return(grid)
}
