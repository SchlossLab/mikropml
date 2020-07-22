#' Generate the Tuning Grid for Tuning Hyperparameters
#'
#' @param method method name (regLogistic, svmRadial, rpart2, rf, xgbTree)
#' @param hyperparamrs Dataframe or named list of lists of hyperparameters
#'
#' @return The tuning grid
#' @export
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#'
#'
#' @examples
#' generate_tuning_grid("regLogistic", default_hyperparams)
generate_tuning_grid <- function(method, hyperparams) {

  # TODO: named list instead of if/else block? could probably use a quosure to delay evaluation
  if (method == "regLogistic") {
    grid <- expand.grid(cost = hyperparams$cost,
                        loss = "L2_primal", # This chooses type=0 for liblinear R package
                        epsilon = 0.01  # default epsilon recommended from liblinear
                        )
  }
  else if (method == "svmRadial") {
    grid <- expand.grid(
      sigma = hyperparams$sigma,
      C = hyperparams$C
    )
  }
  else if (method == "rpart2") {
    grid <- expand.grid(maxdepth = hyperparams$maxdepth)
  }
  else if (method == "rf") {
    grid <- expand.grid(mtry = hyperparams$mtry)
  }
  else if (method == "xgbTree") {
    grid <- expand.grid(
      nrounds = hyperparams$nrounds,
      gamma = hyperparams$gamma,
      eta = hyperparams$eta,
      max_depth = hyperparams$max_depth,
      colsample_bytree = hyperparams$colsample_bytree,
      min_child_weight = hyperparams$min_child_weight,
      subsample = hyperparams$subsample
    )
  }
  else {
    stop(paste0("Method '", method, "' is not supported."))
  }
  return(grid)
}

#' Filter hyperparameters by method and split into lists for each parameter
#'
#' @param method  method (regLogistic, svmRadial, rpart2, rf, xgbTree)
#' @param hyperparameters dataframe with columns param, val, and model
#'
#' @return hyperparams list of lists
#' @export
#'
#' @examples
get_method_hyperparams <- function(method, hyperparams_df) {
  hyperparams <- hyperparams_df[hyperparams_df$method == method, ]
  return(split(hyperparams$val, hyperparams$param))
}
