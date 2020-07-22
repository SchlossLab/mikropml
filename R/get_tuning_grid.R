#' Generate the Tuning Grid for Tuning Hyperparameters
#'
#' @param method method name (regLogistic, svmRadial, rpart2, rf, xgbTree)
#' @param hyperparams Dataframe or named list of lists of hyperparameters
#'
#' @return The tuning grid
#' @export
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' get_tuning_grid("regLogistic", default_hyperparams)
get_tuning_grid <- function(method, hyperparams) {
  if (all(names(hyperparams) == c("param", "val", "method"))) {
    hyperparams <- get_method_hyperparams(method, hyperparams)
  } # otherwise, assumes hyperparams is a named list of lists of hyperparams
  # TODO: simpler way to specify hyperparameters? esp. since it runs for just one method
  return(expand.grid(hyperparams))
}

#' Filter hyperparameters by method and split into lists for each parameter
#'
#' @param method  method (regLogistic, svmRadial, rpart2, rf, xgbTree)
#' @param hyperparams_df dataframe with columns param, val, & model
#'
#' @return named list of lists of hyperparameters
#' @export
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' get_method_hyperparams("regLogistic", default_hyperparams)
get_method_hyperparams <- function(method, hyperparams_df) {
  hyperparams <- hyperparams_df[hyperparams_df$method == method, ]
  return(split(hyperparams$val, hyperparams$param))
}
