#' Define Cross-Validation Scheme and Training Parameters
#'
#' @param train_data Dataframe for training model
#' @inheritParams run_ml
#'
#' @return Caret object for trainControl that controls cross-validation
#' @export
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#'
#'
#' @examples
#' define_cv(train_data_sm, "dx", kfold = 5, seed = 2019)
define_cv <- function(train_data, outcome_colname, kfold = 5, seed = NA) {
  if (!is.na(seed)) {
    set.seed(seed)
  }
  cvIndex <- caret::createMultiFolds(factor(train_data[, outcome_colname]),
    kfold,
    times = 100
  )
  
  seeds <- vector(mode = "list", length = kfold*100+1)
  for(i in 1:(kfold*100)) seeds[[i]] <- sample.int(1000, ncol(train_data)-1)
  ## For the last model:
  seeds[[kfold*100+1]] <- sample.int(1000, 1)
  
  ncol(train_data) -1
  cv <- caret::trainControl(
    method = "repeatedcv",
    number = kfold,
    index = cvIndex,
    returnResamp = "final",
    classProbs = TRUE,
    summaryFunction = caret::twoClassSummary,
    indexFinal = NULL,
    savePredictions = TRUE,
    seeds = seeds
  )
  return(cv)
}
