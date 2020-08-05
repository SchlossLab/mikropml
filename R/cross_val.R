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
#' define_cv(train_data_sm, "dx", nfolds = 5, seed - 2019)
define_cv <- function(train_data, outcome_colname, nfolds = 5, seed = NA) {
  if (!is.na(seed)) {
    set.seed(seed)
  }
  cvIndex <- caret::createMultiFolds(factor(train_data[, outcome_colname]),
    nfolds,
    times = 100
  )
  cv <- caret::trainControl(
    method = "repeatedcv",
    number = nfolds,
    index = cvIndex,
    returnResamp = "final",
    classProbs = TRUE,
    summaryFunction = caret::twoClassSummary,
    indexFinal = NULL,
    savePredictions = TRUE
  )
  return(cv)
}
