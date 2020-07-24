#' Define Cross-Validation Scheme and Training Parameters
#'
#' @param train_data Dataframe for training model
#' @param outcome_colname Column name of the outcome variable
#' @param nfolds fold number for cross-validation
#'
#' @return Caret object for trainControl that controls cross-validation
#' @export
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#'
#'
#' @examples
#' define_cv(train_data_sm, "dx", nfolds = 5)
define_cv <- function(train_data, outcome_colname, nfolds = 5) {
  # -------------------------CV method definition--------------------------------------->
  # ADDED cv index to make sure
  #     1. the internal 5-folds are stratified for diagnosis classes
  #     2. Resample the dataset 100 times for 5-fold cv to get robust tuning
  # IN trainControl function:
  #     1. Train the model with final hp decision to use model to predict
  #     2. Return 2class summary and save predictions to calculate cvROC
  #     3. Save the predictions and class probabilities/decision values.
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
