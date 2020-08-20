#' Define Cross-Validation Scheme and Training Parameters
#'
#' See \link[caret]{trainControl} for info on how the seed is being set.
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
define_cv <- function(train_data, outcome_colname, kfold = 5, cv_times = 100, seed = NA) {
  if (!is.na(seed)) {
    set.seed(seed, "Mersenne-Twister", normal.kind = "Inversion")
  }
  cvIndex <- caret::createMultiFolds(factor(train_data[, outcome_colname]),
    kfold,
    times = cv_times
  )
  #TODO there is something wrong with the seed calculation
  # SVM radial is giving an error on seeds if we use this
  #seeds <- get_seeds_trainControl(kfold, cv_times, ncol(train_data))

  ncol(train_data) - 1
  cv <- caret::trainControl(
    method = "repeatedcv",
    number = kfold,
    index = cvIndex,
    returnResamp = "final",
    classProbs = TRUE,
    summaryFunction = caret::twoClassSummary,
    indexFinal = NULL,
    savePredictions = TRUE
    #,seeds = seeds
  )
  return(cv)
}

#' Get seeds for caret::trainControl
#'
#' @param ncol_train number of columns in training data
#' @inheritParams run_ml
#' @inheritParams define_cv
#'
#' @return seeds for `caret::trainControl`
#' @export
#'
#' @examples
#' get_seeds_trainControl(5, 100, 60)
get_seeds_trainControl <- function(kfold, cv_times, ncol_train) {
  sample_from <- ncol_train * 1000
  seeds <- vector(mode = "list", length = kfold * cv_times + 1)
  for (i in 1:(kfold * cv_times)) {
    seeds[[i]] <- sample.int(sample_from, ncol_train - 1)
  }
  ## For the last model:
  seeds[[kfold * cv_times + 1]] <- sample.int(sample_from, 1)
  return(seeds)
}
