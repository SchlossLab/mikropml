#' Define Cross-Validation Scheme and Training Parameters
#'
#' See \link[caret]{trainControl} for info on how the seed is being set.
#'
#' @param train_data Dataframe for training model
#' @inheritParams run_ml
#' @inheritParams get_tuning_grid
#'
#' @return Caret object for trainControl that controls cross-validation
#' @export
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#'
#'
#' @examples
#' hparams_list <- default_hyperparams %>%
#'   check_hyperparams_df('regLogistic') %>%
#'   get_hyperparams_list()
#' define_cv(train_data_sm, "dx", hparams_list,  kfold = 5, seed = 2019)
define_cv <- function(train_data, outcome_colname, hyperparams_list, kfold = 5, cv_times = 100, seed = NA) {
  if (!is.na(seed)) {
    set.seed(seed, "Mersenne-Twister", normal.kind = "Inversion")
  }
  cvIndex <- caret::createMultiFolds(factor(train_data[, outcome_colname]),
    kfold,
    times = cv_times
  )
  seeds <- get_seeds_trainControl(hyperparams_list, kfold, cv_times, ncol(train_data))

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

#' Get seeds for caret::trainControl
#'
#' Adapted from \href{https://stackoverflow.com/a/32598959}{this Stack Overflow post}
#' and the \link[caret]{trainControl} documentation
#'
#' @param ncol_train number of columns in training data
#' @inheritParams run_ml
#' @inheritParams define_cv
#'
#' @return seeds for `caret::trainControl`
#' @export
#'
#' @examples
#' hparams_list <- default_hyperparams %>%
#'   check_hyperparams_df('regLogistic') %>%
#'   get_hyperparams_list()
#' get_seeds_trainControl(hparams_list, 5, 100, 60)
get_seeds_trainControl <- function(hyperparams_list, kfold, cv_times, ncol_train) {
  seeds <- vector(mode = "list", length = kfold * cv_times + 1)
  sample_from <- ncol_train * 1000
  n_tuning_combos <- hyperparams_list %>% sapply(FUN = length) %>% prod()
  for (i in 1:(kfold * cv_times)) {
    seeds[[i]] <- sample.int(n = sample_from, size = n_tuning_combos)
  }
  ## For the last model:
  seeds[[kfold * cv_times + 1]] <- sample.int(n = sample_from, size = 1)
  return(seeds)
}
