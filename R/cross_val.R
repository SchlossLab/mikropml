#' Define cross-validation scheme and training parameters
#'
#' @param train_data Dataframe for training model.
#' @inheritParams run_ml
#' @inheritParams get_tuning_grid
#' @inheritParams calc_perf_metrics
#'
#' @return Caret object for trainControl that controls cross-validation
#' @export
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' training_inds <- get_partition_indices(otu_small %>% dplyr::pull("dx"),
#'   training_frac = 0.8,
#'   groups = NULL
#' )
#' train_data <- otu_small[training_inds, ]
#' test_data <- otu_small[-training_inds, ]
#' cv <- define_cv(train_data,
#'   outcome_colname = "dx",
#'   hyperparams_list = get_hyperparams_list(otu_small, "glmnet"),
#'   perf_metric_function = caret::multiClassSummary,
#'   class_probs = TRUE,
#'   kfold = 5
#' )
define_cv <- function(train_data, outcome_colname, hyperparams_list, perf_metric_function, class_probs, kfold = 5, cv_times = 100, groups = NULL) {
  if (is.null(groups)) {
    cvIndex <- caret::createMultiFolds(factor(train_data %>%
      dplyr::pull(outcome_colname)),
    kfold,
    times = cv_times
    )
  } else {
    cvIndex <- create_grouped_k_multifolds(groups,
      kfold = kfold,
      cv_times = cv_times
    )
  }

  seeds <- get_seeds_trainControl(hyperparams_list, kfold, cv_times, ncol(train_data))

  cv <- caret::trainControl(
    method = "repeatedcv",
    number = kfold,
    index = cvIndex,
    returnResamp = "final",
    classProbs = class_probs,
    summaryFunction = perf_metric_function,
    indexFinal = NULL,
    savePredictions = TRUE,
    seeds = seeds
  )
  return(cv)
}

#' Get seeds for `caret::trainControl()`
#'
#' Adapted from \href{https://stackoverflow.com/a/32598959}{this Stack Overflow post}
#' and the \link[caret]{trainControl} documentation.
#'
#' @param ncol_train number of columns in training data
#' @inheritParams run_ml
#' @inheritParams define_cv
#'
#' @return seeds for `caret::trainControl()`
#' @noRd
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' get_seeds_trainControl(get_hyperparams_list(otu_small, "glmnet"), 5, 100, 60)
get_seeds_trainControl <- function(hyperparams_list, kfold, cv_times, ncol_train) {
  seeds <- vector(mode = "list", length = kfold * cv_times + 1)
  sample_from <- ncol_train * 1000
  n_tuning_combos <- hyperparams_list %>%
    sapply(FUN = length) %>%
    prod()
  for (i in 1:(kfold * cv_times)) {
    seeds[[i]] <- sample.int(n = sample_from, size = n_tuning_combos)
  }
  ## For the last model:
  seeds[[kfold * cv_times + 1]] <- sample.int(n = sample_from, size = 1)
  return(seeds)
}


#' Splitting into folds for cross-validation when using groups
#'
#' Like \link[caret]{createMultiFolds} but still splitting by groups using \link[caret]{groupKFold}. Code modified from \link[caret]{createMultiFolds}.
#'
#' @param groups equivalent to y in caret::createMultiFolds
#' @param kfold equivalent to k in caret::createMultiFolds
#' @param cv_times equivalent to cv_times in caret::createMultiFolds
#'
#' @return indices of folds for CV
#' @noRd
#' @author Zena Lapp, {zenalapp@@umich.edu}
#'
#' @examples
#' set.seed(0)
#' groups <- c("A", "B", "A", "B", "C", "C", "A", "A", "D")
#' folds <- create_grouped_k_multifolds(groups, kfold = 2, cv_times = 2)
create_grouped_k_multifolds <- function(groups, kfold = 10, cv_times = 5) {
  # we're not doign anything with survival in caret (i.e. copied from caret, but not useful for us)
  # if (class(groups)[1] == "Surv") {
  #   groups <- groups[, "time"]
  # }
  prettyNums <- paste("Rep", gsub(" ", "0", format(1:cv_times)),
    sep = ""
  )
  for (i in 1:cv_times) {
    tmp <- caret::groupKFold(groups, k = kfold)
    names(tmp) <- paste("Fold", gsub(" ", "0", format(seq(along = tmp))),
      ".", prettyNums[i],
      sep = ""
    )
    out <- if (i == 1) {
      tmp
    } else {
      c(out, tmp)
    }
  }
  sapply_fn <- select_apply("sapply")
  if (any(sapply_fn(out, length) == 0)) {
    stop("Could not split the data into train and validate folds. This could mean you do not have enough samples or groups to perform an ML analysis using the groupsing functionality. Alternatively, you can try another seed, or decrease kfold or cv_times.")
  }
  return(out)
}
