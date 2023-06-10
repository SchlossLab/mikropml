#' Check all params that don't return a value
#'
#' @inheritParams run_ml
#'
#' @keywords internal
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
check_all <- function(dataset, method, permute, kfold, training_frac,
                      perf_metric_function, perf_metric_name, groups,
                      group_partitions, corr_thresh, seed, hyperparameters) {
  check_method(method, hyperparameters)
  check_dataset(dataset)
  check_permute(permute)
  check_kfold(kfold, dataset)
  check_perf_metric_function(perf_metric_function)
  check_perf_metric_name(perf_metric_name)
  check_groups(dataset, groups, kfold)
  check_group_partitions(dataset, groups, group_partitions)
  check_corr_thresh(corr_thresh)
  check_seed(seed)
}

#' Check that the dataset is not empty and has more than 1 column.
#'
#' Errors if there are no rows or fewer than 2 columns.
#'
#' @inheritParams run_ml
#'
#' @keywords internal
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' \dontrun{
#' check_dataset(otu_small)
#' }
check_dataset <- function(dataset) {
  if (!any(class(dataset) == "data.frame")) {
    stop(paste("The dataset must be a `data.frame` or `tibble`, but you supplied:", class(dataset)))
  }
  if (nrow(dataset) == 0) {
    stop("No rows detected in dataset.")
  }
  if (ncol(dataset) <= 1) {
    stop(
      "1 or fewer columns detected in dataset. There should be an outcome column and at least one feature column."
    )
  }
}

#' Check if the method is supported. If not, throws error.
#'
#' @inheritParams run_ml
#'
#' @keywords internal
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' \dontrun{
#' check_method("rf")
#' }
check_method <- function(method, hyperparameters) {
  methods <- c("glmnet", "svmRadial", "rpart2", "rf", "xgbTree")
  if (!(method %in% methods) & is.null(hyperparameters)) {
    stop(paste0(
      "Method '",
      method,
      "' is not officially supported by mikropml. ",
      "This ML method may work in our pipeline, ",
      "but you will have to give your own hyperparameters to `run_ml()`. ",
      "See our vignette on tuning hyperparameters for more information: ",
      "http://www.schlosslab.org/mikropml/articles/tuning.html"
    ))
  }
}

#' Check that permute is a logical
#'
#' @inheritParams run_ml
#'
#' @keywords internal
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' \dontrun{
#' do_permute <- TRUE
#' check_permute(do_permute)
#' }
check_permute <- function(permute) {
  if (!is.logical(permute)) {
    stop(paste0(
      "`permute` must be TRUE or FALSE, but you provided a ",
      class(permute)
    ))
  }
}

#' Check that kfold is an integer of reasonable size
#'
#' @inheritParams run_ml
#'
#' @keywords internal
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' \dontrun{
#' check_kfold(5, otu_small)
#' }
check_kfold <- function(kfold, dataset) {
  not_a_number <- !is.integer(kfold) & !is.numeric(kfold)
  not_an_int <- kfold != as.integer(kfold)
  nfeats <- ncol(dataset) - 1
  out_of_range <- (kfold <= 1) | (kfold > nfeats)
  if (not_a_number | not_an_int | out_of_range) {
    stop(paste0(
      "`kfold` must be an integer between 1 and the number of features in the data.\n",
      "  You provided ", kfold, " folds and your dataset has ", nfeats, " features."
    ))
  }
}

#' Check that the training fraction is between 0 and 1
#'
#' @param frac fraction (numeric)
#'
#' @keywords internal
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' \dontrun{
#' check_training_frac(0.8)
#' }
check_training_frac <- function(frac) {
  if (!is.numeric(frac) | (frac <= 0 | frac >= 1)) {
    stop(paste0(
      "`training_frac` must be a numeric between 0 and 1.\n",
      "    You provided: ", frac
    ))
  } else if (frac < 0.5) {
    warning("`training_frac` is less than 0.5. The training set will be smaller than the testing set.")
  }
}

#' Check the validity of the training indices
#'
#' @param training_inds vector of integers corresponding to samples for the training set
#' @param dataset data frame containing the entire dataset
#'
#' @keywords internal
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' \dontrun{
#' training_indices <- otu_small %>%
#'   nrow() %>%
#'   sample(., size = 160)
#' check_training_indices(training_indices, otu_small)
#' }
check_training_indices <- function(training_inds, dataset) {
  if (!all(is_whole_number(training_inds))) {
    warning(
      "The training indices vector contains non-integer numbers."
    )
  }
  stop_msg <- character(0)
  if (min(training_inds) < 1) {
    stop_msg <- paste0(stop_msg,
      "The training indices vector contains a value less than 1.",
      sep = "\n"
    )
  }
  if (max(training_inds) > nrow(dataset)) {
    stop_msg <- paste0(stop_msg,
      "The training indices vector contains a value that is too large for the number of rows in the dataset.",
      sep = "\n"
    )
  }
  if (length(training_inds) >= nrow(dataset)) {
    stop_msg <- paste0(stop_msg,
      "The training indices vector contains too many values for the size of the dataset.",
      sep = "\n"
    )
  }
  if (length(stop_msg) > 0) {
    stop(stop_msg)
  }
}

#' check that the seed is either NA or a number
#'
#' @param seed random seed
#'
#' @keywords internal
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' \dontrun{
#' check_seed(2019)
#' check_seed(NULL)
#' }
check_seed <- function(seed) {
  if (!is.na(seed) & !is.numeric(seed)) {
    stop(paste0(
      "`seed` must be `NA` or numeric.\n",
      "    You provided: ", seed
    ))
  }
}

#' Check that outcome column exists. Pick outcome column if not specified.
#'
#' @param check_values whether to check the outcome values or just get the column (default:TRUE)
#' @param show_message whether to show which column is being used as the output column (default: TRUE)
#' @inheritParams run_ml
#'
#' @return outcome colname
#' @keywords internal
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' \dontrun{
#' check_outcome_column(otu_small, NULL)
#' check_outcome_column(otu_small, "dx")
#' }
check_outcome_column <- function(dataset, outcome_colname, check_values = TRUE, show_message = TRUE) {
  # If no outcome colname specified, use first column in data
  if (is.null(outcome_colname)) {
    outcome_colname <- colnames(dataset)[1]
  } else {
    # check to see if outcome is in column names of data
    if (!outcome_colname %in% colnames(dataset)) {
      stop(paste0("Outcome '", outcome_colname, "' not in column names of data."))
    }
  }

  if (check_values) check_outcome_value(dataset, outcome_colname)

  if (show_message) {
    message(
      paste0(
        "Using '",
        outcome_colname,
        "' as the outcome column."
      )
    )
  }

  return(outcome_colname)
}

#' Check that the outcome variable is valid. Pick outcome value if necessary.
#'
#' @inheritParams run_ml
#'
#' @return outcome value
#' @keywords internal
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' \dontrun{
#' check_outcome_value(otu_small, "dx", "cancer")
#' }
check_outcome_value <- function(dataset, outcome_colname, pos_outcome = NULL) {
  # check no NA's
  outcomes_vec <- dataset %>% dplyr::pull(outcome_colname)
  num_missing <- sum(is.na(outcomes_vec))
  if (num_missing != 0) {
    stop(paste0("Missing data in the output variable is not allowed, but the outcome variable has ", num_missing, " missing value(s) (NA)."))
  }

  # check for empty strings
  num_empty <- sum(outcomes_vec == "")
  if (num_empty != 0) {
    warning(paste0("Possible missing data in the output variable: ", num_empty, " empty value(s)."))
  }

  # check if continuous outcome
  isnum <- is.numeric(outcomes_vec)
  if (isnum) {
    # check if it might actually be categorical
    if (all(floor(outcomes_vec) == outcomes_vec)) {
      warning("Data is being considered numeric, but all outcome values are integers. If you meant to code your values as categorical, please use character values.")
    }
  }

  # check binary and multiclass outcome
  outcomes <- outcomes_vec %>%
    unique()
  num_outcomes <- length(outcomes)
  if (num_outcomes < 2) {
    stop(
      paste0(
        "A binary or multi-class outcome variable is required, but this dataset has ",
        num_outcomes, " outcome(s): ", paste(outcomes, collapse = ", ")
      )
    )
  }
}

#' Check or set outcome column to be a factor with `pos_class` as the first level
#'
#' @inheritParams run_ml
#'
#' @return dataset, with the outcome column as a factor
#' @keywords internal
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' dat <- data.frame("dx" = c("a", "b", "a", "b", "b", "a"), feat = 1:6)
#' dat %>% set_outcome_factor("dx", "a")
#' dat %>% set_outcome_factor("dx", "b")
set_outcome_factor <- function(dataset, outcome_colname, pos_class) {
  relevel_outcome <- FALSE
  outcomes_vctr <- dataset %>% dplyr::pull(outcome_colname)
  # make sure it's either a factor or pos_class is set.
  # the first factor level is used as the positive class by caret
  if (!is.factor(outcomes_vctr)) {
    if (is.null(pos_class)) {
      stop(paste0(
        "Either the outcome column `", outcome_colname,
        "` must be a factor with the first factor level being the positive class,\n",
        "or you must specify `pos_class`."
      ))
    }
    relevel_outcome <- TRUE
  } else {
    first_lvl <- levels(outcomes_vctr)[1]
    if (!is.null(pos_class) & pos_class != first_lvl) {
      warning(paste0(
        "`pos_class` is set, but it is not the first level in the outcome column. ",
        "Releveling the outcome column to set ",
        "`pos_class`=", pos_class, " as the first level."
      ))
      relevel_outcome <- TRUE
    }
  }
  if (isTRUE(relevel_outcome)) {
    if (!(pos_class %in% outcomes_vctr)) {
      stop(paste0(
        "pos_class `", pos_class,
        "` not found in outcome column."
      ))
    }
    dataset[outcome_colname] <- factor(outcomes_vctr,
      levels = unique(c(
        pos_class,
        outcomes_vctr
      ))
    )
  }
  return(dataset)
}

#' Check whether package(s) are installed
#'
#' @param ... names of packages to check
#' @return named vector with status of each packages; installed (`TRUE`) or not (`FALSE`)
#' @keywords internal
#' @author Kelly Sovacool \email{sovacool@@umich.edu}
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' \dontrun{
#' check_packages_installed("base")
#' check_packages_installed("not-a-package-name")
#' all(check_packages_installed("parallel", "doFuture"))
#' }
check_packages_installed <- function(...) {
  return(sapply(c(...), requireNamespace, quietly = TRUE))
}


#' Throw error if required packages are not installed.
#'
#' Reports which packages need to be installed and the parent function name.
#' See \url{https://stackoverflow.com/questions/15595478/how-to-get-the-name-of-the-calling-function-inside-the-called-routine}
#' This is only intended to be used inside a function. It will error otherwise.
#'
#' @inheritParams check_packages_installed
#' @keywords internal
#' @author Kelly Sovacool \email{sovacool@@umich.edu}
#'
#' @examples
#' \dontrun{
#' abort_packages_not_installed("base")
#' abort_packages_not_installed("not-a-package-name", "caret", "dplyr", "non_package")
#' }
abort_packages_not_installed <- function(...) {
  package_status <- check_packages_installed(...)
  parent_fcn_name <- sub("\\(.*$", "\\(\\)", deparse(sys.calls()[[sys.nframe() - 1]]))
  packages_not_installed <- Filter(isFALSE, package_status)
  if (length(packages_not_installed) > 0) {
    msg <- paste0(
      "The following package(s) are required for `", parent_fcn_name,
      "` but are not installed: \n  ",
      paste0(names(packages_not_installed), collapse = ", ")
    )
    stop(msg)
  }
}

#' Check features
#'
#' @param features features for machine learning
#' @param check_missing check whether the features have missing data (default: TRUE)
#'
#' @keywords internal
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' \dontrun{
#' check_features(otu_mini_bin[, 2:11])
#' }
check_features <- function(features, check_missing = TRUE) {
  if (!class(features)[1] %in% c("data.frame", "tbl_df")) {
    stop(paste("Argument `features` must be a `data.frame` or `tibble`, but you provided:", class(features)))
  }

  # check for empty strings
  num_empty <- sum(features == "", na.rm = TRUE)
  if (num_empty != 0) {
    warning(paste0("Possible missing data in the features: ", num_empty, " empty value(s)."))
  }

  if (check_missing) {
    # check no NA's
    num_missing <- sum(is.na(features))
    if (num_missing != 0) {
      stop(paste0("Missing data in the features is not allowed, but the features have ", num_missing, " missing value(s) (NA)."))
    }
  }
}

#' Check grouping vector
#'
#' @inheritParams run_ml
#'
#' @keywords internal
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' \dontrun{
#' check_groups(mikropml::otu_mini_bin,
#'   sample(LETTERS, nrow(mikropml::otu_mini_bin), replace = TRUE),
#'   kfold = 2
#' )
#' }
check_groups <- function(dataset, groups, kfold) {
  # check that groups is a vector or NULL
  isvec <- is.vector(groups)
  isnull <- is.null(groups)
  if (!(isvec | isnull)) {
    stop(paste0("group should be either a vector or NULL, but group is class ", class(groups), "."))
  }
  # if group is a vector, check that it's the correct length
  if (isvec) {
    ndat <- nrow(dataset)
    ngrp <- length(groups)
    if (ndat != ngrp) {
      stop(paste0("group should be a vector that is the same length as the number of rows in the dataset (", ndat, "), but it is of length ", ngrp, "."))
    }
    # check that there are no NAs in group
    nas <- is.na(groups)
    nnas <- sum(nas)
    if (any(nas)) {
      stop(paste0("No NA values are allowed in group, but ", nnas, " NA(s) are present."))
    }
    # check that there is more than 1 group
    ngrp <- length(unique(groups))
    if (ngrp < 2) {
      stop(paste0("The total number of groups should be greater than 1. If all samples are from the same group, use `group=NULL`"))
    }
  }
}

#' Check the validity of the group_partitions list
#'
#' @inheritParams check_groups
#' @inheritParams run_ml
#'
#' @keywords internal
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' \dontrun{
#' check_group_partitions(
#'   otu_mini_bin,
#'   sample(LETTERS[1:8],
#'     size = nrow(otu_mini_bin),
#'     replace = TRUE
#'   ),
#'   list(train = c("A", "B"), test = c("C", "D"))
#' )
#' }
check_group_partitions <- function(dataset, groups, group_partitions) {
  if (is.null(group_partitions)) {
    return()
  }
  names_unrecognized <-
    names(group_partitions[!names(group_partitions) %in% c("train", "test")])
  if (length(names_unrecognized) > 0) {
    stop(paste(
      "Unrecognized name(s) in `group_partitions`:",
      paste(names_unrecognized, collapse = " ")
    ))
  }
  groups_unrecognized <- setdiff(
    group_partitions %>% unlist(),
    groups %>% unique()
  ) %>% sort()
  if (length(groups_unrecognized) > 0) {
    stop(paste(
      "`group_partitions` contains group names not in groups vector:",
      paste(groups_unrecognized, collapse = " ")
    ))
  }
}

#' check that corr_thresh is either NULL or a number between 0 and 1
#'
#' @param corr_thresh correlation threshold
#'
#' @keywords internal
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' \dontrun{
#' check_corr_thresh(1)
#' check_corr_thresh(0.8)
#' check_corr_thresh(2019)
#' check_corr_thresh(NULL)
#' }
check_corr_thresh <- function(corr_thresh) {
  err <- paste0(
    "`corr_thresh` must be `NULL` or numeric between 0 and 1 inclusive.\n",
    "    You provided: ", corr_thresh
  )
  if (!is.null(corr_thresh) & !is.numeric(corr_thresh)) {
    stop(err)
  }
  if (is.numeric(corr_thresh)) {
    if (!(corr_thresh >= 0 & corr_thresh <= 1)) {
      stop(err)
    }
  }
}

#' Check perf_metric_function is NULL or a function
#'
#' @param perf_metric_function performance metric function
#'
#' @keywords internal
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' \dontrun{
#' check_perf_metric_function(NULL)
#' }
check_perf_metric_function <- function(perf_metric_function) {
  if (!is.function(perf_metric_function) & !is.null(perf_metric_function)) {
    stop(paste0("`perf_metric_function` must be `NULL` or a function.\n    You provided: ", class(perf_metric_function)))
  }
}

#' Check perf_metric_name is NULL or a function
#'
#' @param perf_metric_name performance metric function
#'
#' @keywords internal
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' \dontrun{
#' check_perf_metric_name(NULL)
#' }
check_perf_metric_name <- function(perf_metric_name) {
  if (!is.character(perf_metric_name) & !is.null(perf_metric_name)) {
    stop(paste0("`perf_metric_name` must be `NULL` or a character\n    You provided: ", perf_metric_name))
  }
}

#' Check if any features are categorical
#'
#' @param feats features
#'
#' @keywords internal
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' \dontrun{
#' check_cat_feats(otu_mini_bin)
#' }
check_cat_feats <- function(feats) {
  if (any(sapply(feats, class) %in% c("factor", "character"))) {
    stop("No categorical features can be used when performing permutation importance. Please change these features to numeric. One option is to use `preprocess_data`.")
  }
}

#' Check remove_var
#'
#' @inheritParams preprocess_data
#'
#' @keywords internal
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' \dontrun{
#' check_remove_var(NULL)
#' }
check_remove_var <- function(remove_var) {
  if (!is.null(remove_var)) {
    if (!(remove_var %in% c("nzv", "zv"))) {
      stop(paste0("`remove_var` must be one of: NULL, 'nzv','zv'. You provided: ", remove_var))
    }
  }
}

#' Check ntree
#'
#' @inheritParams run_ml
#'
#' @keywords internal
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' \dontrun{
#' check_ntree(NULL)
#' }
check_ntree <- function(ntree) {
  .Deprecated("The `ntree` parameter is no longer checked.")
  if (!is.null(ntree)) {
    if (!is.numeric(ntree) | length(ntree) > 1) {
      stop(paste0("`ntree` must be of length 1 and class numeric. You provided: ", class(ntree)))
    } else if (ntree < 1) {
      stop(paste0("`ntree` must be greater than zero. You provided: ", ntree))
    }
  }
}
