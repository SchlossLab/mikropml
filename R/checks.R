
#' Check all params that don't return a value
#'
#' @inheritParams run_ml
#'
#' @noRd
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' check_all(otu_small, "regLogistic", TRUE, as.integer(5), 0.8, NULL)
check_all <- function(dataset, method, permute, kfold, training_frac, group, corr_thresh, seed) {
  check_method(method)
  check_dataset(dataset)
  check_permute(permute)
  check_kfold(kfold, dataset)
  check_training_frac(training_frac)
  check_group(dataset, group, kfold)
  check_corr_thresh(corr_thresh)
  check_seed(seed)
}

#' Check that the dataset is not empty and has more than 1 column.
#'
#' Errors if there are no rows or fewer than 2 columns.
#'
#' @inheritParams run_ml
#'
#' @noRd
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' check_dataset(otu_small)
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
#' @noRd
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' check_method("regLogistic")
check_method <- function(method) {
  methods <- c("regLogistic", "svmRadial", "rpart2", "rf", "xgbTree")
  if (!(method %in% methods)) {
    stop(paste0(
      "Method '",
      method,
      "' is not supported. Supported methods are:\n    ",
      paste(methods, collapse = ", ")
    ))
  }
}

#' Check that permute is a logical
#'
#' @inheritParams run_ml
#'
#' @noRd
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' do_permute <- TRUE
#' check_permute(do_permute)
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
#' @noRd
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' check_kfold(5, otu_small)
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
#' @noRd
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' check_training_frac(0.8)
check_training_frac <- function(frac) {
  if (!is.numeric(frac) | (frac <= 0 | frac >= 1)) {
    stop(paste0(
      "`training_frac` must be a numeric between 0 and 1.\n",
      "    You provided: ", frac
    ))
  }
}

#' check that the seed is either NULL or a number
#'
#' @param seed random seed
#'
#' @noRd
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' check_seed(2019)
#' check_seed(NULL)
check_seed <- function(seed) {
  if (!is.null(seed) & !is.numeric(seed)) {
    stop(paste0(
      "`seed` must be `NULL` or numeric.\n",
      "    You provided: ", seed
    ))
  }
}

#' Check that outcome column exists. Pick outcome column if not specified.
#'
#' @inheritParams run_ml
#'
#' @return outcome colname
#' @noRd
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' check_outcome_column(otu_small, NULL)
#' check_outcome_column(otu_small, "dx")
check_outcome_column <- function(dataset, outcome_colname) {
  # If no outcome colname specified, use first column in data
  if (is.null(outcome_colname)) {
    outcome_colname <- colnames(dataset)[1]
  } else {
    # check to see if outcome is in column names of data
    if (!outcome_colname %in% colnames(dataset)) {
      stop(paste0("Outcome '", outcome_colname, "' not in column names of data."))
    }
  }
  return(outcome_colname)
}

#' Check that the outcome variable is binary. Pick outcome value if necessary.
#'
#' @inheritParams run_ml
#' @inheritParams pick_outcome_value
#'
#' @return outcome value
#' @noRd
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' check_outcome_value(otu_small, "dx", "cancer")
check_outcome_value <- function(dataset, outcome_colname, outcome_value, method = "fewer") {
  # check no NA's
  num_missing <- sum(is.na(dataset[, outcome_colname]))
  if (num_missing != 0) {
    stop(paste0("Missing data in the output variable is not allowed, but the outcome variable has ", num_missing, " missing value(s) (NA)."))
  }

  # check for empty strings
  num_empty <- sum(dataset[, outcome_colname] == "")
  if (num_empty != 0) {
    warning(paste0("Possible missing data in the output variable: ", num_empty, " empty value(s)."))
  }

  # check binary outcome
  outcomes <- unique(dataset[, outcome_colname])
  num_outcomes <- length(outcomes)
  if (num_outcomes != 2) {
    stop(
      paste0(
        "A binary outcome variable is required, but this dataset has ",
        num_outcomes,  " outcomes: ", paste(outcomes, collapse = ",")
      )
    )
  }
  # pick binary outcome value of interest if not provided by user
  if (is.null(outcome_value)) {
    outcome_value <-
      pick_outcome_value(dataset, outcome_colname, method = method)
  } else if (!any(dataset[, outcome_colname] == outcome_value)) {
    stop(
      paste0(
        "No rows in the outcome column (",
        outcome_colname,
        ") with the outcome of interest (",
        outcome_value,
        ") were detected."
      )
    )
  }
  message(
    paste0(
      "Using '",
      outcome_colname,
      "' as the outcome column and '",
      outcome_value,
      "' as the outcome value of interest."
    )
  )
  return(outcome_value)
}

#' Check whether package(s) are installed
#'
#' @param package_names names of packages (or a single package) to check
#' @return logical vector - whether packages are installed (TRUE) or not (FALSE).
#' @noRd
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' check_package_installed("base")
#' check_package_installed("asdf")
#' all(check_package_installed(c("parallel", "doParallel")))
check_package_installed <- function(package_names) {
  return(package_names %in% rownames(utils::installed.packages()))
}

check_features <- function(features, check_missing = TRUE) {
  if (!class(features)[1] %in% c("data.frame", "tbl_df")) {
    stop("Argument `features` must be a `data.frame` or `tibble`")
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

#' Check group
#'
#' @inheritParams run_ml
#'
#' @return
#' @export
#'
#' @examples
#' check_group(mikRopML::otu_mini,
#'             sample(LETTERS, nrow(mikRopML::otu_mini), replace = TRUE),
#'             kfold = 2)
check_group <- function(dataset, group, kfold) {
  # check that group is a vector or NULL
  isvec <- is.vector(group)
  isnull <- is.null(group)
  if (!(isvec | isnull)) {
    stop(paste0("group should be either a vector or NULL, but group is class ", class(group), "."))
  }
  # if group is a vector, check that it's the correct length
  if (isvec) {
    ndat <- nrow(dataset)
    ngrp <- length(group)
    if (ndat != ngrp) {
      stop(paste0("group should be a vector that is the same length as the number of rows in the dataset (", ndat, "), but it is of length ", ngrp, "."))
    }
    # check that there are no NAs in group
    nas <- is.na(group)
    nnas <- sum(nas)
    if (any(nas)) {
      stop(paste0("No NA values are allowed in group, but ", nnas, " NA(s) are present."))
    }
    # check that there is more than 1 group
    ngrp <- length(unique(group))
    if (ngrp < 2) {
      stop(paste0("The total number of groups should be greater than 1. If all samples are from the same group, use `group=NULL`"))
    }
    # check that kfold is not greater than the number of groups minus 1 (assuming 1 in the test set)
    if (kfold > (ngrp - 1)) {
      stop(paste0("The number of folds for cross-validation, `k-fold`, must be less than the number of groups. Number of groups: ", ngrp, ". `kfold`: ", kfold, "."))
    }
  }
}

#' check that corr_thresh is either NULL or a number between 0 and 1
#'
#' @param corr_thresh correlation threshold
#'
#' @noRd
#'
#' @examples
#' check_corr_thresh(1)
#' check_corr_thresh(0.8)
#' check_corr_thresh(2019)
#' check_corr_thresh(NULL)
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
