#' Generate the Tuning Grid for Tuning Hyperparameters
#'
#' @param hyperparams_list named list of lists of hyperparameters
#' @inheritParams run_ml
#'
#' @return The tuning grid
#' @export
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' ml_method <- "regLogistic"
#' hparams_list <- get_hyperparams_list(otu_small, ml_method)
#' get_tuning_grid(hparams_list, ml_method)
get_tuning_grid <- function(hyperparams_list, method) {
  return(hyperparams_list %>%
    expand.grid() %>%
    mutate_all_types())
}

#' Check that hyperparameters are valid
#'
#' Currently, it only checks the loss & epsilon hyperparameters for L2-normalized logistic regression.
#'
#' @param hp_list named list of hyperparameters
#' @inheritParams run_ml
#'
#' @noRd
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' # no warning message if defaults are used
#' set_hparams_regLogistic() %>%
#'   check_hyperparams()
check_hyperparams <- function(hp_list, method) {
  if (method == "regLogistic") {
    l2logit_required1 <- list(
      epsilon = c(0.01),
      loss = c("L2_primal")
    )
    l2logit_required2 <- list(
      loss = c("L2_primal"),
      epsilon = c(0.01)
    )
    logit_given <-
      hp_list[names(hp_list) %in% names(l2logit_required1)]

    # must use !isTRUE intead of isFALSE because `all.equal.list` returns description of differences when not equal
    if (!isTRUE(all.equal.list(l2logit_required1, logit_given)) &
        !isTRUE(all.equal.list(l2logit_required2, logit_given))) {
      warning(
        paste0(
          "For L2-normalized Logistic Regression, ",
          "`loss`` must be 'L2_primal' and `epsilon` must be '0.01',",
          "\n  Be sure you intend to not perform L2-normalization.",
          "\n  You supplied these hyperparameters:\n    ",
          paste0(utils::capture.output(logit_given), collapse = "\n    ")
        )
      )
    } else {
      message("Using L2 normalization for Logistic Regression")
    }
  }
}

#' Split hyperparameters dataframe into named lists for each parameter
#'
#' Using \code{\link{get_hyperparams_list}} is preferred over this function.
#'
#' @param hyperparams_df dataframe of hyperparameters with columns `param`, `value`, and `method`
#' @param ml_method machine learning method
#'
#' @return named list of lists of hyperparameters
#' @export
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' hparams_df <- dplyr::tibble(
#'   param = c("cost", "cost", "loss", "epsilon"),
#'   value = c(1, 0.1, "L2_primal", 0.01),
#'   method = rep("regLogistic", 4)
#' )
#' get_hyperparams_from_df(hparams_df, "regLogistic")
get_hyperparams_from_df <- function(hyperparams_df, ml_method) {
  hyperparams_df_filt <- hyperparams_df %>% dplyr::filter(.data$method == ml_method)
  return(split(hyperparams_df_filt$value, hyperparams_df_filt$param))
}

#' Set hyperparameters based on ML method and dataset characteristics
#'
#' @inheritParams run_ml
#'
#' @return named list of hyperparameters
#' @export
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' get_hyperparams_list(otu_mini, "rf")
#' get_hyperparams_list(otu_medium, "rf")
#' get_hyperparams_list(otu_mini, "rpart2")
#' get_hyperparams_list(otu_medium, "rpart2")
get_hyperparams_list <- function(dataset, method) {
  n_features <- ncol(dataset) - 1
  n_samples <- nrow(dataset)
  hparams_functions <- list(
    regLogistic = rlang::quo(set_hparams_regLogistic()),
    rf = rlang::quo(set_hparams_rf(n_features)),
    rpart2 = rlang::quo(set_hparams_rpart2(n_samples)),
    svmRadial = rlang::quo(set_hparams_svmRadial()),
    xgbTree = rlang::quo(set_hparams_xgbTree(n_samples))
  )
  if (!(method %in% names(hparams_functions))) {
    stop(paste0("method '", method, "' is not supported."))
  }
  return(rlang::eval_tidy(hparams_functions[[method]]))
}

#' Set hyperparameters for logistic regression models
#'
#' @return named list of hyperparameters
#' @export
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' set_hparams_regLogistic()
set_hparams_regLogistic <- function() {
  return(list(
    cost = 10^seq(-4, 1, 1),
    epsilon = c(0.01),
    loss = c("L2_primal")
  ))
}

#' Set hyparameters for random forest models
#'
#' @param n_features number of features in the dataset
#'
#' @return named list of hyperparameters
#' @noRd
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' set_hparams_rf(16)
#' set_hparams_rf(2000)
#' set_hparams_rf(1)
set_hparams_rf <- function(n_features) {
  sqrt_features <- round(sqrt(n_features))
  if (n_features == 1) {
    mtry <- c(1)
  } else {
    mtry <- c(
      sqrt_features / 2,
      sqrt_features,
      sqrt_features * 2
    ) %>%
      round() %>%
      .[. >= 1 & . < n_features]
  }
  return(list(mtry = mtry))
}

#' Set hyperparameters for decision tree models
#'
#' @param n_samples number of samples in the dataset
#'
#' @return named list of hyperparameters
#' @noRd
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' set_hparams_rpart2(100)
#' set_hparams_rpart2(20)
set_hparams_rpart2 <- function(n_samples) {
  return(list(maxdepth = c(1, 2, 4, 8, 16, 30) %>% .[. < n_samples]))
}

#' Set hyperparameters for SVM with radial kernel
#'
#' @return named list of hyperparameters
#' @noRd
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' set_hparams_svmRadial()
set_hparams_svmRadial <- function() {
  return(list(
    C = 10^seq(-3, 2, 1),
    sigma = 10^seq(-6, -1, 1)
  ))
}

#' Set hyperparameters for SVM with radial kernel
#'
#' @inheritParams set_hparams_rpart2
#'
#' @return named list of hyperparameters
#' @noRd
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' set_hparams_xgbTree()
set_hparams_xgbTree <- function(n_samples) {
  return(list(
    nrounds = c(100),
    gamma = c(0),
    eta = 10^seq(-3, 0, 1),
    max_depth = set_hparams_rpart2(n_samples)[["maxdepth"]],
    colsample_bytree = c(0.8),
    min_child_weight = c(1),
    subsample = seq(4, 7, 1) / 10
  ))
}
