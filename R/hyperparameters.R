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
#' get_tuning_grid(default_hyperparams, "regLogistic")
get_tuning_grid <- function(hyperparams_list, method) {
  return(hyperparams_list %>%
    expand.grid() %>%
    mutate_all_types())
}

#' Split hyperparameters dataframe into named lists for each parameter
#'
#' @param hyperparams_df dataframe of hyperparameters
#'
#' @return named list of lists of hyperparameters
#' @export
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' get_hyperparams_list(default_hyperparams)
get_hyperparams_list <- function(hyperparams_df) {
  return(split(hyperparams_df$value, hyperparams_df$param))
}

#' Check loss & epsilon hyperparameters for L2-normalized logistic regression
#'
#' @param hp_list named list of hyperparameters
#'
#' @noRd
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' # no warning message if defaults are used
#' set_hparams_regLogistic() %>%
#'   check_l2logit_hyperparams()
check_l2logit_hyperparams <- function(hp_list) {
  l2logit_required <- list(epsilon = c(0.01),
                           loss = c('L2_primal'))
  logit_given <- hp_list[names(hp_list) %in% names(l2logit_required)]

  if (!isTRUE(all.equal.list(l2logit_required, logit_given))) {
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

#' Set hyperparameters based on ML method and dataset characteristics
#'
#' @inheritParams run_ml
#'
#' @return list of hyperparameters
#' @export
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' get_hyperparams_list(otu_mini, 'rf')
#' get_hyperparams_list(otu_medium, 'rf')
#' get_hyperparams_list(otu_mini, 'rpart2')
#' get_hyperparams_list(otu_medium, 'rpart2)
get_hyperparams_list <- function(dataset, method) {
   n_features <- ncol(dataset) - 1
   n_samples <- nrow(dataset)
   hparams_functions <- list(regLogistic = rlang::quo(set_hparams_regLogistic()),
                             rf          = rlang::quo(set_hparams_rf(n_features)),
                             rpart2      = rlang::quo(set_hparams_rpart2(n_samples)),
                             svmRadial   = rlang::quo(set_hparams_svmRadial()),
                             xgbTree     = rlang::quo(set_hparams_xgbTree(n_samples))
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
  return(list(cost = 10^seq(-2, 1, 1),
              epsilon = c(0.01),
              loss = c('L2_primal')
       ))
}

#' Set hyparameters for random forest models
#'
#' @param n_features number of features in the dataset
#'
#' @return named list of hyperparameters
#' @export
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' set_hparams_rf(16)
#' set_hparams_rf(2000)
#' set_hparams_rf(1)
set_hparams_rf <- function(n_features) {
  sqrt_features <- round(sqrt(n_features))
  if (n_features == 1) {
    mtry <-  c(1)
  } else {
    mtry <-  c(sqrt_features / 2,
               sqrt_features,
               sqrt_features * 2) %>%
      round() %>%
      subset(. >= 1 & . < n_features)
  }
  return(list(mtry = mtry))
}

#' Set hyperparameters for decision tree models
#'
#' @param n_samples number of samples in the dataset
#'
#' @return named list of hyperparameters
#' @export
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' set_hparams_rpart2(100)
#' set_hparams_rpart2(20)
set_hparams_rpart2 <- function(n_samples) {
  return(list(maxdepth = c(1, 2, 4, 8, 16, 30) %>%
                subset(. < n_samples)))
}

#' Set hyperparameters for SVM with radial kernel
#'
#' @return named list of hyperparameters
#' @export
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' set_hparams_svmRadial()
set_hparams_svmRadial <- function() {
  return(list(C = 10^seq(-3, 2, 1),
              sigma = 10^seq(-6, -1, 1)))
}

#' Set hyperparameters for SVM with radial kernel
#'
#' @inheritParams set_hparams_rpart2
#'
#' @return named list of hyperparameters
#' @export
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' set_hparams_xgbTree()
set_hparams_xgbTree <- function(n_samples) {
  return(list(nrounds =	c(100),
              gamma	= c(0),
              eta	= 10^seq(-3, 0, 1),
              max_depth =	set_hparams_rpart2(n_samples)[['maxdepth']],
              colsample_bytree = c(0.8),
              min_child_weight = c(1),
              subsample	= seq(4, 7, 1) / 10
              ))
}

