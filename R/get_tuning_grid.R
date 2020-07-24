#' Generate the Tuning Grid for Tuning Hyperparameters
#'
#' @param hyperparams_df Dataframe with columns `param` and `value`
#'
#' @return The tuning grid
#' @export
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' mikRopML:::validate_hyperparams_df(default_hyperparams, "regLogistic") %>%
#'   get_tuning_grid()
get_tuning_grid <- function(hyperparams_df) {
  return(get_hyperparams_list(hyperparams_df) %>% expand.grid())
}

#' Split hyperparameters dataframe into named lists for each parameter
#'
#' @param hyperparams_df dataframe with columns `param` and `value`
#'
#' @return named list of lists of hyperparameters
#' @noRd
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' get_hyperparams_list(default_hyperparams)
get_hyperparams_list <- function(hyperparams_df) {
  return(split(hyperparams_df$value, hyperparams_df$param))
}

#' Ensure the hyperparameters dataframe has a valid format
#'
#' @param hyperparameters dataframe containing columns `param` and `value`
#' @param method_name method name (regLogistic, svmRadial, rpart2, rf, xgbTree)
#'
#' @return hyperparams df, filtered by method if needed
#' @noRd
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' validate_hyperparams_df(default_hyperparams, "regLogistic")
validate_hyperparams_df <- function(hyperparams_df, method_name) {
  df_error_msg <-
    "`hyperparameters` must be a dataframe with columns `param` and `value`"
  if (!any(class(hyperparams_df) == "data.frame")) {
    stop(paste0(
      df_error_msg,
      "\n  You supplied: ",
      paste(class(hyperparams_df), collapse = " ")
    ))
  } else if (sum(names(hyperparams_df) %in% c("param", "value")) < 2 |
    any(!(names(hyperparams_df) %in% c("param", "value", "method")))) {
    stop(paste0(
      df_error_msg,
      "\n  You supplied: ",
      paste(names(hyperparams_df), collapse = " ")
    ))
  }

  if ("method" %in% names(hyperparams_df)) {
    hyperparams_df <- hyperparams_df %>%
      dplyr::filter(.data$method == method_name) %>%
      dplyr::select(.data$param, .data$value)
  }

  # warn if regLogistic hyperparameters aren't for L2-normalization
  if (method_name == "regLogistic") {
    check_l2logit_hyperparams(hyperparams_df)
  }

  return(hyperparams_df)
}


#' Check loss & epsilon hyperparameters for L2-normalized logistic regression
#'
#' @param hyperparams_df data.frame with columns `param` and `value`
#'
#' @noRd
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' # no warning message if defaults are used
#' default_hyperparams %>%
#'   dplyr::filter(method == "regLogistic") %>%
#'   check_l2logit_hyperparams()
check_l2logit_hyperparams <- function(hyperparams_df) {
  l2logit_required <- dplyr::tibble(
    param = c("loss", "epsilon"),
    value = c("L2_primal", "0.01")
  )
  logit_given <- hyperparams_df %>%
    dplyr::filter(.data$param %in% c("loss", "epsilon")) %>%
    dplyr::select(.data$param, .data$value)

  if (!isTRUE(dplyr::all_equal(l2logit_required, logit_given))) {
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
