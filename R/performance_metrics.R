#' Get outcome type
#'
#' @param outcomes_vec vector of outcomes
#'
#' @return outcome type
#' @export
#'
#' @examples
#' get_outcome_type(c(1, 2, 1))
#' get_outcome_type(c("a", "b", "b"))
#' get_outcome_type(c("a", "b", "c"))
get_outcome_type <- function(outcomes_vec) {
  if (is.numeric(outcomes_vec)) {
    # regression
    otype <- "continuous"
  } else {
    if (length(unique(outcomes_vec)) == 2) {
      # binary classification
      otype <- "binary"
    } else {
      # multi-class classification
      otype <- "multiclass"
    }
  }
  return(otype)
}


#' Get default performance metric function
#'
#' @param outcome_type type of outcome (one of: `"continuous"`,`"binary"`,`"multiclass"`)
#'
#' @return performance metric function
#' @export
#'
#' @examples
#' get_perf_metric_fn("continuous")
#' get_perf_metric_fn("binary")
#' get_perf_metric_fn("multiclass")
get_perf_metric_fn <- function(outcome_type) {
  if (outcome_type == "continuous") {
    # regression
    perf_metric_fn <- caret::defaultSummary
  } else {
    if (outcome_type == "binary") {
      # binary classification
      perf_metric_fn <- caret::twoClassSummary
    } else if (outcome_type == "multiclass") {
      # multi-class classification
      perf_metric_fn <- caret::multiClassSummary
    } else {
      stop(paste0('Outcome type of outcome must be one of: `"continuous"`,`"binary"`,`"multiclass`), but you provided: ', outcome_type))
    }
  }
  return(perf_metric_fn)
}


#' Get default performance metric name
#'
#' @param outcome_type type of outcome (one of: `"continuous"`,`"binary"`,`"multiclass"`)
#'
#' @return performance metric name
#' @export
#'
#' @examples
#' get_perf_metric_name("continuous")
#' get_perf_metric_name("binary")
#' get_perf_metric_name("multiclass")
get_perf_metric_name <- function(outcome_type) {
  if (outcome_type == "continuous") {
    # regression
    perf_metric_name <- "RMSE"
  } else {
    if (outcome_type == "binary") {
      # binary classification
      perf_metric_name <- "ROC"
    } else if (outcome_type == "multiclass") {
      # multi-class classification
      perf_metric_name <- "logLoss"
    } else {
      stop(paste0('Outcome type of outcome must be one of: `"continuous"`,`"binary"`,`"multiclass`), but you provided: ', outcome_type))
    }
  }
  return(perf_metric_name)
}
