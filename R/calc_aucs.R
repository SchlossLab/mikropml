#' Calculate AUROC and AUPRC
#'
#' @param trained_model trained model from caret
#' @param test_data dataframe of testing data
#' @param outcome_colname Column name of the outcome variable
#' @param outcome_value Outcome value of interest
#'
#' @return
#' @export
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#'
calc_aucs <-
  function(trained_model,
           test_data,
           outcome_colname,
           outcome_value) {
    pred <- get_prediction(trained_model, test_data, outcome_value)
    bin_outcomes <- recode_outcome(outcome_colname, outcome_value)
    auroc <- calc_auroc(pred, bin_outcomes)
    auprc <- calc_auprc(pred, bin_outcomes)
    return(list(auroc = auroc, auprc = auprc))
  }

#' Get predictions from trained model and test data
#'
#' @inheritParams calc_aucs
#'
#' @return
#' @export
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
get_prediction <- function(trained_model, test_data, outcome_value) {
  return(stats::predict(trained_model, test_data, type = "prob")[[outcome_value]])
}

#' Recode the outcome column to a binary vector for calculating AUCs
#'
#' @inheritParams calc_aucs
#'
#' @return Outcome column recoded as binary (1 = outcome of interest, 0 = other outcome)
#' @export
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#'
recode_outcome <- function(outcome_colname, outcome_value) {
  outcome_vec <- test_data[, outcome_colname]
  return(ifelse(outcome_vec == outcome_value, 1, 0))
}

#' Calculate AUROC
#'
#' @param pred Predictions of the trained model on the test data
#' @param bin_outcomes Binary outcome vector
#'
#' @return Area under the receiver-operator characteristic curve
#' @export
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#'
calc_auroc <- function(pred, bin_outcomes) {
  return(PRROC::roc.curve(pred, weights.class0 = bin_outcomes)$auc)
}

#' Calculate AUPRC
#'
#' @inheritParams calc_auroc
#'
#' @return Area under the precision-recall curve
#' @export
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
calc_auprc <- function(pred, bin_outcomes) {
  return(PRROC::pr.curve(pred, weights.class0 = bin_outcomes)$auc.integral)
}
