# Code written by Zena Lapp
# Calculate auroc and auprc

#' Get predictions from trained model and test data
#'
#' @param trained_model TODO
#' @param test_data TODO
#' @param outcome_samples TODO
#'
#' @return
#' @export
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
get_pred <- function(trained_model, test_data, outcome_samples) {
  pred <- stats::predict(trained_model, test_data, type = "prob")[[outcome_samples]]
}

#' Get binary outcome vector for calculating AUCs
#'
#' @param outcome_vec TODO
#' @param outcome_samples TODO
#'
#' @return
#' @export
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#'
get_bin_outcome <- function(outcome_vec, outcome_samples) {
  # 1 is outcome with fewer samples, 0 is outcome with more samples
  bin_outcome <- ifelse(outcome_vec == outcome_samples, 1, 0)
}

#' Calculate AUROC
#'
#' @param pred TODO
#' @param bin_outcome TODO
#'
#' @return
#' @export
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#'
calc_auroc <- function(pred, bin_outcome) {
  auroc <- PRROC::roc.curve(pred, weights.class0 = bin_outcome)$auc
}

#' Calculate AUPRC
#'
#' @param pred TODO
#' @param bin_outcome TODO
#'
#' @return
#' @export
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#'
calc_auprc <- function(pred, bin_outcome) {
  auprc <- PRROC::pr.curve(pred, weights.class0 = bin_outcome)$auc.integral
}

#' Calculate AUROC and AUPRC
#'
#' @param trained_model TODO
#' @param test_data TODO
#' @param outcome TODO
#' @param outcome_samples TODO
#'
#' @return
#' @export
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#'
calc_aucs <- function(trained_model, test_data, outcome, outcome_samples) {
  pred <- get_pred(trained_model, test_data, outcome_samples)
  outcome_vec <- test_data[, outcome]
  bin_outcome <- get_bin_outcome(outcome_vec, outcome_samples)
  auroc <- calc_auroc(pred, bin_outcome)
  auprc <- calc_auprc(pred, bin_outcome)
  return(list(auroc = auroc, auprc = auprc))
}
