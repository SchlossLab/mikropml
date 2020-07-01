# Code written by Zena Lapp
# Calculate auroc and auprc

#' Title
#'
#' @param trained_model TODO
#' @param test_data TODO
#' @param fewer_samples TODO
#'
#' @return
#' @export
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
get_pred <- function(trained_model, test_data, fewer_samples) {
  # get predictions
  pred <- stats::predict(trained_model, test_data, type = "prob")[[fewer_samples]]
}

#' Title
#'
#' @param outcome_vec TODO
#' @param fewer_samples TODO
#'
#' @return
#' @export
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#'
get_bin_outcome <- function(outcome_vec, fewer_samples) {
  # get binary outcome to calculate aucs (1 is outcome with fewer samples, 0 is outcome with more samples)
  bin_outcome <- ifelse(outcome_vec == fewer_samples, 1, 0)
}

#' Title
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
  # calculate auroc
  auroc <- PRROC::roc.curve(pred, weights.class0 = bin_outcome)$auc
}

#' Title
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
  # calculate auprc
  auprc <- PRROC::pr.curve(pred, weights.class0 = bin_outcome)$auc.integral
}

# wrapper function to calculate aucs
#' Title
#'
#' @param trained_model TODO
#' @param test_data TODO
#' @param outcome TODO
#' @param fewer_samples TODO
#'
#' @return
#' @export
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#'
calc_aucs <- function(trained_model, test_data, outcome, fewer_samples) {
  pred <- get_pred(trained_model, test_data, fewer_samples)
  outcome_vec <- test_data[, outcome]
  bin_outcome <- get_bin_outcome(outcome_vec, fewer_samples)
  auroc <- calc_auroc(pred, bin_outcome)
  auprc <- calc_auprc(pred, bin_outcome)
  return(list(auroc = auroc, auprc = auprc))
}
