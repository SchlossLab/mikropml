# Code written by Zena Lapp
#' Calculate auroc and auprc

library(PRROC)

get_pred <- function(trained_model, test_data, fewer_samples){
  # get predictions
  pred <- predict(trained_model, test_data, type="prob")[[fewer_samples]]
}

get_bin_outcome <- function(outcome_vec, fewer_samples){
  # get binary outcome to calculate aucs (1 is outcome with fewer samples, 0 is outcome with more samples)
  bin_outcome <- ifelse(outcome_vec == fewer_samples, 1, 0)
}

calc_auroc <- function(pred,bin_outcome){
  # calculate auroc
  auroc <- roc.curve(pred, weights.class0 = bin_outcome)$auc
}

calc_auprc <- function(pred,bin_outcome){
  # calculate auprc
  auprc <- pr.curve(pred, weights.class0 = bin_outcome)$auc.integral
}

# wrapper function to calculate aucs
calc_aucs <- function(trained_model, test_data, outcome, fewer_samples){
  pred <- get_pred(trained_model, test_data, fewer_samples)
  outcome_vec <- test_data[,outcome]
  bin_outcome <- get_bin_outcome(outcome_vec, fewer_samples)
  auroc <- calc_auroc(pred, bin_outcome)
  auprc <- calc_auprc(pred, bin_outcome)
  return(list(auroc=auroc, auprc=auprc))
}


