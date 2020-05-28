# Code written by Zena Lapp
#' Calculate auroc and auprc
calc_aucs <- function(pred,outcome){
  bin_outcome <- ifelse(outcome == names(pred)[1], 1, 0)
  auroc <- roc.curve(pred[[1]], weights.class0 = bin_outcome)$auc
  auprc <- pr.curve(pred[[1]], weights.class0 = bin_outcome)$auc.integral
  return(list(auroc=auroc,auprc=auprc))
}

