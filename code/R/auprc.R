# Code written by Zena Lapp
#' Calculate auprc
calc_auprc <- function(pred,bin_outcome){
  auprc <- MLmetrics::PRAUC(pred,bin_outcome)
  return(auprc)
}

#' Get binary outcome
get_binary_outcome <- function(outcome_vec, first_outcome){
  max_name <- names(which.max(table(outcome_vec)))
  if(max_name == first_outcome){
    bin_outcome <- ifelse(outcome_vec==first_outcome,0,1)
  }else{
    bin_outcome <- ifelse(outcome_vec==first_outcome,1,0)
  }
  return(bin_outcome)
}
