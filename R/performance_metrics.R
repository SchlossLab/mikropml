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
  }else if (outcome_type %in% c('binary','multiclass')){
    # multi-class classification
    perf_metric_fn <- caret::multiClassSummary
  }else{
      stop(paste0('Outcome type of outcome must be one of: `"continuous"`,`"binary"`,`"multiclass`), but you provided: ',outcome_type))
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
      perf_metric_name <- "AUC"
    }else if(outcome_type == 'multiclass'){
      # multi-class classification
      perf_metric_name <- "logLoss"
    } else {
      stop(paste0('Outcome type of outcome must be one of: `"continuous"`,`"binary"`,`"multiclass`), but you provided: ', outcome_type))
    }
  }
  return(perf_metric_name)
}

#' Get performance metrics for test data
#'
#' @param test_data test data
#' @param trained_model trained model
#' @param class_probs whether to use class probabilities
#' @inheritParams run_ml
#'
#' @return performance metrics
#' @export
#'
#' @examples
#' results <- run_ml(otu_small,'glmnet',kfold = 2, cv_times = 2)
#' calc_perf_metrics(results$test_data, 
#' results$trained_model, 
#' 'dx',
#' multiClassSummary, 
#' class_probs = TRUE)
calc_perf_metrics <- function(test_data, trained_model, outcome_colname, perf_metric_function, class_probs){
  pred_type <- 'raw'
  if(class_probs) pred_type <- 'prob'
  preds <- stats::predict(trained_model, test_data, type = pred_type)
  if(class_probs){
    uniq_obs <- unique(c(test_data %>% dplyr::pull(outcome_colname),as.character(trained_model$pred$obs)))
    obs <- factor(test_data %>% dplyr::pull(outcome_colname),levels = uniq_obs)
    pred_class <- factor(names(preds)[apply(preds, 1, which.max)],levels = uniq_obs)
    perf_met <- perf_metric_function(data.frame(obs=obs,pred=pred_class,preds),lev=uniq_obs)
  }else{
    obs <- test_data %>% dplyr::pull(outcome_colname)
    perf_met <- perf_metric_function(data.frame(obs=obs,pred=preds))
  }
  return(perf_met)
}
