#' Calculate AUROC and AUPRC
#'
#' @param trained_model trained model from caret
#' @inheritParams get_feature_importance
#' @inheritParams run_ml
#'
#' @return List of auroc and auprc values
#' @export
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' calc_aucs(trained_model_sm1, test_data_sm, "dx", "cancer")
calc_aucs <-
  function(trained_model,
           test_data,
           outcome_colname,
           outcome_value) {
    preds <- get_predictions(trained_model, test_data, outcome_value)
    bin_outcomes <- recode_outcome(test_data, outcome_colname, outcome_value)
    return(list(
      auroc = calc_auroc(preds, bin_outcomes),
      auprc = calc_auprc(preds, bin_outcomes)
    ))
  }

#' Get predictions from trained model and test data
#'
#' @inheritParams calc_aucs
#'
#' @return vector of predictions
#' @noRd
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' get_predictions(trained_model_sm1, test_data_sm, "cancer")
get_predictions <- function(trained_model, test_data, outcome_value) {
  return(stats::predict(trained_model, test_data, type = "prob")[[outcome_value]])
}

#' Get the outcome column as a binary vector
#'
#' @inheritParams calc_aucs
#'
#' @return Outcome column recoded as binary (1 = outcome of interest, 0 = other outcome)
#' @noRd
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' recode_outcome(test_data_sm, "dx", "cancer")
recode_outcome <- function(test_data, outcome_colname, outcome_value) {
  outcome_vec <- test_data[, outcome_colname]
  return(ifelse(outcome_vec == outcome_value, 1, 0))
}

#' Calculate AUROC
#'
#' @param pred Predictions of the trained model on the test data
#' @param bin_outcomes Binary outcome vector
#'
#' @return Area under the receiver-operator characteristic curve
#' @noRd
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' prediction <- get_prediction(trained_model_sm1, test_data_sm, "cancer")
#' outcomes <- recode_outcome(test_data_sm, "dx", "cancer")
#' calc_auroc(prediction, outcomes)
calc_auroc <- function(pred, bin_outcomes) {
  return(PRROC::roc.curve(pred, weights.class0 = bin_outcomes)$auc)
}

#' Calculate AUPRC
#'
#' @inheritParams calc_auroc
#'
#' @return Area under the precision-recall curve
#' @noRd
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' prediction <- get_prediction(trained_model_sm1, test_data_sm, "cancer")
#' outcomes <- recode_outcome(test_data_sm, "dx", "cancer")
#' calc_auprc(prediction, outcomes)
calc_auprc <- function(pred, bin_outcomes) {
  return(PRROC::pr.curve(pred, weights.class0 = bin_outcomes)$auc.integral)
}
