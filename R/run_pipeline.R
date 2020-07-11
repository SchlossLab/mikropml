

# Author: Begum Topcuoglu
# Date: 2019-01-14
######################################################################
# Description:
# This script trains and tests the model according to proper pipeline
######################################################################

######################################################################
# Dependencies and Outputs:
#    Model to put to function:
#       1. "L2_Logistic_Regression"
#       2. "RBF_SVM"
#       3. "Decision_Tree"
#       4. "Random_Forest"
#       5. "XGBoost"
#    data to put to function:
#         Features: Hemoglobin levels and 16S rRNA gene sequences in the stool
#         Labels: - Colorectal lesions of 490 patients.
#                 - Defined as cancer or not.(Cancer here means: SRN)
#
# Usage:
# Call as source when using the function. The function is:
#   pipeline(data, model)

# Output:
#  A results list of:
#     1. cvAUC and testAUC for 1 data-split
#     2. cvAUC for all hyper-parameters during tuning for 1 datasplit
#     3. feature importance info on first 10 features for 1 datasplit
#     4. trained model as a caret object
######################################################################

######################################################################
#------------------------- DEFINE FUNCTION -------------------#
######################################################################

#' Run machine learning pipeline
#'
#' @param dataset TODO
#' @param model TODO
#' @param outcome_colname TODO
#' @param outcome_value TODO
#' @param hyperparameters TODO
#' @param permutation TODO
#'
#' @return
#' @export
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#'
#'
run_pipeline <-
  function(dataset,
           model,
           outcome_colname = NA,
           outcome_value = NA,
           hyperparameters = default_hyperparams,
           permutation = TRUE) {

    # If no outcome colname specified, use first column in data
    if (is.na(outcome_colname)) {
      outcome_colname <- colnames(dataset)[1]
    } else {
      # check to see if outcome is in column names of data
      if (!outcome_colname %in% colnames(dataset)) {
        stop(paste("Outcome", outcome_colname, "not in column names of data."))
      }

      # Let's make sure that the first column in the data frame is the outcome variable
      # TODO: is this necessary?
      temp_data <- data.frame(outcome = dataset[, outcome_colname])
      colnames(temp_data) <- outcome_colname
      dataset <-
        cbind(temp_data, dataset[, !(colnames(dataset) %in% outcome_colname)]) # want the outcome column to appear first
    }

    # check binary outcome
    num_outcomes <- length(unique(dataset[, outcome_colname]))
    if (num_outcomes != 2) {
      stop(
        paste(
          "A binary outcome variable is required, but this dataset has",
          num_outcomes,
          "outcomes."
        )
      )
    }

    # pick binary outcome value of interest if not provided by user
    if (is.na(outcome_value)) {
      outcome_value <-
        get_outcome_value(dataset, outcome_colname, method = "fewer")
    } else if (!any(dataset[, outcome_colname] == outcome_value)) {
      stop(
        "No rows in the outcome column (", outcome_colname,
        ") with the outcome of interest (", outcome_value, ") were detected."
      )
    }
    message(paste0(
      "Using '", outcome_colname, "' as the outcome column and '", outcome_value,
      "' as the outcome value of interest."
    ))

    # ------------------Check data for pre-processing------------------------->
    # Data is pre-processed in code/R/setup_model_data.R
    # This removes OTUs with near zero variance and scales 0-1
    # Then generates a correlation matrix
    # Test if data has been preprocessed - range 0-1 and are not all 0s
    # TODO: is this necessary? should it be optional?
    # feature_summary <- any(c(
    #   min(dataset[, -1]) < 0,
    #   max(dataset[, -1]) > 1,
    #   any(apply(dataset[, -1], 2, sum) == 0)
    # ))
    # if (feature_summary) {
    # TODO: more informative error message; e.g. explain expected format
    #   stop(
    #     'Data has not been preprocessed, please use "code/R/setup_model_data.R" to preprocess data'
    #   )
    # }

    # ------------------Randomize features----------------------------------->
    # Randomize feature order, to eliminate any position-dependent effects
    features <- sample(colnames(dataset[, -1]))
    dataset <- dplyr::select(dataset, dplyr::one_of(outcome_colname), dplyr::one_of(features))



    # ------------------80-20 Datasplit for each seed------------------------->
    # Do the 80-20 data-split
    # Stratified data partitioning %80 training - %20 testing
    inTraining <-
      caret::createDataPartition(dataset[, outcome_colname], p = .80, list = FALSE)
    train_data <- dataset[inTraining, ]
    test_data <- dataset[-inTraining, ]
    # ----------------------------------------------------------------------->

    # -------------Define hyper-parameter and cv settings-------------------->
    # Define hyper-parameter tuning grid and the training method
    # Uses function tuning_grid() in file ('code/learning/tuning_grid.R')
    tune <- tuning_grid(train_data, model, outcome_colname, hyperparameters)
    grid <- tune[[1]]
    method <- tune[[2]]
    cv <- tune[[3]]
    # ----------------------------------------------------------------------->

    # ---------------------------Train the model ---------------------------->
    # ------------------------------- 1. -------------------------------------
    # - We train on the 80% of the full data.
    # - We use the cross-validation and hyper-parameter settings defined above to train
    # ------------------------------- 2. -------------------------------------
    # We use ROC metric for all the models
    # To do that I had to make changes to the caret package functions.
    # The files 'data/caret_models/svmLinear3.R and svmLinear5.R are my functions.
    # I added 1 line to get Decision Values for linear SVMs:
    #
    #           prob = function(modelFit, newdata, submodels = NULL){
    #             predict(modelFit, newdata, decisionValues = TRUE)$decisionValues
    #           },
    #
    # This line gives decision values instead of probabilities and computes ROC in:
    #   1. train function with the cross-validataion
    #   2. final trained model
    # using decision values and saves them in the variable "prob"
    # ------------------------------- 3. --------------------------------------
    # - If the model is logistic regression, we need to add a family=binomial parameter.
    # - If the model is random forest, we need to add a ntree=1000 parameter.
    #         We chose ntree=1000 empirically.
    # ----------------------------------------------------------------------->
    # Make formula based on outcome
    f <- stats::as.formula(paste(outcome_colname, "~ ."))
    message("Machine learning formula:")
    message(f)
    # TODO: use named list or vector instead of if/else block? could use a quosure to delay evaluation?
    if (model == "L2_Logistic_Regression") {
      message(model)

      trained_model <- caret::train(
        f,
        # label
        data = train_data,
        # total data
        method = method,
        trControl = cv,
        metric = "ROC",
        tuneGrid = grid,
        family = "binomial"
      )
    }
    else if (model == "Random_Forest") {
      message(model)

      trained_model <- caret::train(
        f,
        data = train_data,
        method = method,
        trControl = cv,
        metric = "ROC",
        tuneGrid = grid,
        ntree = 1000
      ) # not tuning ntree
    }
    else {
      message(model)

      trained_model <- caret::train(
        f,
        data = train_data,
        method = method,
        trControl = cv,
        metric = "ROC",
        tuneGrid = grid
      )
    } 
    # ------------- Output the cvAUC and testAUC for 1 datasplit ---------------------->
    # Mean cv AUC value over repeats of the best cost parameter during training
    cv_auc <- caret::getTrainPerf(trained_model)$TrainROC
    # Save all results of hyper-parameters and their corresponding meanAUCs over 100 internal repeats
    results_individual <- trained_model$results
    # ---------------------------------------------------------------------------------->

    # Get AUROC and AUPRC
    # Calculate the test aucs for the actual pre-processed held-out data
    aucs <-
      calc_aucs(trained_model, test_data, outcome_colname, outcome_value)
    test_auc <- aucs$auroc
    auprc <- aucs$auprc

    # -------------------------- Feature importances ----------------------------------->
    #   if linear: Output the weights of features of linear models
    #   else: Output the feature importances based on random permutation for non-linear models
    # Here we look at the top 20 important features
    if (permutation) {
      # We will use the permutation_importance function here to:
      #     1. Predict held-out test-data
      #     2. Calculate ROC and AUROC values on this prediction
      #     3. Get the feature importances for correlated and uncorrelated feautures
      roc_results <-
        permutation_importance(
          trained_model,
          test_data,
          outcome_colname,
          outcome_value,
          level
        )
      if (model == "L2_Logistic_Regression") {
        feature_importance_weights <-
          trained_model$finalModel$W # Get feature weights
        feature_importance_perm <-
          roc_results # save permutation results
      } else {
        feature_importance_weights <- NULL
        feature_importance_perm <-
          roc_results # save permutation results of cor
      }
    } else {
      message("No permutation test being performed.")
      if (model == "L2_Logistic_Regression") {
        # Get feature weights
        feature_importance_weights <- trained_model$finalModel$W
        # Get feature weights
        feature_importance_perm <- NULL
      } else {
        # Get feature weights
        feature_importance_weights <- NULL
        # Get feature weights
        feature_importance_perm <- NULL
      }
    }

    # ---------------------------------------------------------------------------------->

    # ----------------------------Save metrics as vector ------------------------------->
    # Return all the metrics
    results <-
      list(
        cv_auc,
        test_auc,
        results_individual,
        feature_importance_weights,
        feature_importance_perm,
        trained_model,
        auprc
      )
    return(results)
  }
