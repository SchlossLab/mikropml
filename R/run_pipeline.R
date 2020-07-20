

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
           permute = FALSE,
           seed = NA) {

    if (!is.na(seed)) {
      set.seed(seed)
    }

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
    dataset <- dplyr::select(dataset,
                             dplyr::one_of(outcome_colname),
                             dplyr::one_of(features))

    # TODO: optional arg for trainingpartition size
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

    # Make formula based on outcome
    f <- stats::as.formula(paste(outcome_colname, "~ ."))

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

    # Calculate the aucs for the held-out test data
    test_aucs <- calc_aucs(trained_model, test_data, outcome_colname, outcome_value)

    if (permute) {
      message("Performing permutation test")
      feature_importance_perm <-
        get_feature_importance(dataset,
                               trained_model,
                               test_data,
                               outcome_colname,
                               outcome_value)
    } else {
      message("Skipping permutation test")
      feature_importance_perm <- NULL
    }

    feature_importance_weights <- ifelse(model == "L2_Logistic_Regression",
                                         trained_model$finalModel$W,
                                         NULL)

    return(
      list(
        trained_model = trained_model,
        cv_auc = cv_auc,
        test_aucs = test_aucs,
        results_individual = results_individual,
        feature_importance_weights = feature_importance_weights,
        feature_importance_perm = feature_importance_perm
      ))
  }
