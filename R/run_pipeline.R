#' Run machine learning pipeline
#'
#' @param dataset TODO
#' @param method TODO
#' @param outcome_colname TODO
#' @param outcome_value TODO
#' @param hyperparameters TODO
#' @param metric TODO
#' @param permute TODO
#' @param nfolds fold number for cross-validation
#' @param seed random seed (default: NA)
#'
#' @return named list with results
#' @export
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#'
run_pipeline <-
  function(dataset,
           method,
           outcome_colname = NA,
           outcome_value = NA,
           hyperparameters = mikRopML::default_hyperparams,
           metric = "ROC",
           permute = FALSE,
           nfolds = 5,
           seed = NA) {
    if (!is.na(seed)) {
      set.seed(seed)
    }

    methods <- c("regLogistic", "svmRadial", "rpart2", "rf", "xgbTree")
    if (!(method %in% methods)) {
      stop(paste0(
        "Method '",
        method,
        "' is not supported. Supported methods are:",
        paste(methods, sep = ", ", collapse = "")
      ))
    }

    hyperparameters <- validate_hyperparams_df(hyperparameters, method)

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

    # Randomize feature order to eliminate any position-dependent effects
    features <- sample(colnames(dataset[, -1]))
    dataset <- dplyr::select(
      dataset,
      dplyr::one_of(outcome_colname),
      dplyr::one_of(features)
    )

    # TODO: optional arg for trainingpartition size
    inTraining <-
      caret::createDataPartition(dataset[, outcome_colname], p = .80, list = FALSE)
    train_data <- dataset[inTraining, ]
    test_data <- dataset[-inTraining, ]


    tune_grid <- get_tuning_grid(hyperparameters)
    cv <- define_cv(train_data, outcome_colname, nfolds = nfolds)

    # Make formula based on outcome
    model_formula <- stats::as.formula(paste(outcome_colname, "~ ."))

    # TODO: use named list or vector instead of if/else block? could use a quosure to delay evaluation?
    # TODO: or could set unused args to NULL and just call train once?
    if (method == "regLogistic") {
      trained_model <- caret::train(
        model_formula,
        data = train_data,
        method = method,
        trControl = cv,
        metric = metric,
        tuneGrid = tune_grid,
        family = "binomial"
      )
    }
    else if (method == "rf") {
      trained_model <- caret::train(
        model_formula,
        data = train_data,
        method = method,
        trControl = cv,
        metric = metric,
        tuneGrid = tune_grid,
        ntree = 1000
      ) # not tuning ntree
    }
    else {
      trained_model <- caret::train(
        model_formula,
        data = train_data,
        method = method,
        trControl = cv,
        metric = metric,
        tuneGrid = tune_grid
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
        permutation_importance(
          dataset,
          trained_model,
          test_data,
          outcome_colname,
          outcome_value
        )
    } else {
      message("Skipping permutation test")
      feature_importance_perm <- NULL
    }

    feature_importance_weights <- ifelse(method == "L2_Logistic_Regression",
      trained_model$finalModel$W,
      NULL
    )

    return(
      list(
        trained_model = trained_model,
        cv_auc = cv_auc,
        test_aucs = test_aucs,
        results_individual = results_individual,
        feature_importance_weights = feature_importance_weights,
        feature_importance_perm = feature_importance_perm
      )
    )
  }
