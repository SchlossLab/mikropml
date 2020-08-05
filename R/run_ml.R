#' Run the machine learning pipeline
#'
#' @param dataset dataframe with an outcome variable and other columns as features
#' @param method ML method ("regLogistic", "svmRadial", "rpart2", "rf", "xgbTree")
#' @param outcome_colname column name as a string of the outcome variable
#' @param outcome_value outcome value of interest as a string
#' @param hyperparameters dataframe of hyperparameters (default: default_hyperparams)
#' @param find_feature_importance run permutation imporance (default: FALSE)
#' @param nfolds fold number for cross-validation (default: 5)
#' @param training_frac fraction size of data for training (default: 0.8)
#' @param seed random seed (default: NA)
#'
#' @return named list with results
#' @export
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
run_ml <-
  function(dataset,
           method,
           outcome_colname = NA,
           outcome_value = NA,
           hyperparameters = mikRopML::default_hyperparams,
           find_feature_importance = FALSE,
           nfolds = as.integer(5),
           training_frac = 0.8,
           seed = NA) {
    # input validation
    check_all(dataset,
              method,
              find_feature_importance,
              nfolds,
              training_frac,
              seed)
    outcome_colname <-
      check_outcome_column(dataset, outcome_colname)
    outcome_value <- check_outcome_value(dataset, outcome_colname,
                                         outcome_value,
                                         method = "fewer")
    dataset <-
      randomize_feature_order(dataset, outcome_colname, seed = NA)


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

    if (!is.na(seed)) {
      set.seed(seed)
    }
    inTraining <-
      caret::createDataPartition(dataset[, outcome_colname],
                                 p = training_frac, list = FALSE)
    train_data <- dataset[inTraining,]
    test_data <- dataset[-inTraining,]

    tune_grid <- get_tuning_grid(hyperparameters, method)
    cv <-
      define_cv(train_data,
                outcome_colname,
                nfolds = nfolds,
                seed = seed)

    model_formula <-
      stats::as.formula(paste(outcome_colname, "~ ."))

    # TODO: use named list or vector instead of if/else block? could use a quosure to delay evaluation?
    # TODO: or could set unused args to NULL and just call train once?
    metric <- "ROC"
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

    return(
      list(
        trained_model = trained_model,
        cv_auc = caret::getTrainPerf(trained_model)$TrainROC,
        test_aucs = calc_aucs(trained_model, test_data, outcome_colname, outcome_value),
        feature_importance = ifelse(
          find_feature_importance,
          get_feature_importance(
            trained_model,
            train_data,
            test_data,
            outcome_colname,
            outcome_value
          ),
          "Skipped feature importance"
        )
      )
    )
  }
