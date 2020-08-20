#' Run the machine learning pipeline
#'
#' @param dataset dataframe with an outcome variable and other columns as features
#' @param method ML method ("regLogistic", "svmRadial", "rpart2", "rf", "xgbTree")
#' @param outcome_colname column name as a string of the outcome variable
#' @param outcome_value outcome value of interest as a string
#' @param hyperparameters dataframe of hyperparameters (default: default_hyperparams)
#' @param find_feature_importance run permutation imporance (default: FALSE)
#' @param kfold fold number for k-fold cross-validation (default: 5)
#' @param training_frac fraction size of data for training (default: 0.8)
#' @param corr_thresh for feature importance, group correlations above or equal to corr_thresh (default: 1)
#' @param seed random seed (default: NA)
#' @param ncores number of cores for parallel processing (default: NA). `parallel` and `doParallel` packages are needed for ncores > 1
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
           kfold = 5,
           training_frac = 0.8,
           corr_thresh = corr_thresh,
           seed = NA,
           ncores = NA) {
    check_all(
      dataset,
      method,
      find_feature_importance,
      kfold,
      training_frac,
      seed
    )
    outcome_colname <-
      check_outcome_column(dataset, outcome_colname)
    outcome_value <- check_outcome_value(dataset, outcome_colname,
      outcome_value,
      method = "fewer"
    )
    dataset <-
      randomize_feature_order(dataset, outcome_colname, seed = seed)

    if (!is.na(seed)) {
      set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion")
    }
    inTraining <-
      caret::createDataPartition(dataset[, outcome_colname],
        p = training_frac, list = FALSE
      )
    train_data <- dataset[inTraining, ]
    test_data <- dataset[-inTraining, ]

    tune_grid <- get_tuning_grid(hyperparameters, method)
    cv <-
      define_cv(train_data,
        outcome_colname,
        kfold = kfold,
        seed = seed
      )

    model_formula <-
      stats::as.formula(paste(outcome_colname, "~ ."))

    pcluster <- setup_parallel(ncores)

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

    stop_parallel(pcluster)

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
            outcome_value,
            corr_thresh
          ),
          "Skipped feature importance"
        )
      )
    )
  }
