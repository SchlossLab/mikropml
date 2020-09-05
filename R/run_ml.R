#' Run the machine learning pipeline
#'
#' @param dataset dataframe with an outcome variable and other columns as features
#' @param method ML method ("regLogistic", "rf", "rpart2", "svmRadial", "xgbTree")
#' @param outcome_colname column name as a string of the outcome variable
#' @param outcome_value outcome value of interest as a string
#' @param hyperparameters dataframe of hyperparameters (default: NULL). if NULL given, they will be chosen automatically.
#' @param find_feature_importance run permutation imporance (default: FALSE)
#' @param kfold fold number for k-fold cross-validation (default: 5)
#' @param cv_times Number of partitions to create
#' @param training_frac fraction size of data for training (default: 0.8)
#' @param group vector of groups to keep together when splitting the data into train and test sets, and for cross-validation; length matches the number of rows in the dataset (default: no groups)
#' @param corr_thresh for feature importance, group correlations above or equal to corr_thresh (default: 1)
#' @param seed random seed (default: NULL)
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
           outcome_colname = NULL,
           outcome_value = NULL,
           hyperparameters = NULL,
           find_feature_importance = FALSE,
           kfold = 5,
           cv_times = 100,
           training_frac = 0.8,
           group = NULL,
           corr_thresh = 1,
           seed = NULL,
           ncores = NA) {
    check_all(
      dataset,
      method,
      find_feature_importance,
      kfold,
      training_frac,
      group,
      corr_thresh,
      seed
    )
    outcome_colname <- check_outcome_column(dataset, outcome_colname)
    outcome_value <- check_outcome_value(dataset, outcome_colname,
      outcome_value,
      method = "fewer"
    )
    dataset <- randomize_feature_order(dataset, outcome_colname, seed = seed)

    if (!is.null(seed)) {
      set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion")
    }

    if (is.null(group)) {
      training_inds <-
        caret::createDataPartition(dataset[, outcome_colname],
          p = training_frac, list = FALSE
        )
    } else {
      training_inds <-
        createGroupedDataPartition(group,
          p = training_frac
        )
      train_group <- group[training_inds]
      test_group <- group[-training_inds]
    }

    train_data <- dataset[training_inds, ]
    test_data <- dataset[-training_inds, ]

    if (is.null(hyperparameters)) {
      hyperparameters <- get_hyperparams_list(dataset, method)
    }
    check_hyperparams(hyperparameters, method = method)

    tune_grid <- get_tuning_grid(hyperparameters, method)
    if (is.null(group)) {
      cv <- define_cv(train_data,
        outcome_colname,
        hyperparameters,
        kfold = kfold,
        seed = seed,
        cv_times = cv_times
      )
    } else {
      cv <- define_cv(train_data,
        outcome_colname,
        hyperparameters,
        kfold = kfold,
        cv_times = cv_times,
        group = train_group,
        seed = seed
      )
    }

    model_formula <- stats::as.formula(paste(outcome_colname, "~ ."))

    pcluster <- setup_parallel(ncores)

    metric <- "ROC"
    if (method == "regLogistic") {
      trained_model_caret <- caret::train(
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
      trained_model_caret <- caret::train(
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
      trained_model_caret <- caret::train(
        model_formula,
        data = train_data,
        method = method,
        trControl = cv,
        metric = metric,
        tuneGrid = tune_grid
      )
    }

    stop_parallel(pcluster)

    feature_importance_result <- "Skipped feature importance"
    if (find_feature_importance) {
      feature_importance_result <-
        get_feature_importance(
          trained_model_caret,
          train_data,
          test_data,
          outcome_colname,
          outcome_value,
          corr_thresh
        )
    }

    return(
      list(
        trained_model = trained_model_caret,
        performance = get_performance_tbl(
          trained_model_caret,
          test_data,
          outcome_colname,
          outcome_value
        ),
        feature_importance = feature_importance_result
      )
    )
  }
