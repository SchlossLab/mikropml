#' Run the machine learning pipeline
#'
#' TODO: more details
#'
#' @param dataset Dataframe with an outcome variable and other columns as features.
#' @param method ML method. Options: `c("regLogistic", "rf", "rpart2", "svmRadial", "xgbTree")``
#' @param outcome_colname Column name as a string of the outcome variable (default `NULL`; will be chosen automatically).
#' @param outcome_value Outcome value of interest as a string (default `NULL`; will be chosen automatically).
#' @param hyperparameters Dataframe of hyperparameters (default `NULL`; will be chosen automatically).
#' @param seed Random seed (default: `NA`). Your results will be reproducible if you set a seed.
#' @param find_feature_importance Run permutation imporance (default: `FALSE`). This is recommended, but it is resource-intensive.
#' @param kfold Fold number for k-fold cross-validation (default: `5`).
#' @param cv_times Number of partitions to create (default: `100`).
#' @param training_frac Fraction of data for training set (default: `0.8`). The remaining data will be used in the testing set.
#' @param perf_metric_function Function to calculate the performance metric to be used for cross-validation and test performance. Some functions are provided by caret (see \link[caret]{defaultSummary}). Defaults: binary classification = `twoClassSummary`, multi-class classification = `multiClassSummary`, regression = `defaultSummary`.
#' @param perf_metric_name The column name from the output of the function provided to perf_metric_function that is to be used as the performance metric. Defaults: binary classification = `"ROC"`, multi-class classification = `"logLoss"`, regression = `"RMSE"`.
#' @param group Vector of groups to keep together when splitting the data into train and test sets, and for cross-validation; length matches the number of rows in the dataset (default: no groups).
#' @param corr_thresh For feature importance, group correlations above or equal to corr_thresh (default: `1`).
#'
#' @return named list with results
#' @export
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' \dontrun{
#' run_ml(otu_large, "regLogistic")
#' run_ml(otu_mini, "regLogistic",
#'   kfold = 2,
#'   find_feature_importance = TRUE
#' )
#' }
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
           perf_metric_function = NULL,
           perf_metric_name = NULL,
           group = NULL,
           corr_thresh = 1,
           seed = NA) {
    check_all(
      dataset,
      method,
      find_feature_importance,
      kfold,
      training_frac,
      perf_metric_function,
      perf_metric_name,
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

    if (!is.na(seed)) {
      set.seed(seed)
    }

    if (is.null(group)) {
      training_inds <-
        caret::createDataPartition(dataset %>% dplyr::pull(outcome_colname),
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

    outcomes_vec <- dataset %>% dplyr::pull(outcome_colname)

    outcome_type <- get_outcome_type(outcomes_vec)
    class_probs <- ifelse(outcome_type == "numeric", FALSE, TRUE)

    if (is.null(perf_metric_function)) {
      perf_metric_function <- get_perf_metric_fn(outcome_type)
    }

    if (is.null(perf_metric_name)) {
      perf_metric_name <- get_perf_metric_name(outcome_type)
    }

    if (is.null(group)) {
      cv <- define_cv(train_data,
        outcome_colname,
        hyperparameters,
        perf_metric_function,
        class_probs,
        kfold = kfold,
        seed = seed,
        cv_times = cv_times
      )
    } else {
      cv <- define_cv(train_data,
        outcome_colname,
        hyperparameters,
        perf_metric_function,
        class_probs,
        kfold = kfold,
        cv_times = cv_times,
        group = train_group,
        seed = seed
      )
    }

    model_formula <- stats::as.formula(paste(outcome_colname, "~ ."))
    # metric <- "ROC"
    if (method == "regLogistic") {
      trained_model_caret <- caret::train(
        model_formula,
        data = train_data,
        method = method,
        trControl = cv,
        metric = perf_metric_name,
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
        metric = perf_metric_name,
        tuneGrid = tune_grid,
        ntree = 1000
      ) # caret doesn't allow ntree to be tuned
    }
    else {
      trained_model_caret <- caret::train(
        model_formula,
        data = train_data,
        method = method,
        trControl = cv,
        metric = perf_metric_name,
        tuneGrid = tune_grid
      )
    }

    performance_tbl <- get_performance_tbl(
      trained_model_caret,
      test_data,
      outcome_colname,
      outcome_value,
      seed
    )
    feature_importance_tbl <- "Skipped feature importance"
    if (find_feature_importance) {
      feature_importance_tbl <- get_feature_importance(
        trained_model_caret,
        train_data,
        test_data,
        outcome_colname,
        outcome_value,
        method,
        seed,
        corr_thresh
      )
    }

    return(
      list(
        trained_model = trained_model_caret,
        test_data = test_data,
        performance = performance_tbl,
        feature_importance = feature_importance_tbl
      )
    )
  }
