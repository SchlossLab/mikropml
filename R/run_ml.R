#' Run the machine learning pipeline
#'
#' This function splits the data set into a train & test set,
#' trains machine learning (ML) models using k-fold cross-validation,
#' evaluates the best model on the held-out test set,
#' and optionally calculates feature importance using the framework
#' outlined in Topçuoğlu _et al._ 2020 (\doi{10.1128/mBio.00434-20}).
#' Required inputs are a data frame (must contain an outcome variable and all
#' other columns as features) and the ML method.
#' See `vignette('introduction')` for more details.
#'
#' @param dataset Data frame with an outcome variable and other columns as features.
#' @param method ML method.
#'   Options: `c("glmnet", "rf", "rpart2", "svmRadial", "xgbTree")`.
#'   - glmnet: linear, logistic, or multiclass regression
#'   - rf: random forest
#'   - rpart2: decision tree
#'   - svmRadial: support vector machine
#'   - xgbTree: xgboost
#' @param outcome_colname Column name as a string of the outcome variable
#'   (default `NULL`; the first column will be chosen automatically).
#' @param pos_class The positive class, i.e. which level of `outcome_colname` is
#'   the event of interest. If the outcome is binary, either the
#'   `outcome_colname` must be a factor with the first level being the positive
#'   class, or `pos_class` must be set. (default: `NULL`).
#' @param hyperparameters Dataframe of hyperparameters
#'   (default `NULL`; sensible defaults will be chosen automatically).
#' @param seed Random seed (default: `NA`).
#'  Your results will only be reproducible if you set a seed.
#' @param find_feature_importance Run permutation importance (default: `FALSE`).
#'   `TRUE` is recommended if you would like to identify features important for
#'   predicting your outcome, but it is resource-intensive.
#' @param calculate_performance Whether to calculate performance metrics (default: `TRUE`).
#'   You might choose to skip this if you do not perform cross-validation during model training.
#' @param kfold Fold number for k-fold cross-validation (default: `5`).
#' @param cv_times Number of cross-validation partitions to create (default: `100`).
#' @param cross_val a custom cross-validation scheme from `caret::trainControl()`
#'   (default: `NULL`, uses `kfold` cross validation repeated `cv_times`).
#'   `kfold` and `cv_times` are ignored if the user provides a custom cross-validation scheme.
#'   See the `caret::trainControl()` docs for information on how to use it.
#' @param training_frac Fraction of data for training set (default: `0.8`). Rows
#'   from the dataset will be randomly selected for the training set, and all
#'   remaining rows will be used in the testing set. Alternatively, if you
#'   provide a vector of integers, these will be used as the row indices for the
#'   training set. All remaining rows will be used in the testing set.
#' @param perf_metric_function Function to calculate the performance metric to
#'   be used for cross-validation and test performance. Some functions are
#'   provided by caret (see [caret::defaultSummary()]).
#'   Defaults: binary classification = `twoClassSummary`,
#'             multi-class classification = `multiClassSummary`,
#'             regression = `defaultSummary`.
#' @param perf_metric_name The column name from the output of the function
#'   provided to perf_metric_function that is to be used as the performance metric.
#'   Defaults: binary classification = `"ROC"`,
#'             multi-class classification = `"logLoss"`,
#'             regression = `"RMSE"`.
#' @param groups Vector of groups to keep together when splitting the data into
#'   train and test sets. If the number of groups in the training set is larger
#'   than `kfold`, the groups will also be kept together for cross-validation.
#'   Length matches the number of rows in the dataset (default: `NULL`).
#' @param group_partitions Specify how to assign `groups` to the training and
#'   testing partitions (default: `NULL`). If `groups` specifies that some
#'   samples belong to group `"A"` and some belong to group `"B"`, then setting
#'   `group_partitions = list(train = c("A", "B"), test = c("B"))` will result
#'   in all samples from group `"A"` being placed in the training set, some
#'   samples from `"B"` also in the training set, and the remaining samples from
#'   `"B"` in the testing set. The partition sizes will be as close to
#'   `training_frac` as possible. If the number of groups in the training set is
#'   larger than `kfold`, the groups will also be kept together for
#'   cross-validation.
#' @param corr_thresh For feature importance, group correlations
#'   above or equal to `corr_thresh` (range `0` to `1`; default: `1`).
#' @param ... All additional arguments are passed on to `caret::train()`, such as
#'   case weights via the `weights` argument or `ntree` for `rf` models.
#'   See the `caret::train()` docs for more details.
#'
#'
#' @return Named list with results:
#'
#' - `trained_model`: Output of [caret::train()], including the best model.
#' - `test_data`: Part of the data that was used for testing.
#' - `performance`: Data frame of performance metrics. The first column is the
#'    cross-validation performance metric, and the last two columns are the ML
#'    method used and the seed (if one was set), respectively.
#'    All other columns are performance metrics calculated on the test data.
#'    This contains only one row, so you can easily combine performance
#'    data frames from multiple calls to `run_ml()`
#'    (see `vignette("parallel")`).
#' - `feature_importance`: If feature importances were calculated, a data frame
#'    where each row is a feature or correlated group. The columns are the
#'    performance metric of the permuted data, the difference between the true
#'    performance metric and the performance metric of the permuted data
#'    (true - permuted), the feature name, the ML method,
#'    the performance metric name, and the seed (if provided).
#'    For AUC and RMSE, the higher perf_metric_diff is, the more important that
#'    feature is for predicting the outcome. For log loss, the lower
#'    perf_metric_diff is, the more important that feature is for
#'    predicting the outcome.
#'
#' @section More details:
#'
#' For more details, please see
#' [the vignettes](http://www.schlosslab.org/mikropml/articles/).
#'
#' @export
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' \dontrun{
#'
#' # regression
#' run_ml(otu_small, "glmnet",
#'   seed = 2019
#' )
#'
#' # random forest w/ feature importance
#' run_ml(otu_small, "rf",
#'   outcome_colname = "dx",
#'   find_feature_importance = TRUE
#' )
#'
#' # custom cross validation & hyperparameters
#' run_ml(otu_mini_bin[, 2:11],
#'   "glmnet",
#'   outcome_colname = "Otu00001",
#'   seed = 2019,
#'   hyperparameters = list(lambda = c(1e-04), alpha = 0),
#'   cross_val = caret::trainControl(method = "none"),
#'   calculate_performance = FALSE
#' )
#' }
run_ml <-
  function(dataset,
           method,
           outcome_colname = NULL,
           pos_class = NULL,
           hyperparameters = NULL,
           find_feature_importance = FALSE,
           calculate_performance = TRUE,
           kfold = 5,
           cv_times = 100,
           cross_val = NULL,
           training_frac = 0.8,
           perf_metric_function = NULL,
           perf_metric_name = NULL,
           groups = NULL,
           group_partitions = NULL,
           corr_thresh = 1,
           seed = NA,
           ...) {
    check_all(
      dataset,
      method,
      find_feature_importance,
      kfold,
      training_frac,
      perf_metric_function,
      perf_metric_name,
      groups,
      group_partitions,
      corr_thresh,
      seed,
      hyperparameters
    )
    if (!is.na(seed)) {
      set.seed(seed)
    }

    # `future.apply` is required for `find_feature_importance()`.
    # check it here to adhere to the fail fast principle.
    if (find_feature_importance) {
      abort_packages_not_installed("future.apply")
    }
    # can't have categorical features for feature importance because have to find correlations
    outcome_colname <- check_outcome_column(dataset, outcome_colname)
    if (find_feature_importance) {
      check_cat_feats(dataset %>% dplyr::select(-outcome_colname))
    }

    dataset <- dataset %>%
      randomize_feature_order(outcome_colname) %>%
      # convert tibble to dataframe to silence warning from caret::train():
      # "Warning: Setting row names on a tibble is deprecated.."
      as.data.frame()

    outcomes_vctr <- dataset %>% dplyr::pull(outcome_colname)

    if (length(training_frac) == 1) {
      training_inds <- get_partition_indices(outcomes_vctr,
        training_frac = training_frac,
        groups = groups,
        group_partitions = group_partitions
      )
    } else {
      training_inds <- training_frac
      training_frac <- length(training_inds) / nrow(dataset)
      message(
        paste0(
          "Using the custom training set indices provided by `training_frac`.
      The fraction of data in the training set will be ",
          round(training_frac, 2)
        )
      )
    }
    check_training_frac(training_frac)
    check_training_indices(training_inds, dataset)

    train_data <- dataset[training_inds, ]
    test_data <- dataset[-training_inds, ]
    # train_groups & test_groups will be NULL if groups is NULL
    train_groups <- groups[training_inds]
    test_groups <- groups[-training_inds]

    if (is.null(hyperparameters)) {
      hyperparameters <- get_hyperparams_list(dataset, method)
    }
    tune_grid <- get_tuning_grid(hyperparameters, method)


    outcome_type <- get_outcome_type(outcomes_vctr)
    class_probs <- outcome_type != "continuous"
    if (outcome_type == "binary") {
      # enforce factor levels
      dataset <- dataset %>% set_outcome_factor(outcome_colname, pos_class)
    }

    if (is.null(perf_metric_function)) {
      perf_metric_function <- get_perf_metric_fn(outcome_type)
    }

    if (is.null(perf_metric_name)) {
      perf_metric_name <- get_perf_metric_name(outcome_type)
    }

    if (is.null(cross_val)) {
      cross_val <- define_cv(
        train_data,
        outcome_colname,
        hyperparameters,
        perf_metric_function,
        class_probs,
        kfold = kfold,
        cv_times = cv_times,
        groups = train_groups,
        group_partitions = group_partitions
      )
    }


    message("Training the model...")
    trained_model_caret <- train_model(
      train_data = train_data,
      outcome_colname = outcome_colname,
      method = method,
      cv = cross_val,
      perf_metric_name = perf_metric_name,
      tune_grid = tune_grid,
      ...
    )
    message("Training complete.")
    if (!is.na(seed)) {
      set.seed(seed)
    }
    # verify that correct outcome level got used
    trained_model_caret$levels[1]

    if (calculate_performance) {
      performance_tbl <- get_performance_tbl(
        trained_model_caret,
        test_data,
        outcome_colname,
        perf_metric_function,
        perf_metric_name,
        class_probs,
        method,
        seed
      )
    } else {
      performance_tbl <- "Skipped calculating performance"
    }

    if (find_feature_importance) {
      message("Finding feature importance...")
      feature_importance_tbl <- get_feature_importance(
        trained_model_caret,
        test_data,
        outcome_colname,
        perf_metric_function,
        perf_metric_name,
        class_probs,
        method,
        seed,
        corr_thresh
      )
      message("Feature importance complete.")
    } else {
      feature_importance_tbl <- "Skipped feature importance"
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
