#' Get outcome type.
#'
#' If the outcome is numeric, the type is continuous.
#' Otherwise, the outcome type is binary if there are only two outcomes or
#' multiclass if there are more than two outcomes.
#'
#' @param outcomes_vec Vector of outcomes.
#'
#' @return Outcome type (continuous, binary, or multiclass).
#' @export
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' get_outcome_type(c(1, 2, 1))
#' get_outcome_type(c("a", "b", "b"))
#' get_outcome_type(c("a", "b", "c"))
get_outcome_type <- function(outcomes_vec) {
  num_outcomes <- length(unique(outcomes_vec))
  if (num_outcomes < 2) {
    stop(
      paste0(
        "A continuous, binary, or multi-class outcome variable is required, but this dataset has ",
        num_outcomes,
        " outcome(s)."
      )
    )
  }
  if (is.numeric(outcomes_vec)) {
    # regression
    otype <- "continuous"
  } else if (num_outcomes == 2) {
    # binary classification
    otype <- "binary"
  } else {
    # multi-class classification
    otype <- "multiclass"
  }
  return(otype)
}


#' Get default performance metric function
#'
#' @param outcome_type Type of outcome (one of: `"continuous"`,`"binary"`,`"multiclass"`).
#'
#' @return Performance metric function.
#' @export
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' get_perf_metric_fn("continuous")
#' get_perf_metric_fn("binary")
#' get_perf_metric_fn("multiclass")
get_perf_metric_fn <- function(outcome_type) {
  if (outcome_type == "continuous") { # regression
    perf_metric_fn <- caret::defaultSummary
  } else if (outcome_type %in% c("binary", "multiclass")) { # multi-class classification
    perf_metric_fn <- caret::multiClassSummary
  } else {
    stop(paste0('Outcome type of outcome must be one of: `"continuous"`,`"binary"`,`"multiclass`), but you provided: ', outcome_type))
  }
  return(perf_metric_fn)
}


#' Get default performance metric name
#'
#' Get default performance metric name for cross-validation.
#'
#' @param outcome_type Type of outcome (one of: `"continuous"`,`"binary"`,`"multiclass"`).
#'
#' @return Performance metric name.
#' @export
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' get_perf_metric_name("continuous")
#' get_perf_metric_name("binary")
#' get_perf_metric_name("multiclass")
get_perf_metric_name <- function(outcome_type) {
  if (outcome_type == "continuous") { # regression
    perf_metric_name <- "RMSE"
  } else {
    if (outcome_type == "binary") { # binary classification
      perf_metric_name <- "AUC"
    } else if (outcome_type == "multiclass") { # multi-class classification
      perf_metric_name <- "logLoss"
    } else {
      stop(paste0('Outcome type of outcome must be one of: `"continuous"`,`"binary"`,`"multiclass`), but you provided: ', outcome_type))
    }
  }
  return(perf_metric_name)
}

#' Get performance metrics for test data
#'
#' @param test_data Held out test data: dataframe of outcome and features.
#' @param trained_model Trained model from [caret::train()].
#' @param class_probs Whether to use class probabilities (TRUE for categorical outcomes, FALSE for numeric outcomes).
#' @inheritParams run_ml
#'
#' @return
#'
#' Dataframe of performance metrics.
#'
#' @export
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' \dontrun{
#' results <- run_ml(otu_small, "glmnet", kfold = 2, cv_times = 2)
#' calc_perf_metrics(results$test_data,
#'   results$trained_model,
#'   "dx",
#'   multiClassSummary,
#'   class_probs = TRUE
#' )
#' }
calc_perf_metrics <- function(test_data, trained_model, outcome_colname, perf_metric_function, class_probs) {
  pred_type <- "raw"
  if (class_probs) pred_type <- "prob"
  preds <- stats::predict(trained_model, test_data, type = pred_type)
  if (class_probs) {
    uniq_obs <- unique(c(test_data %>% dplyr::pull(outcome_colname), as.character(trained_model$pred$obs)))
    obs <- factor(test_data %>% dplyr::pull(outcome_colname), levels = uniq_obs)
    pred_class <- factor(names(preds)[apply(preds, 1, which.max)], levels = uniq_obs)
    perf_met <- perf_metric_function(data.frame(obs = obs, pred = pred_class, preds), lev = uniq_obs)
  } else {
    obs <- test_data %>% dplyr::pull(outcome_colname)
    perf_met <- perf_metric_function(data.frame(obs = obs, pred = preds))
  }
  return(perf_met)
}


#' Get model performance metrics as a one-row tibble
#'
#' @inheritParams calc_perf_metrics
#' @inheritParams run_ml
#' @inheritParams get_feature_importance
#'
#'
#' @return A one-row tibble with a column for the cross-validation performance,
#'  columns for each of the performance metrics for the test data,
#'  plus the `method`, and `seed`.
#'
#' @export
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' \dontrun{
#' results <- run_ml(otu_small, "glmnet", kfold = 2, cv_times = 2)
#' names(results$trained_model$trainingData)[1] <- "dx"
#' get_performance_tbl(results$trained_model, results$test_data,
#'   "dx",
#'   multiClassSummary, "AUC",
#'   class_probs = TRUE,
#'   method = "glmnet"
#' )
#' }
#'
get_performance_tbl <- function(trained_model,
                                test_data,
                                outcome_colname,
                                perf_metric_function,
                                perf_metric_name,
                                class_probs,
                                method,
                                seed = NA) {
  test_perf_metrics <- calc_perf_metrics(
    test_data,
    trained_model,
    outcome_colname,
    perf_metric_function,
    class_probs
  )

  train_perf <- caret::getTrainPerf(trained_model)
  cv_metric_name <- paste0("Train", perf_metric_name)
  cv_metric_options <- names(train_perf)

  if (!(cv_metric_name %in% cv_metric_options)) {
    warning(
      "The performance metric provided does not match the metric used to train the data.\n",
      "You provided: `", perf_metric_name, "`\n",
      "The options are: \n    ",
      paste(gsub("Train|method", "", cv_metric_options),
        collapse = ", "
      )
    )
    cv_metric_value <- NA
  } else {
    cv_metric_value <- train_perf[[cv_metric_name]]
  }
  return(dplyr::bind_rows(c(
    cv_metric = cv_metric_value,
    test_perf_metrics,
    method = method,
    seed = seed
  )) %>%
    dplyr::rename_with(
      function(x) paste0("cv_metric_", perf_metric_name),
      .data$cv_metric
    ) %>%
    change_to_num())
}

#' @name sensspec
#' @title Calculate and summarize sensitivity, specificity, and precision.
#' @description These functions assume a binary outcome
#'
#' @return data frame with summarized performance
#'
#' @author Courtney Armour
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' \dontrun{
#' # get cumulative performance for a single model
#' sensspec_1 <- get_model_sensspec(otu_mini_bin_results_glmnet$trained_model,
#'                    otu_mini_bin_results_glmnet$test_data,
#'                    'dx', 'cancer'
#' )
#' head(pred_1)
#'
#' # get performance for multiple models
#' get_sensspec_seed <- function(seed) {
#'   ml_result <- run_ml(otu_mini_bin, 'glmnet', seed = seed)
#'   sensspec <- get_model_sensspec(ml_result$trained_model,
#'                                  ml_result$test_data,
#'                                  'dx', 'cancer') %>%
#'                                  mutate(seed = seed)
#'   return(sensspec)
#' }
#' sensspec_dat <- purrr::map_dfr(seq(100, 102), get_sensspec_seed)
#'
#' # calculate mean sensitivity over specificity
#' roc_dat <- calc_mean_roc(sensspec_dat)
#' head(roc_dat)
#'
#' # calculate mean precision over recall
#' prc_dat <- calc_mean_prc(sensspec_dat)
#' head(prc_dat)
#'
#' # plot ROC & PRC
#' # TODO
#' }
NULL

#' @describeIn sensspec Get sensitivty, specificity, and precision for a model.
#'
#' @inheritParams calc_perf_metrics
#' @inheritParams run_ml
#' @param pos_outcome the positive outcome from `outcome_colname`,
#'   e.g. "cancer" for the `otu_mini_bin` dataset.
#'
#' @export
get_model_sensspec <- function(trained_model, test_data, outcome_colname, pos_outcome) {
    # adapted from https://github.com/SchlossLab/2021-08-09_ROCcurves/blob/8e62ff8b6fe1b691450c953a9d93b2c11ce3369a/ROCcurves.Rmd#L95-L109
    probs <- predict(model,
                     newdata = test_dat,
                     type = "prob"
    ) %>%
        dplyr::mutate(actual = test_dat %>%
                          dplyr::pull(outcome_colname)
                      )

    total <- probs %>%
        count(actual) %>%
        pivot_wider(names_from = "actual", values_from = "n") %>%
        as.list()

    neg_outcome <- names(total) %>%
        # assumes binary outcome
        Filter(function(x) { x != pos_outcome}, .)

    sensspec <- probs %>%
        arrange(desc(pos_outcome)) %>%
        mutate(is_pos = actual == pos_outcome) %>%
        mutate(
            tp = cumsum(is_pos),
            fp = cumsum(!is_pos),
            sensitivity = tp / total[[pos_outcome]],
            fpr = fp / total[[neg_outcome]]
        ) %>%
        mutate(
            specificity = 1 - fpr,
            precision = tp / (tp + fp)
        ) %>%
        select(-is_pos)
    return(sensspec)
}

#' Generic function to calculate mean performance curves for multiple models
#'
#' @param sensspec_dat data frame by concatenating results of `get_model_sensspec()` for multiple models.
#' @param group_var variable to group by (e.g. specificity or recall).
#' @param sum_var variable to summarize (e.g. sensitivity or precision).
#'
#' @return data frame with mean & sd `sum_var` summarized over `group_var`
#'
#' @author Courtney Armour
#' @author Kelly Sovacool
calc_mean_perf <- function(sensspec_dat,
                           group_var = specificity,
                           sum_var = sensitivity) {
    # adapted from https://github.com/SchlossLab/2021-08-09_ROCcurves/blob/8e62ff8b6fe1b691450c953a9d93b2c11ce3369a/ROCcurves.Rmd#L166-L209

    sensspec_dat %>%
        mutate({{ group_var }} := round({{ group_var }}, 2)) %>%
        group_by({{ group_var }}) %>%
        summarise(
            mean = mean({{ sum_var }}),
            sd = sd({{ sum_var }})
        ) %>%
        mutate(
            upper = mean + sd,
            lower = mean - sd,
            upper = case_when(
                upper > 1 ~ 1,
                TRUE ~ upper
            ),
            lower = case_when(
                lower < 0 ~ 0,
                TRUE ~ lower
            )
        ) %>%
        rename("mean_{{ sum_var }}" := mean,
               "sd_{{ sum_var }}" := sd)
}

#' @describeIn sensspec Calculate mean sensitivity over specificity
#' @inheritParams calc_mean_perf
#' @export
calc_mean_roc <- function(sensspec_dat) {
    return(calc_mean_perf(sensspec_dat,
                          group_var = specificity,
                          sum_var = sensitivity)
           )
}

#' @describeIn sensspec Calculate mean precision over recall
#' @inheritParams calc_mean_perf
#' @export
calc_mean_prc <- function(sensspec_dat) {
    return(calc_mean_perf(sensspec_dat %>%
                              rename(recall = sensitivity),
                          group_var = recall,
                          sum_var = precision
                          )
           )
}

#' Calculate the fraction of positives, i.e. baseline precision
#'
#' @inheritParams get_outcome_type
#' @inheritParams run_ml
#' @inheritParams get_model_sensspec
#'
#' @return the baseline precision based on the fraction of positives
#' @export
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#'
#' calc_baseline_precision(otu_mini_bin, 'dx', 'cancer')
#'
calc_baseline_precision <- function(dataset, outcome_colname, pos_outcome) {
    outcome_tally <- dataset %>%
        dplyr::count(!!rlang::sym(outcome_colname))
    npos <- outcome_tally %>%
        dplyr::filter(!!rlang::sym(outcome_colname) == pos_outcome) %>%
        dplyr::pull(n)
    ntot <- outcome_tally %>%
        dplyr::pull(n) %>%
        sum()
    baseline_prec <- npos / ntot
    return(baseline_prec)
}
