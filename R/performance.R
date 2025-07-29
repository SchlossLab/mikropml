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
calc_perf_metrics <- function(test_data, trained_model, outcome_colname,
                              perf_metric_function, class_probs,
                              pos_class = NULL) {
  pred_type <- "raw"
  if (class_probs) pred_type <- "prob"
  preds <- stats::predict(trained_model, test_data, type = pred_type)
  obs <- test_data %>% dplyr::pull(outcome_colname)
  if (class_probs) {
    if (is.factor(obs)) {
      uniq_obs <- obs %>% levels()
    } else {
      uniq_obs <- unique(c(
        pos_class,
        test_data %>% dplyr::pull(outcome_colname),
        as.character(trained_model$pred$obs)
      ))
      obs <- factor(test_data %>% dplyr::pull(outcome_colname), levels = uniq_obs)
    }
    # TODO refactor this line
    pred_class <- factor(names(preds)[apply(preds, 1, which.max)], levels = uniq_obs)
    perf_met <- perf_metric_function(data.frame(obs = obs, pred = pred_class, preds), lev = uniq_obs)
  } else {
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
  cv_metric <- NULL
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
      cv_metric
    ) %>%
    change_to_num())
}

#' Calculate a bootstrap confidence interval for the performance on a single train/test split
#'
#' Uses [rsample::bootstraps()], [rsample::int_pctl()], and [furrr::future_map()]
#'
#' @param ml_result result returned from a single [run_ml()] call
#' @inheritParams run_ml
#' @param bootstrap_times the number of boostraps to create (default: `10000`)
#' @param alpha the alpha level for the confidence interval (default `0.05` to create a 95% confidence interval)
#'
#' @return a data frame with an estimate (`.estimate`), lower bound (`.lower`),
#'  and upper bound (`.upper`) for each performance metric (`term`).
#' @export
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' bootstrap_performance(otu_mini_bin_results_glmnet, "dx",
#'   bootstrap_times = 10, alpha = 0.10
#' )
#' \dontrun{
#' outcome_colname <- "dx"
#' run_ml(otu_mini_bin, "rf", outcome_colname = "dx") %>%
#'   bootstrap_performance(outcome_colname,
#'     bootstrap_times = 10000,
#'     alpha = 0.05
#'   )
#' }
bootstrap_performance <- function(ml_result,
                                  outcome_colname,
                                  bootstrap_times = 10000,
                                  alpha = 0.05) {
  abort_packages_not_installed("assertthat", "rsample", "furrr")
  splits <- perf <- NULL

  model <- ml_result$trained_model
  test_dat <- ml_result$test_data
  outcome_type <- get_outcome_type(test_dat %>% dplyr::pull(outcome_colname))
  class_probs <- outcome_type != "continuous"
  method <- model$modelInfo$label
  seed <- ml_result$performance %>% dplyr::pull(seed)
  assertthat::are_equal(length(seed), 1)
  return(
    rsample::bootstraps(test_dat, times = bootstrap_times) %>%
      dplyr::mutate(perf = furrr::future_map(
        splits,
        ~ calc_perf_bootstrap_split(
          .x,
          trained_model = model,
          outcome_colname = outcome_colname,
          perf_metric_function = get_perf_metric_fn(outcome_type),
          perf_metric_name = model$metric,
          class_probs = outcome_type != "continuous",
          method = model$trained_model$modelInfo$label,
          seed = seed
        )
      )) %>%
      rsample::int_pctl(perf, alpha = alpha)
  )
}

#' Calculate performance for a single split from [rsample::bootstraps()]
#'
#' Used by [bootstrap_performance()].
#'
#' @param test_data_split a single bootstrap of the test set from [rsample::bootstraps()]
#' @inheritParams get_performance_tbl
#' @return a long data frame of performance metrics for [rsample::int_pctl()]
#'
#' @keywords internal
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
calc_perf_bootstrap_split <- function(test_data_split,
                                      trained_model,
                                      outcome_colname,
                                      perf_metric_function,
                                      perf_metric_name,
                                      class_probs,
                                      method,
                                      seed) {
  abort_packages_not_installed("rsample")
  return(
    get_performance_tbl(
      trained_model,
      rsample::analysis(test_data_split),
      outcome_colname,
      perf_metric_function,
      perf_metric_name,
      class_probs,
      method,
      seed
    ) %>%
      dplyr::select(-dplyr::all_of(c(method)), -seed) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) %>%
      tidyr::pivot_longer(
        dplyr::everything(),
        names_to = "term",
        values_to = "estimate"
      )
  )
}


#' @describeIn sensspec Get sensitivity, specificity, and precision for a model.
#'
#' @inheritParams calc_perf_metrics
#' @inheritParams run_ml
#'
#' @export
calc_model_sensspec <- function(trained_model, test_data, outcome_colname = NULL) {
  # adapted from https://github.com/SchlossLab/2021-08-09_ROCcurves/blob/8e62ff8b6fe1b691450c953a9d93b2c11ce3369a/ROCcurves.Rmd#L95-L109
  outcome_colname <- check_outcome_column(test_data, outcome_colname)
  pos_outcome <- trained_model$levels[1]
  actual <- is_pos <- tp <- fp <- fpr <- NULL
  probs <- stats::predict(trained_model,
    newdata = test_data,
    type = "prob"
  ) %>%
    dplyr::mutate(actual = test_data %>%
      dplyr::pull(outcome_colname))

  total <- probs %>%
    dplyr::count(actual) %>%
    tidyr::pivot_wider(names_from = "actual", values_from = "n") %>%
    as.list()

  neg_outcome <- names(total) %>%
    # assumes binary outcome
    Filter(function(x) {
      x != pos_outcome
    }, .)

  sensspec <- probs %>%
    dplyr::arrange(dplyr::desc(!!rlang::sym(pos_outcome))) %>%
    dplyr::mutate(is_pos = actual == pos_outcome) %>%
    dplyr::mutate(
      tp = cumsum(is_pos),
      fp = cumsum(!is_pos),
      sensitivity = tp / total[[pos_outcome]],
      fpr = fp / total[[neg_outcome]]
    ) %>%
    dplyr::mutate(
      specificity = 1 - fpr,
      precision = tp / (tp + fp)
    ) %>%
    dplyr::select(-is_pos)
  return(sensspec)
}

#' Generic function to calculate mean performance curves for multiple models
#'
#' Used by `calc_mean_roc()` and `calc_mean_prc()`.
#'
#' @param sensspec_dat data frame created by concatenating results of
#'   `calc_model_sensspec()` for multiple models.
#' @param group_var variable to group by (e.g. specificity or recall).
#' @param sum_var variable to summarize (e.g. sensitivity or precision).
#'
#' @return data frame with mean & standard deviation of `sum_var` summarized over `group_var`
#' @export
#'
#' @author Courtney Armour
#' @author Kelly Sovacool
calc_mean_perf <- function(sensspec_dat,
                           group_var = specificity,
                           sum_var = sensitivity) {
  # adapted from https://github.com/SchlossLab/2021-08-09_ROCcurves/blob/8e62ff8b6fe1b691450c953a9d93b2c11ce3369a/ROCcurves.Rmd#L166-L209
  specificity <- sensitivity <- sd <- NULL
  sensspec_dat %>%
    dplyr::mutate({{ group_var }} := round({{ group_var }}, 2)) %>%
    dplyr::group_by({{ group_var }}) %>%
    dplyr::summarise(
      mean = mean({{ sum_var }}),
      sd = stats::sd({{ sum_var }})
    ) %>%
    dplyr::mutate(
      upper = mean + sd,
      lower = mean - sd,
      upper = dplyr::case_when(
        upper > 1 ~ 1,
        TRUE ~ upper
      ),
      lower = dplyr::case_when(
        lower < 0 ~ 0,
        TRUE ~ lower
      )
    ) %>%
    dplyr::rename(
      "mean_{{ sum_var }}" := mean,
      "sd_{{ sum_var }}" := sd
    )
}

#' @describeIn sensspec Calculate mean sensitivity over specificity for multiple models
#' @inheritParams calc_mean_perf
#' @export
calc_mean_roc <- function(sensspec_dat) {
  specificity <- sensitivity <- NULL
  return(calc_mean_perf(sensspec_dat,
    group_var = specificity,
    sum_var = sensitivity
  ))
}

#' @describeIn sensspec Calculate mean precision over recall for multiple models
#' @inheritParams calc_mean_perf
#' @export
calc_mean_prc <- function(sensspec_dat) {
  sensitivity <- recall <- precision <- NULL
  return(calc_mean_perf(
    sensspec_dat %>%
      dplyr::rename(recall = sensitivity),
    group_var = recall,
    sum_var = precision
  ))
}

#' @name sensspec
#' @title Calculate and summarize performance for ROC and PRC plots
#' @description Use these functions to calculate cumulative sensitivity,
#'   specificity, recall, etc. on single models, concatenate the results
#'   together from multiple models, and compute mean ROC and PRC.
#'   You can then plot mean ROC and PRC curves to visualize the results.
#'   **Note**: These functions assume a binary outcome.
#'
#' @return data frame with summarized performance
#'
#' @author Courtney Armour
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' # get cumulative performance for a single model
#' sensspec_1 <- calc_model_sensspec(
#'   otu_mini_bin_results_glmnet$trained_model,
#'   otu_mini_bin_results_glmnet$test_data,
#'   "dx"
#' )
#' head(sensspec_1)
#'
#' # get performance for multiple models
#' get_sensspec_seed <- function(seed) {
#'   ml_result <- run_ml(otu_mini_bin, "glmnet", seed = seed)
#'   sensspec <- calc_model_sensspec(
#'     ml_result$trained_model,
#'     ml_result$test_data,
#'     "dx"
#'   ) %>%
#'     dplyr::mutate(seed = seed)
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
#' roc_dat %>% plot_mean_roc()
#' baseline_prec <- calc_baseline_precision(otu_mini_bin, "dx", "cancer")
#' prc_dat %>%
#'   plot_mean_prc(baseline_precision = baseline_prec)
#'
#' # balanced precision
#' prior <- calc_baseline_precision(otu_mini_bin,
#'   outcome_colname = "dx",
#'   pos_outcome = "cancer"
#' )
#' bprc_dat <- sensspec_dat %>%
#'   dplyr::mutate(balanced_precision = calc_balanced_precision(precision, prior)) %>%
#'   dplyr::rename(recall = sensitivity) %>%
#'   calc_mean_perf(group_var = recall, sum_var = balanced_precision)
#' bprc_dat %>% plot_mean_prc(ycol = mean_balanced_precision) + ylab("Mean Bal. Precision")
#' }
NULL

#' Calculate the fraction of positives, i.e. baseline precision for a PRC curve
#'
#' @inheritParams get_outcome_type
#' @inheritParams run_ml
#' @inheritParams calc_model_sensspec
#' @param pos_outcome the positive outcome from `outcome_colname`,
#'   e.g. "cancer" for the `otu_mini_bin` dataset.
#'
#' @return the baseline precision based on the fraction of positives
#' @export
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' # calculate the baseline precision
#' data.frame(y = c("a", "b", "a", "b")) %>%
#'   calc_baseline_precision(
#'     outcome_colname = "y",
#'     pos_outcome = "a"
#'   )
#'
#'
#' calc_baseline_precision(otu_mini_bin,
#'   outcome_colname = "dx",
#'   pos_outcome = "cancer"
#' )
#'
#'
#' # if you're not sure which outcome was used as the 'positive' outcome during
#' # model training, you can access it from the trained model and pass it along:
#' calc_baseline_precision(otu_mini_bin,
#'   outcome_colname = "dx",
#'   pos_outcome = otu_mini_bin_results_glmnet$trained_model$levels[1]
#' )
#'
calc_baseline_precision <- function(dataset,
                                    outcome_colname = NULL,
                                    pos_outcome = NULL) {
  outcome_colname <- check_outcome_column(dataset, outcome_colname)
  npos <- dataset %>%
    dplyr::filter(!!rlang::sym(outcome_colname) == pos_outcome) %>%
    nrow()
  ntot <- dataset %>% nrow()
  baseline_prec <- npos / ntot
  return(baseline_prec)
}

#' Calculate balanced precision given actual and baseline precision
#'
#' Implements Equation 1 from Wu _et al._ 2021 \doi{10.1016/j.ajhg.2021.08.012}.
#' It is the same as Equation 7 if `AUPRC` (aka `prAUC`) is used in place of `precision`.
#'
#' @param precision actual precision of the model.
#' @param prior baseline precision, aka frequency of positives.
#'   Can be calculated with [calc_baseline_precision]
#'
#' @return the expected precision if the data were balanced
#' @export
#' @author Kelly Sovacool \email{sovacool@@umich.edu}
#'
#' @examples
#' prior <- calc_baseline_precision(otu_mini_bin,
#'   outcome_colname = "dx",
#'   pos_outcome = "cancer"
#' )
#' calc_balanced_precision(otu_mini_bin_results_rf$performance$Precision, prior)
#'
#' otu_mini_bin_results_rf$performance %>%
#'   dplyr::mutate(
#'     balanced_precision = calc_balanced_precision(Precision, prior),
#'     aubprc = calc_balanced_precision(prAUC, prior)
#'   ) %>%
#'   dplyr::select(AUC, Precision, balanced_precision, aubprc)
#'
#' # cumulative performance for a single model
#' sensspec_1 <- calc_model_sensspec(
#'   otu_mini_bin_results_glmnet$trained_model,
#'   otu_mini_bin_results_glmnet$test_data,
#'   "dx"
#' )
#' head(sensspec_1)
#' prior <- calc_baseline_precision(otu_mini_bin,
#'   outcome_colname = "dx",
#'   pos_outcome = "cancer"
#' )
#' sensspec_1 %>%
#'   dplyr::mutate(balanced_precision = calc_balanced_precision(precision, prior)) %>%
#'   dplyr::rename(recall = sensitivity) %>%
#'   calc_mean_perf(group_var = recall, sum_var = balanced_precision) %>%
#'   plot_mean_prc(ycol = mean_balanced_precision)
calc_balanced_precision <-
  function(precision, prior) {
    return(
      precision * (1 - prior) / (
        precision * (1 - prior) + (1 - precision) * prior
      )
    )
  }
