#' @noRd
benchmark_perm_functions <- function() {
  trained_model <- otu_mini_bin_results_glmnet$trained_model
  train_data <- trained_model$trainingData %>% dplyr::rename(dx = .outcome)
  test_data <- otu_mini_bin_results_glmnet$test_data
  outcome_colname <- 'dx'
  perf_metric_function <- caret::multiClassSummary
  perf_metric_name <- 'AUC'
  class_probs <- TRUE
  method <- 'glmnet'
  get_feat_imp1 <- function() {
      get_feature_importance(
          trained_model,
          train_data,
          test_data,
          outcome_colname,
          perf_metric_function,
          perf_metric_name,
          class_probs = TRUE,
          method = 'glmnet'
      )
  }
  get_feat_imp2 <- function() {
      get_feature_importance2(
          trained_model,
          train_data,
          test_data,
          outcome_colname,
          perf_metric_function,
          perf_metric_name,
          class_probs = TRUE,
          method = 'glmnet'
      )
  }
  bench_results <- microbenchmark::microbenchmark(
      get_feat_imp1(),
      get_feat_imp2(),
      times = 3L
  )
}

#' @noRd
benchmark_summaries <- function() {
    dplyr_sum <- function() {
        perms_df %>%
            group_by(feat_group) %>%
            summarize(
                perf_metric = mean(perf_metric),
                perf_metric_diff = test_perf_value - mean(perf_metric),
                pvalue = calc_pvalue(perf_metric, test_perf_value)
            ) %>%
            dplyr::relocate(names = feat_group, .after = pvalue) %>%
            dplyr::mutate(method = method,
                          perf_metric_name = perf_metric_name,
                          seed = seed)
    }
    datatable_sum <- function() {
        dt <- as.data.table(perms_df) %>%  # group_by & summarize
            .[, .(perf_metric = mean(perf_metric),
                  perf_metric_diff = test_perf_value - mean(perf_metric),
                  pvalue = calc_pvalue(perf_metric, test_perf_value)
                  ),
              by = "feat_group"]

        dt[, ':='(method = method,
                  perf_metric_name = perf_metric_name,
                  seed = seed)]
        setnames(dt, "feat_group", "names")
        setcolorder(dt, c("perf_metric", "perf_metric_diff", "pvalue", "names",
                          "method", "perf_metric_name", "seed"))
        return(as_tibble(dt))
    }
    microbenchmark::microbenchmark(dplyr_sum(), datatable_sum(), times = 10L)
}
