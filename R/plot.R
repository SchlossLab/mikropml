#' Plot performance metrics for multiple ML runs with different parameters
#'
#' ggplot2 is required to use this function.
#'
#' @param performance_df dataframe of performance results from multiple calls to `run_ml()`
#'
#' @return a ggplot2 plot
#' @export
#' @author Begüm Topçuoglu, \email{topcuoglu.begum@@gmail.com}
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' \dontrun{
#' # call `run_ml()` multiple times with different seeds
#' results_lst <- lapply(seq(100, 104), function(seed) {
#'   run_ml(otu_small, "glmnet", seed = seed)
#' })
#' # extract and combine the performance results
#' perf_df <- lapply(results_lst, function(result) {
#'   result[["performance"]]
#' }) %>%
#'   dplyr::bind_rows()
#' # plot the performance results
#' p <- plot_performance(perf_df)
#'
#' # call `run_ml()` with different ML methods
#' param_grid <- expand.grid(
#'   seeds = seq(100, 104),
#'   methods = c("glmnet", "rf")
#' )
#' results_mtx <- mapply(
#'   function(seed, method) {
#'     run_ml(otu_mini, method, seed = seed, kfold = 2)
#'   },
#'   param_grid$seeds, param_grid$methods
#' )
#' # extract and combine the performance results
#' perf_df2 <- dplyr::bind_rows(results_mtx["performance", ])
#' # plot the performance results
#' p <- plot_performance(perf_df2)
#'
#' # you can continue adding layers to customize the plot
#' p +
#'   theme_classic() +
#'   scale_color_brewer(palette = "Dark2") +
#'   coord_flip()
#' }
plot_performance <- function(performance_df) {
  abort_packages_not_installed("ggplot2", "tidyr")
  performance_df %>%
    tidy_perf_data() %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$method, y = .data$value, color = .data$metric)) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed") +
    ggplot2::ylim(0, 1) +
    ggplot2::labs(y = "Performance", x = NULL) +
    ggplot2::theme(legend.title = ggplot2::element_blank())
}

#' Tidy the performance dataframe
#'
#' Used by `plot_performance()`.
#'
#' @inheritParams plot_performance
#' @return Tidy dataframe with model performance metrics
#' @export
#' @author Begüm Topçuoglu, \email{topcuoglu.begum@@gmail.com}
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#' @examples
#' \dontrun{
#' # call `run_ml()` multiple times with different seeds
#' results_lst <- lapply(seq(100, 104), function(seed) {
#'   run_ml(otu_small, "glmnet", seed = seed)
#' })
#' # extract and combine the performance results
#' perf_df <- lapply(results_lst, function(result) {
#'   result[["performance"]]
#' }) %>%
#'   dplyr::bind_rows()
#' # make it pretty!
#' tidy_perf_data(perf_df)
#' }
tidy_perf_data <- function(performance_df) {
  abort_packages_not_installed("tidyr")
  performance_df %>%
    dplyr::select(-.data$seed) %>%
    tidyr::pivot_longer(
      cols = c(.data$cv_auroc, .data$test_auroc, .data$test_auprc),
      names_to = "metric"
    ) %>%
    dplyr::mutate(
      metric = dplyr::case_when(
        # TODO: these metrics are hard-coded, but they can change depending on the input data. need to make them dynamic.
        metric == "cv_auroc" ~ "Cross-validation AUROC",
        metric == "test_auroc" ~ "Testing AUROC",
        metric == "test_auprc" ~ "Testing AUPRC"
      )
    )
}

#' Get hyperparameter peformance metrics
#'
#' @param trained_model trained model (e.g. from `run_ml()`)
#'
#' @return list: dat - dataframe of performance metric for each group of hyperparameters, params - hyperparameters tuned, metric - performance metric used
#' @export
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#' @author Kelly Sovacool \email{sovacool@@umich.edu}
#'
#' @examples
#' get_hp_performance(otu_mini_results1$trained_model)
get_hp_performance <- function(trained_model){
  metric <- trained_model$metric
  dat <- trained_model$results %>%
    dplyr::select(dplyr::all_of(trained_model$modelInfo$parameters$parameter),
                  dplyr::all_of(metric))
  params <- sapply(dat, function(x) length(unique(x)) > 1) %>%
    Filter(isTRUE, .) %>%
    names() %>%
    Filter(function(x) x != metric, .)
  return(list(
    dat = dat,
    params = params,
    metric = metric
  ))
}

#' Combine hyperparameter performance metrics for multiple train/test splits
#'
#' @param trained_model_lst list of trained models
#'
#' @return list: dat - dataframe of performance metric for each group of hyperparameters, params - hyperparameters tuned, metric - performance metric used
#' @export
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' \dontrun{
#' results <- lapply(seq(100, 102), function(seed) {
#'   run_ml(otu_small, "glmnet", seed = seed)
#'   })
#' models <- lapply(results, function(x) x$trained_model)
#' combine_hp_performance(models)
#' }
combine_hp_performance <- function(trained_model_lst){
  abort_packages_not_installed('purrr')
  # TODO: can we do this without purrr so we don't have to add a new dep?
  dat_params <- lapply(trained_model_lst, function(x) get_hp_performance(x)) %>%
    purrr::transpose()
  dat <- dplyr::bind_rows(dat_params$dat)
  return(list(dat = dat,
              params = unique(unlist(dat_params$params)),
              metric = unique(unlist(dat_params$metric))))
}

#' Plot hyperparameter performance metrics
#'
#' @param dat dataframe of hyperparameters and performance metric (e.g. from `get_hp_performance()` or `combine_hp_performance()`)
#' @param param_col hyperparameter to be plotted. must be a column in `dat`.
#' @param metric_col performance metric. must be a column in `dat`.
#'
#' @export
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#' @author Kelly Sovacool \email{sovacool@@umich.edu}
#'
#' @examples
#' # plot for a single `run_ml()` call
#' hp_metrics <- get_hp_performance(otu_mini_results1$trained_model)
#' hp_metrics
#' plot_hp_performance(hp_metrics$dat, lambda, AUC)
#'
#' \dontrun{
#' # plot for multiple `run_ml()` calls
#' results <- lapply(seq(100, 102), function(seed) {
#'   run_ml(otu_small, "glmnet", seed = seed)
#'   })
#' models <- lapply(results, function(x) x$trained_model)
#' hp_metrics <- combine_hp_performance(models)
#' plot_hp_performance(hp_metrics$dat, lambda, AUC)
#' }
plot_hp_performance <- function(dat, param_col, metric_col) {
  abort_packages_not_installed('ggplot2')
  return(dat %>%
          ggplot2::ggplot(ggplot2::aes(group = {{ param_col }},
                                       y = {{ metric_col }})) +
           ggplot2::geom_boxplot())
}

