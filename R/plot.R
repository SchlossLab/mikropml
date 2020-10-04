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
#'   run_ml(otu_small, "regLogistic", seed = seed)
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
#'   methods = c("regLogistic", "rf")
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
  abort_packages_not_installed(check_packages_installed("ggplot2", "tidyr"))
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
#' @inheritParams plot_performance
#' @return Tidy dataframe with model performance metrics
#' @noRd
#' @author Begüm Topçuoglu, \email{topcuoglu.begum@@gmail.com}
#' @author Kelly Sovacool, \email(sovacool@@umich.edu)
tidy_perf_data <- function(performance_df) {
  abort_packages_not_installed(check_packages_installed("tidyr"))
  performance_df %>%
    dplyr::select(-.data$seed) %>%
    tidyr::pivot_longer(
      cols = c(.data$cv_auroc, .data$test_auroc, .data$test_auprc),
      names_to = "metric"
    ) %>%
    dplyr::mutate(
      metric = dplyr::case_when(
        metric == "cv_auroc" ~ "Cross-validation AUROC",
        metric == "test_auroc" ~ "Testing AUROC",
        metric == "test_auprc" ~ "Testing AUPRC"
      )
    )
}
