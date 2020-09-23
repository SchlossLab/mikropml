#' Plot performance metrics for multiple ML runs with different parameters
#'
#' @param performance_df dataframe of performance results from multiple calls to `run_ml()`
#'
#' @return a ggplot2 plot
#' @export
#' @author Begüm Topçuoglu, \email{topcuoglu.begum@@gmail.com}
#' @author Kelly Sovacool, \email(sovacool@@umich.edu)
#'
#' @examples
#' \dontrun{
#' # call `run_ml` multiple times with different seeds
#' results_lst <- lapply(seq(100, 104), function(seed) {
#'   run_ml(otu_mini, 'regLogistic', seed = seed, kfold = 2)
#'   })
#' # extract and combine the performance results
#' perf_df <- lapply(results_lst, function(result) {
#'   result[['performance']]
#'   }) %>%
#'   dplyr::bind_rows()
#' # plot the performance results
#' plot_performance(perf_df)
#'
#' # call `run_ml` with different ML methods
#' param_grid <- expand.grid(seeds=seq(100, 104),
#'                           methods=c('regLogistic', 'rf'))
#' results_mtx <- mapply(function(seed, method) {
#'   run_ml(otu_mini, method, seed = seed, kfold = 2)},
#'   param_grid$seeds, param_grid$methods)
#' # extract and combine the performance results
#' perf_df2 <- dplyr::bind_rows(results_mtx['performance',])
#' # plot the performance results
#' plot_performance(perf_df2)
#' }
plot_performance <- function(performance_df) {
  if (!check_package_installed('ggplot2')) {
    stop('`ggplot2` is required for `plot_performance()`, but you do not have it installed.')
  }
  performance_df  %>%
    tidy_perf_data() %>%
    ggplot2::ggplot(aes(x=method, y=value)) +
      stat_summary(aes(color = metric),
                   fun.data="mean_se",
                   fun.args = list(mult=1),
                   geom = "pointrange",
                   position = position_dodge(0.2)
                   ) +
      geom_hline(yintercept = 0.5, linetype="dashed") +
    ylim(0, 1) +
    labs(y='Performance', x='') +
    coord_flip()
}

#' Tidy the performance dataframe
#' @inheritParams plot_performance
#' @return Tidy dataframe with model performance metrics
#' @noRd
#' @author Begüm Topçuoglu, \email{topcuoglu.begum@@gmail.com}
#' @author Kelly Sovacool, \email(sovacool@@umich.edu)
tidy_perf_data <- function(performance_df) {
  performance_df %>%
    dplyr::select(-seed) %>%
    tidyr::pivot_longer(cols = c(cv_auroc, test_auroc, test_auprc),
                        names_to = "metric") %>%
    dplyr::mutate(
      metric_pretty = dplyr::case_when(
        metric == "cv_auroc" ~ 'Cross-validation AUROC',
        metric == "test_auroc" ~ 'Testing AUROC',
        metric == "test_auprc" ~ 'Testing Precision-Recall Curve'
      )
    )
}
