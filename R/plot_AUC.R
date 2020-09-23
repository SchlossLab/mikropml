#' Plot performance metrics for multiple ML runs with different seeds
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
#' results_multi <- lapply(seq(100, 104), function(seed) {
#'   run_ml(otu_mini, 'regLogistic', seed = seed, kfold = 2)
#'   })
#' # extract and combine the performance results
#' perf_df <- lapply(results_multi, function(result) {
#'   result[['performance']]
#'   }) %>%
#'   dplyr::bind_rows()
#' # plot the performance results
#' plot_performance(perf_df)
#' }
plot_performance <- function(performance_df) {
  performance_df  %>%
    tidy_perf_data() %>%
    ggplot2::ggplot(aes(x=method, y=value)) +
      geom_jitter(aes(color = metric),
                  size = 1.2,
                  position = position_jitterdodge(jitter.width = 0.2,
                                                  dodge.width = 0.7)) +
      stat_summary(aes(color = metric),
                   fun.data="mean_se",
                   fun.args = list(mult=1),
                   geom = "pointrange",  size = 0.9,
                   position = position_dodge(0.7)) +
      geom_hline(yintercept = 0.5, linetype="dashed") +
      scale_y_continuous(name = "Performance",
                         breaks = seq(0.2, 1, 0.1),
                         limits=c(0.2,1),
                         expand=c(0,0)) +
      scale_x_discrete(name = "") +
      theme_bw() +
      theme(plot.margin=unit(c(0,1.1,0,0),"cm"),
            legend.justification=c(0,1),
            legend.position=c(0,1),
            legend.title = element_blank(),
            legend.background = element_rect(linetype="solid", color="black", size=0.4),
            legend.box.margin=margin(c(4,4,4, 4)),
            legend.text=element_text(size=8),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(size=1),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            text = element_text(size = 12),
            axis.text.x=element_text(size = 10, colour='black'),
            axis.text.y=element_text(size = 10, colour='black'),
            axis.title.y=element_text(size = 10),
            axis.title.x=element_text(size = 10),
            panel.border = element_rect(linetype="solid", colour = "black", fill=NA, size=1.5))
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
