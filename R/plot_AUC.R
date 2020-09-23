#' Plot AUROC and AUPROC values for multiple datasplit ML runs
#'
#' @inheritParams run_ml
#'
#' @return ggplot boxplot 
#' @export
#' @author Begüm Topçuoglu, \email{topcuoglu.begum@@gmail.com}
#'
#' @examples
#' plot_AUC(performance_df)
plot_AUC <- function(performance_df) {
  
    tidy_performance <- performance_df  %>%
      melt_data()
    
    performance_plot <- ggplot2::ggplot(tidy_performance, aes(x=method, y=AUC)) +
      geom_jitter(aes(color = Performance),
                  size = 1.2,
                  position = position_jitterdodge(jitter.width = 0.2, 
                                                  dodge.width = 0.7)) +
      stat_summary(aes(color = Performance),
                   fun.data="mean_se",  
                   fun.args = list(mult=1),
                   geom = "pointrange",  size = 0.9,
                   position = position_dodge(0.7)) +
      geom_hline(yintercept = 0.5, linetype="dashed") +
      scale_y_continuous(name = "AUC",
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
    
    return(performance_plot)
}

#' Tidy the performance table
#'
#'
#' @return Tidy dataframe with model performance metrics 
#' @noRd
#' @author Begüm Topçuoglu, \email{topcuoglu.begum@@gmail.com}
#'
#' @examples
#' get_predictions(trained_model_sm1, test_data_sm, "cancer")
melt_data <-  function(perf_table) {
    data_melt <- perf_table %>%
      dplyr::select(-seed) %>% 
      reshape2::melt(measure.vars=c('cv_auroc', 'test_auroc', 'test_auprc')) %>%
      dplyr::mutate(Performance = dplyr::case_when(variable == "cv_auroc" ~ 'Cross-validation AUROC', variable == "test_auroc" ~ 'Testing AUROC', variable == "test_auprc" ~ 'Testing Precision-Recall Curve')) %>%
      dplyr::rename(AUC = value) %>%
      dplyr::group_by(Performance)
    return(data_melt)
}