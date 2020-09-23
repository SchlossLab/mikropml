#Still working on it. THis is the skeleton.
melt_data <-  function(data) {
  data_melt <- data %>%
    select(-seed)
    melt(measure.vars=c('cv_auroc', 'test_auroc', 'test_auprc')) %>%
    mutate(Performance = case_when(variable == "cv_auroc" ~ 'Cross-validation AUROC', variable == "test_auroc" ~ 'Testing AUROC', variable == "test_auprc" ~ 'Testing Precision-Recall Curve')) %>%
    rename(AUC = value) %>%
    group_by(Performance)
  return(data_melt)
}
# -------------------------------------------------------------------->

tidy_performance <- perf_table  %>%
  melt_data()

performance_plot <- ggplot(tidy_performance, aes(x=method, y=AUC)) +
  geom_jitter(
    aes(color = Performance),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.7),
    size = 1.2
    ) +
    stat_summary(
      aes(color = Performance),
      fun.data="mean_se",  fun.args = list(mult=1),
      geom = "pointrange",  size = 0.9,
      position = position_dodge(0.7)
      )+
    geom_hline(yintercept = 0.5, linetype="dashed") +
    scale_y_continuous(name = "AUC",
                       breaks = seq(0.2, 1, 0.1),
                       limits=c(0.2,1),
                       expand=c(0,0)) +
   labs(title = paste0(level)) +
    scale_x_discrete(name = "") +
    theme_bw() +
    theme(plot.margin=unit(c(0,1.1,0,0),"cm"),
          legend.justification=c(0,1),
          legend.position=c(0,1),
          #legend.position="bottom",
          legend.title = element_blank(),
          legend.background = element_rect(linetype="solid", color="black", size=0.5),
          legend.box.margin=margin(c(10,10,10, 10)),
          legend.text=element_text(size=10),
          #legend.title=element_text(size=22),
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
