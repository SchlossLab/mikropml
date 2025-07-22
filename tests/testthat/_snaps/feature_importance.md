# feature importances are correct when tibbles used

    Code
      get_feature_importance(otu_mini_bin_results_glmnet$trained_model,
      otu_mini_bin_results_glmnet$test_data %>% dplyr::as_tibble(), "dx", caret::multiClassSummary,
      "AUC", TRUE, "glmnet", seed = 2019, corr_thresh = 1)
    Output
         perf_metric perf_metric_diff     pvalue     lower     upper     feat method
      1    0.6291579      0.018210526 0.20792079 0.5815789 0.6684211 Otu00001 glmnet
      2    0.6054737      0.041894737 0.07920792 0.5315789 0.6578947 Otu00002 glmnet
      3    0.6387895      0.008578947 0.18811881 0.6184211 0.6526316 Otu00003 glmnet
      4    0.6367632      0.010605263 0.37623762 0.5815789 0.6947368 Otu00004 glmnet
      5    0.6294474      0.017921053 0.38613861 0.4947368 0.7184211 Otu00005 glmnet
      6    0.6378684      0.009500000 0.35643564 0.5947368 0.6789474 Otu00006 glmnet
      7    0.6425526      0.004815789 0.28712871 0.6236842 0.6578947 Otu00007 glmnet
      8    0.5921579      0.055210526 0.11881188 0.5210526 0.6605263 Otu00008 glmnet
      9    0.6396842      0.007684211 0.21782178 0.6236842 0.6526316 Otu00009 glmnet
      10   0.6375263      0.009842105 0.43564356 0.5552632 0.7210526 Otu00010 glmnet
         perf_metric_name seed
      1               AUC 2019
      2               AUC 2019
      3               AUC 2019
      4               AUC 2019
      5               AUC 2019
      6               AUC 2019
      7               AUC 2019
      8               AUC 2019
      9               AUC 2019
      10              AUC 2019

# custom grouped features works

    Code
      get_feature_importance(otu_mini_bin_results_glmnet$trained_model,
      otu_mini_bin_results_glmnet$test_data %>% dplyr::as_tibble(), "dx", caret::multiClassSummary,
      "AUC", TRUE, "glmnet", seed = 2019, groups = groups)
    Output
        perf_metric perf_metric_diff     pvalue     lower     upper
      1   0.6291579      0.018210526 0.20792079 0.5815789 0.6684211
      2   0.5969211      0.050447368 0.25742574 0.4631579 0.7000000
      3   0.6336053      0.013763158 0.32673267 0.5842105 0.6789474
      4   0.6391053      0.008263158 0.36633663 0.6000000 0.6684211
      5   0.6424211      0.004947368 0.33663366 0.6236842 0.6578947
      6   0.5968421      0.050526316 0.08910891 0.5236842 0.6710526
      7   0.6402895      0.007078947 0.20792079 0.6236842 0.6526316
      8   0.6298684      0.017500000 0.32673267 0.5605263 0.7026316
                              feat method perf_metric_name seed
      1                   Otu00001 glmnet              AUC 2019
      2 Otu00002|Otu00003|Otu00005 glmnet              AUC 2019
      3                   Otu00004 glmnet              AUC 2019
      4                   Otu00006 glmnet              AUC 2019
      5                   Otu00007 glmnet              AUC 2019
      6                   Otu00008 glmnet              AUC 2019
      7                   Otu00009 glmnet              AUC 2019
      8                   Otu00010 glmnet              AUC 2019

