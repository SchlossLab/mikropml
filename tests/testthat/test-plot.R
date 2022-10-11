perf_df_untidy <- structure(list(cv_metric_AUC = c(
  0.626118719362745, 0.631857444852941,
  0.660083976715686, 0.638769209558824, 0.657143321078431
), logLoss = c(
  0.686781644602582,
  0.683505908139307, 0.687957870274247, 0.688183575172013, 0.688869161857159
), AUC = c(
  0.634210526315789, 0.652631578947368, 0.589473684210526,
  0.573684210526316, 0.586842105263158
), prAUC = c(
  0.565824790303497,
  0.61663092781822, 0.545144586653095, 0.555803529302002, 0.533415969473769
), Accuracy = c(
  0.615384615384615, 0.58974358974359, 0.615384615384615,
  0.512820512820513, 0.641025641025641
), Kappa = c(
  0.225165562913907,
  0.168, 0.22103861517976, 0.0159362549800795, 0.275862068965517
), F1 = c(
  0.666666666666667, 0.68, 0.693877551020408, 0.595744680851064,
  0.695652173913043
), Sensitivity = c(0.75, 0.85, 0.85, 0.7, 0.8), Specificity = c(
  0.473684210526316, 0.315789473684211, 0.368421052631579,
  0.315789473684211, 0.473684210526316
), Pos_Pred_Value = c(
  0.6,
  0.566666666666667, 0.586206896551724, 0.518518518518518, 0.615384615384615
), Neg_Pred_Value = c(
  0.642857142857143, 0.666666666666667, 0.7,
  0.5, 0.692307692307692
), Precision = c(
  0.6, 0.566666666666667,
  0.586206896551724, 0.518518518518518, 0.615384615384615
), Recall = c(
  0.75,
  0.85, 0.85, 0.7, 0.8
), Detection_Rate = c(
  0.384615384615385,
  0.435897435897436, 0.435897435897436, 0.358974358974359, 0.41025641025641
), Balanced_Accuracy = c(
  0.611842105263158, 0.582894736842105,
  0.609210526315789, 0.507894736842105, 0.636842105263158
), method = c(
  "glmnet",
  "glmnet", "glmnet", "glmnet", "glmnet"
), seed = c(
  100, 101, 102,
  103, 104
)), row.names = c(NA, -5L), class = c("tbl_df", "tbl", "data.frame"))

perf_df_tidy <- dplyr::tibble(
  method = c(
    "glmnet", "glmnet", "glmnet", "glmnet",
    "glmnet", "glmnet", "glmnet", "glmnet", "glmnet", "glmnet"
  ),
  metric = c(
    "CV AUC", "Test AUC", "CV AUC", "Test AUC", "CV AUC",
    "Test AUC", "CV AUC", "Test AUC", "CV AUC", "Test AUC"
  ),
  value = c(
    0.626118719362745, 0.634210526315789, 0.631857444852941,
    0.652631578947368, 0.660083976715686, 0.589473684210526,
    0.638769209558824, 0.573684210526316, 0.657143321078431,
    0.586842105263158
  )
)

test_that("tidy_perf_data works", {
  expect_equal(tidy_perf_data(perf_df_untidy), perf_df_tidy)
})

test_that("plot_model_performance creates a boxplot from tidied data", {
  p <- perf_df_untidy %>% plot_model_performance()
  expect_invisible(print(p))
  expect_equal(p$data, perf_df_untidy %>% tidy_perf_data())
  expect_equal(
    p$layers[[1]]$geom %>% class() %>% as.vector(),
    c("GeomBoxplot", "Geom", "ggproto", "gg")
  )
})

test_that("get_hp_performance works", {
  expect_equal(
    get_hp_performance(otu_mini_bin_results_glmnet$trained_model),
    list(dat = structure(list(
      alpha = c(0L, 0L, 0L, 0L, 0L, 0L),
      lambda = c(1e-04, 0.001, 0.01, 0.1, 1, 10), AUC = c(
        0.608255208333333,
        0.608255208333333, 0.608645833333333, 0.616678921568627,
        0.622173713235294, 0.618740808823529
      )
    ), row.names = c(
      NA,
      6L
    ), class = "data.frame"), params = "lambda", metric = "AUC")
  )
})

test_that("combine_hp_performance works", {
  expect_equal(
    combine_hp_performance(list(otu_mini_bin_results_glmnet$trained_model, otu_mini_bin_results_glmnet$trained_model)),
    list(dat = structure(list(alpha = c(
      0L, 0L, 0L, 0L, 0L, 0L, 0L,
      0L, 0L, 0L, 0L, 0L
    ), lambda = c(
      1e-04, 0.001, 0.01, 0.1, 1, 10,
      1e-04, 0.001, 0.01, 0.1, 1, 10
    ), AUC = c(
      0.608255208333333, 0.608255208333333,
      0.608645833333333, 0.616678921568627, 0.622173713235294, 0.618740808823529,
      0.608255208333333, 0.608255208333333, 0.608645833333333, 0.616678921568627,
      0.622173713235294, 0.618740808823529
    )), row.names = c(NA, -12L), class = "data.frame"), params = "lambda", metric = "AUC")
  )
})
