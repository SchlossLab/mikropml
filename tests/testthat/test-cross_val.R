
hparams_list <- list(cost = c("1e-3", "1e-2", "1e-1"), epsilon = "0.01", loss = "L2_primal")
cv2_group <- c("A", "A", "B", "A", "A", "A", "D", "C", "A", "D", "C", "B",
               "A", "A", "B", "B", "A", "D", "C", "C", "B", "B", "C", "D", "C",
               "D", "C", "A", "C", "D", "A", "C", "C", "A", "D", "A", "B", "C",
               "C", "D", "B", "D", "B", "A", "C", "D", "B", "C", "D", "B", "A",
               "D", "C", "A", "B", "C", "B", "D", "A", "B", "B", "B", "A", "A",
               "D", "B", "A", "A", "A", "D", "A", "D", "C", "A", "D", "C", "A",
               "D", "B", "C", "C", "B", "D", "A", "B", "C", "B", "A", "A", "B",
               "D", "C", "B", "D", "C", "D", "D", "D", "C", "C", "A", "A", "A",
               "B", "B", "B", "A", "B", "D", "C", "C", "D", "D", "C", "B", "D",
               "D", "C", "A", "D", "B", "C", "A", "D", "D", "B", "C", "B", "D",
               "C", "B", "A", "C", "B", "B", "C", "A", "C", "B", "D", "B", "D",
               "C", "B")
outcome_type <- get_outcome_type(otu_mini %>% dplyr::pull('dx'))
class_probs <- outcome_type != "numeric"
perf_metric_function <- get_perf_metric_fn(outcome_type)

test_that("define_cv works for 2-fold cv on otu_mini training data with groups", {
  set.seed(2019)
  expect_equal(
    define_cv(otu_mini_results1$trained_model$trainingData,
              "dx",
              hparams_list,
              perf_metric_function,
              class_probs,
              kfold = 2,
              cv_times = 2,
              group = cv2_group
    ),
    otu_mini_cv2
  )
})

test_that("get_seeds_trainControl works", {
  set.seed(0)
  expect_equal(
    length(get_seeds_trainControl(hparams_list, 2, 2, 2)),
    2 * 2 + 1
  )
  expect_equal(
    sapply((get_seeds_trainControl(hparams_list, 2, 2, 3)), length),
    c(rep(3, 4), 1)
  )
  set.seed(0)
  expect_equal(
    get_seeds_trainControl(hparams_list, 2, 2, 3),
    list(c(1422L, 1017L, 679L), c(2177L, 930L, 1533L), c(
      471L, 2347L,
      270L
    ), c(1211L, 597L, 1301L), 1974L)
  )
})
