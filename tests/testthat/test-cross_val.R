
hparams_list <- list(lambda = c("1e-3", "1e-2", "1e-1"), alpha = "0.01")
outcome_type <- get_outcome_type(otu_mini_bin %>% dplyr::pull("dx"))
class_probs <- outcome_type != "numeric"
perf_metric_function <- get_perf_metric_fn(outcome_type)
cv_group <- c(
  "C", "D", "E", "C", "D", "E", "A", "D", "A", "D", "D", "A",
  "B", "E", "D", "A", "D", "E", "B", "E", "A", "B", "A", "E", "A",
  "D", "A", "D", "A", "C", "A", "B", "B", "E", "A", "E", "B", "C",
  "D", "D", "C", "A", "E", "E", "B", "B", "A", "C", "D", "D", "D",
  "D", "A", "D", "C", "A", "D", "D", "B", "C", "E", "C", "E", "C",
  "B", "D", "B", "D", "C", "B", "B", "B", "B", "B", "B", "B", "C",
  "D", "D", "E", "A", "E", "D", "E", "A", "D", "A", "E", "E", "C",
  "B", "B", "E", "B", "C", "C", "D", "A", "A", "E", "E", "C", "A",
  "C", "E", "A", "D", "A", "C", "D", "E", "E", "A", "A", "B", "E",
  "C", "B", "B", "C", "C", "D", "C", "E", "E", "E", "C", "E", "D",
  "D", "B", "B", "B", "E", "E", "A", "A", "A", "B", "D", "B", "D",
  "B", "B", "B", "D", "B", "B", "D", "B", "D", "C", "C", "B", "A",
  "A", "D", "C", "E", "E", "A"
)

test_that("define_cv works on otu_mini training data with groups", {
  set.seed(2019)
  expect_equal(expect_message(
    define_cv(otu_mini_bin_results_rf$trained_model$trainingData,
      "dx",
      hparams_list,
      perf_metric_function,
      class_probs = class_probs,
      cv_times = 2,
      groups = cv_group), 'Groups will be kept together in CV partitions'
    ),
    otu_mini_cv
  )

  expect_message(
  define_cv(
    otu_mini_bin_results_rf$trained_model$trainingData,
    ".outcome",
    hparams_list,
    perf_metric_function,
    class_probs = class_probs,
    kfold = 5,
    cv_times = 2,
    groups = cv_group,
    group_partitions = list(train = c('A', 'B', 'C', 'D'), test = c('E'))
  ),
  'Groups will not be kept together in CV partitions because the number of groups in the training set is not larger than `kfold`'
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

test_that("create_grouped_k_multifolds keeps groups together in CV partitions", {
  set.seed(0)
  group <- c("A", "B", "A", "B", "C", "C", "A", "A", "D")
  folds <- create_grouped_k_multifolds(group, kfold = 2, cv_times = 2)

  expect_equal(folds, list(Fold1.Rep1 = c(1L, 3L, 5L, 6L, 7L, 8L), Fold2.Rep1 = c(
    2L,
    4L, 9L
  ), Fold1.Rep2 = c(2L, 4L), Fold2.Rep2 = c(
    1L, 3L, 5L, 6L,
    7L, 8L, 9L
  )))

  fold_grps <- sapply(folds, function(x) group[x])
  expect_false(any(fold_grps$Fold1.Rep1 %in% fold_grps$Fold2.Rep1))
  expect_false(any(fold_grps$Fold1.Rep2 %in% fold_grps$Fold2.Rep2))

  set.seed(5)
  expect_error(create_grouped_k_multifolds(group, kfold = 2, cv_times = 2),
               "Could not split the data into train and validate folds")
})
