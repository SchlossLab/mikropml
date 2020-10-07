hparams_list <- list(
  cost = c("1e-3", "1e-2", "1e-1"),
  epsilon = "0.01",
  loss = "L2_primal"
)

test_that("define_cv works for 2-fold cv on otu_mini training data with groups", {
  set.seed(2019)
  expect_equal(
    define_cv(otu_mini_results1$trained_model$trainingData,
      "dx",
      hparams_list,
      caret::multiClassSummary,
      class_probs = TRUE,
      kfold = 2,
      cv_times = 2,
      seed = 2019,
      group = sample(LETTERS[1:4],
        nrow(otu_mini_results1$trained_model$trainingData),
        replace = TRUE
      )
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
