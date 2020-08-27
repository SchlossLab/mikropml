hparams_list <- list(
  cost = c("1e-3", "1e-2", "1e-1"),
  epsilon = "0.01",
  loss = "L2_primal"
)
test_that("define_cv works for 2-fold cv on otu_mini training data", {
  expect_equal(
    define_cv(train_data_mini, "dx", hparams_list, kfold = 2, cv_times = 100, seed = 2019),
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
