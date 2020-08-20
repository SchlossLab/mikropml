test_that("define_cv works for 2-fold cv on otu_sm data", {
  expect_equal(
    define_cv(train_data_sm, "dx", 2, 100, 2019),
    otu_sm_cv5
  )
})

test_that("get_seeds_trainControl works", {
  set.seed(0)
  expect_equal(length(get_seeds_trainControl(2, 2, 2)), 2 * 2 + 1)
  expect_equal(sapply((get_seeds_trainControl(2, 2, 3)), length), c(rep(2, 4), 1))
  expect_equal(
    get_seeds_trainControl(2, 2, 3),
    list(c(330L, 1799L), c(1615L, 1749L), c(37L, 1129L), c(729L, 878L), 485L)
  )
})
