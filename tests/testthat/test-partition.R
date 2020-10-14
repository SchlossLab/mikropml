
group <- c("A", "B", "A", "B", "C", "C", "A", "A", "D")

test_that("create_grouped_data_partition works", {
  set.seed(0)
  train_ind <- create_grouped_data_partition(group, 0.8)
  expect_equal(train_ind, c(1L, 3L, 5L, 6L, 7L, 8L, 9L))
  expect_false(any(group[train_ind] %in% group[-train_ind]))
  expect_false(any(group[-train_ind] %in% group[train_ind]))
  expect_true(length(train_ind) / length(group) <= 0.8)
})
