
group <- c("A", "B", "A", "B", "C", "C", "A", "A", "D")

test_that("get_partition_indices() works", {
  set.seed(0)
  outcomes <- c(
    "normal", "cancer", "normal", "normal", "cancer", "cancer",
    "normal", "normal", "normal", "cancer"
  )
  expect_equal(
    get_partition_indices(outcomes,
      training_frac = 0.8,
      groups = group
    ),
    c(1L, 3L, 5L, 6L, 7L, 8L, 9L)
  )
  set.seed(0)
  expect_equal(
    get_partition_indices(outcomes,
      training_frac = 0.5,
      groups = NULL
    ),
    c(1L, 2L, 3L, 5L, 7L)
  )
  expect_error(
    get_partition_indices(outcomes, training_frac = 0),
    "`training_frac` must be a numeric between 0 and 1."
  )
})

test_that("create_grouped_data_partition() works", {
  set.seed(0)
  train_ind <- create_grouped_data_partition(group, 0.8)
  expect_equal(train_ind, c(1L, 3L, 5L, 6L, 7L, 8L, 9L))
  expect_false(any(group[train_ind] %in% group[-train_ind]))
  expect_false(any(group[-train_ind] %in% group[train_ind]))
  expect_true(length(train_ind) / length(group) <= 0.8)
})
