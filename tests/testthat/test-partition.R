
sample_groups <- c("A", "B", "A", "B", "C", "C", "A", "A", "D")

test_that("get_partition_indices() works", {
  set.seed(0)
  outcomes <- c(
    "normal", "cancer", "normal", "normal", "cancer", "cancer",
    "normal", "normal", "normal", "cancer"
  )
  expect_equal(
    get_partition_indices(outcomes,
      training_frac = 0.8,
      groups = sample_groups
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

check_simple_grouped_partition <- function(groups, train_indices) {
  expect_false(any(groups[train_indices] %in% groups[-train_indices]))
  expect_false(any(groups[-train_indices] %in% groups[train_indices]))
  expect_true(length(train_indices) / length(groups) <= 0.8)
}

test_that("create_grouped_data_partition() works with entire groups in train/test splits", {
  set.seed(0)
  train_ind <- create_grouped_data_partition(sample_groups, training_frac = 0.8)
  expect_equal(train_ind, c(1L, 3L, 5L, 6L, 7L, 8L, 9L))
  check_simple_grouped_partition(sample_groups, train_ind)
})
check_custom_grouped_partition <- function(groups, train_indices, group_partitions) {
  check_simple_grouped_partition(groups, train_indices)
  in_train_only <-
    setdiff(group_partitions$train, group_partitions$test)
  in_test_only <-
    setdiff(group_partitions$test, group_partitions$train)
  in_both <-
    intersect(group_partitions$test, group_partitions$train)
  in_neither <-
    setdiff(unique_groups,
            union(group_partitions$test, group_partitions$train))
  # TODO
}
test_that("create_grouped_data_partition() works with entire groups in train/test splits", {
  set.seed(0)
  sample_groups <- rep.int(c("A", "B", "C"), 3)
  group_part <- list(train = c("A"), test = c("A", "B", "C"))
  train_ind <- create_grouped_data_partition(sample_groups,
                                             group_partitions = group_part,
                                             training_frac = 0.33)
  #expect_equal(train_ind, c(1L, 3L, 5L, 6L, 7L, 8L, 9L))
  check_custom_grouped_partition(sample_groups, train_ind, group_part)
})
