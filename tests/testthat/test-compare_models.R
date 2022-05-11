
# get_difference
test_that("get_difference works", {
  expect_equal(
    get_difference(
      data.frame(AUC = c(0.5, 0.8), type = c("a", "b")),
      "type", "AUC"
    ),
    0.3
  )
  expect_error(
    get_difference(
      data.frame(AUC = c(0.5, 0.8), type = c("a", "b")),
      "type", "type"
    ),
    "The specified metric is not numeric, please check that you specified the right column."
  )
})

# shuffle_group
test_that("shuffle_group works", {
  set.seed(2022)
  df <- structure(list(
    condition = c("a", "a", "b", "b"),
    AUC = c(0.2, 0.3, 0.8, 0.9)
  ),
  .Names = c("condition", "AUC"),
  row.names = c(NA, -4L),
  class = "data.frame"
  )
  expect_equal(
    shuffle_group(df, "condition"),
    data.frame(condition = c("b", "b", "a", "a"), AUC = c(0.2, 0.3, 0.8, 0.9))
  )
  expect_error(
    shuffle_group(df, "group"),
    "The col_name `group` does not exist in the data frame."
  )
})

# permute_p_value
test_that("permute_p_value works", {
  set.seed(2022)
  df <- structure(list(
    model = c("rf", "rf", "glmnet", "glmnet", "svmRadial", "svmRadial"),
    AUC = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2)
  ),
  .Names = c("model", "AUC"),
  row.names = c(NA, -6L),
  class = "data.frame"
  )
  expect_equal(
    permute_p_value(df, "AUC", "model", "rf", "glmnet", nperm = 10),
    1
  )
  expect_error(
    permute_p_value(df, "auc", "model", "rf", "glmnet", nperm = 10),
    "The metric does not exist in the data."
  )
  expect_error(
    permute_p_value(df, "AUC", "group", "rf", "glmnet", nperm = 10),
    "The group_name does not exist in the data."
  )
  expect_error(
    permute_p_value(df, "AUC", "model", "RF", "glmnet", nperm = 10),
    "group_1 does not exist in the data."
  )
  expect_error(
    permute_p_value(df, "AUC", "model", "rf", "logreg", nperm = 10),
    "group_2 does not exist in the data."
  )
})

# compare_models
test_that("compare_models works", {
  set.seed(2022)
  df <- structure(list(
    model = c("rf", "rf", "glmnet", "glmnet", "svmRadial", "svmRadial"),
    AUC = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2)
  ),
  .Names = c("model", "AUC"),
  row.names = c(NA, -6L),
  class = "data.frame"
  )
  expect_equal(
    compare_models(df, "AUC", "model", 10),
    structure(list(
      group1 = c("glmnet", "rf", "rf"),
      group2 = c("svmRadial", "glmnet", "svmRadial"),
      p_value = c(1, 1, 1)
    ),
    .Names = c("group1", "group2", "p_value"),
    row.names = c(NA, -3L),
    class = "data.frame"
    )
  )
  expect_error(
    compare_models(df, "auc", "model", 100),
    "The metric does not exist in the data."
  )
  expect_error(
    compare_models(df, "AUC", "group", 100),
    "The group_name does not exist in the data."
  )
})
