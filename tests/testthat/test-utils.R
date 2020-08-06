test_df <- data.frame(
  outcome = c("normal", "normal", "cancer"),
  var1 = 1:3,
  var2 = 4:6
)

test_that("pick_outcome_value works for all methods", {
  expect_equal(pick_outcome_value(test_df, "outcome", "fewer"), "cancer")
  expect_equal(pick_outcome_value(test_df, "outcome", "first"), "normal")
  expect_error(
    pick_outcome_value(test_df, "outcome", "not_a_method"),
    "Method not_a_method for selecting outcome value not recognized."
  )
})

test_that("randomize_feature_order works for known seed", {
  reordered_df <- data.frame(
    outcome = c("normal", "normal", "cancer"),
    var2 = 4:6,
    var1 = 1:3
  )
  expect_equal(
    randomize_feature_order(test_df, "outcome", seed = 20),
    reordered_df
  )
})

test_that("check if correct lapply is selected", {
  fa_installed <- check_package_installed("future.apply")
  if (fa_installed) {
    expect_equal(select_apply("lapply"), future.apply::future_lapply)
  } else {
    expect_equal(select_apply("lapply"), lapply)
  }
})
test_that("check if correct sapply is selected", {
  fa_installed <- check_package_installed("future.apply")
  if (fa_installed) {
    expect_equal(select_apply("sapply"), future.apply::future_sapply)
  } else {
    expect_equal(select_apply("sapply"), sapply)
  }
})
test_that("check if correct lapply is selected", {
  fa_installed <- check_package_installed("future.apply")
  if (fa_installed) {
    expect_equal(select_apply("apply"), future.apply::future_apply)
  } else {
    expect_equal(select_apply("apply"), apply)
  }
})

test_that("mutate_all_types converts factors to other types", {
  dat1 <- data.frame(
    c1 = as.factor(c("a", "b", "c")),
    c2 = as.factor(1:3),
    c3 = as.factor(c(1.1, 1.2, 1.3))
  )
  dat2 <- mutate_all_types(dat1)
  expect_equal(class(dat2$c1), "character")
  expect_equal(class(dat2$c2), "integer")
  expect_equal(class(dat2$c3), "numeric")
})
