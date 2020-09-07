options(warnPartialMatchArgs = FALSE)
# Without this, underlying code in either stats or base R causes this warning in several places:
#   warning: get_predictions works
#   partial argument match of 'contrasts' to 'contrasts.arg'

test_df <- data.frame(
  outcome = c("normal", "normal", "cancer"),
  var1 = 1:3,
  var2 = 4:6
)

test_that("split_outcome_features works", {
  expect_equal(
    split_outcome_features(test_df, "outcome"),
    list(
      outcome = data.frame(outcome = c("normal", "normal", "cancer")),
      features = data.frame(
        var1 = 1:3,
        var2 = 4:6
      )
    )
  )
})

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

test_that("check if correct apply is selected", {
  fa_installed <- check_package_installed("future.apply")
  if (fa_installed) {
    expect_equal(select_apply("lapply"), future.apply::future_lapply)
    expect_equal(select_apply("sapply"), future.apply::future_sapply)
    expect_equal(select_apply("apply"), future.apply::future_apply)
  } else {
    expect_equal(select_apply("lapply"), lapply)
    expect_equal(select_apply("sapply"), sapply)
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

test_that("setup_parallel warns", {
  expect_warning(
    setup_parallel("not_a_number"),
    "`ncores` must be `NA` or a number, but you provided"
  )
  if (check_package_installed("doParallel")) {
    expect_warning(
      pc <- setup_parallel(9999999),
      "You specified 9999999 cores, but only"
    )
  } else {
    expect_warning(
      pc <- setup_parallel(9999999),
      "The packages `parallel`, `doParallel`, and `foreach` are required for using multiple cores.
 You specified 9999999 cores, but one or more of these packages are not installed.
 Proceeding with only one process"
    )
  }
  stop_parallel(pc)
})
test_that("setup_parallel works", {
  expect_true(is.null(setup_parallel(NA)))
  if (check_package_installed("doParallel")) {
    expect_message(pc <- setup_parallel(2), "Using 2 cores for parallel processing.")
  } else {
    expect_warning(
      pc <- setup_parallel(2),
      "The packages `parallel`, `doParallel`, and `foreach` are required for using multiple cores.
 You specified 2 cores, but one or more of these packages are not installed.
 Proceeding with only one process"
    )
  }
  stop_parallel(pc)
})

test_that("get_performance_tbl works", {
  expect_equal(
    get_performance_tbl(trained_model_mini,
      test_data_mini,
      "dx",
      "cancer",
      seed = 2019
    ),
    otu_mini_results1$performance
  )
})
