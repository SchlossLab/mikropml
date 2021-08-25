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

test_that("randomize_feature_order works for known seed", {
  reordered_df <- data.frame(
    outcome = c("normal", "normal", "cancer"),
    var2 = 4:6,
    var1 = 1:3
  )
  set.seed(20)
  expect_equal(
    randomize_feature_order(test_df, "outcome"),
    reordered_df
  )
})

test_that("check if correct apply is selected", {
  fa_installed <- all(check_packages_installed("future.apply"))
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

test_that("replace_spaces works", {
  expect_equal(
    replace_spaces(c("outcome 1", "outcome 2", "outcome 1")),
    c("outcome_1", "outcome_2", "outcome_1")
  )
  expect_equal(
    replace_spaces(c("no_spaces_here", "none")),
    c("no_spaces_here", "none")
  )
})

test_that("replace_spaces() doesn't modify non-character vectors", {
  x <- 1:3
  expect_equal(replace_spaces(x), x)
  y <- c(1.1, 2.2, 3.3)
  expect_equal(replace_spaces(y), y)
})

test_that("pbtick() updates the progress bar", {
  f <- function() {
    pb <- progressr::progressor(steps = 5)
    pbtick(pb, message = "progress!")
  }
  expect_condition(expect_invisible(f()))
})

test_that("radix_sort() order is stable regardless of locale", {
  locale <- Sys.getlocale("LC_COLLATE")
  invisible(Sys.setlocale("LC_COLLATE", "en_US.UTF-8"))
  sort_enus <- radix_sort(c(letters, LETTERS))
  invisible(Sys.setlocale("LC_COLLATE", "C"))
  sort_c <- radix_sort(c(letters, LETTERS))

  expect_equal(sort_enus, sort_c)

  invisible(Sys.setlocale("LC_COLLATE", locale))
})
