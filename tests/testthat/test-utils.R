df <- data.frame(
  outcome = c("normal", "normal", "cancer"),
  var1 = 1:3,
  var2 = 4:6
)

test_that("get_outcome_value works with method=fewer", {
  expect_equal(get_outcome_value(df, "outcome", "fewer"), "cancer")
})
test_that("get_outcome_value works with method=first", {
  expect_equal(get_outcome_value(df, "outcome", "first"), "normal")
})
test_that("get_outcome_value errors with unsupported method", {
  expect_error(get_outcome_value(df, "outcome", "not_a_method"))
})

# check_package_installed
test_that('check if package is installed',{
  expect_equal(check_package_installed('caret'),TRUE)
})
test_that('check if packages is installed',{
  expect_equal(check_package_installed('asdf'),FALSE)
})

# select_apply
test_that('check if correct lapply is selected',{
  fa_installed = check_package_installed('future.apply')
  if(fa_installed){
    expect_equal(select_apply('lapply'),future.apply::future_lapply)
  }else{
    expect_equal(select_apply('lapply'),lapply)
  }
})
test_that('check if correct sapply is selected',{
  fa_installed = check_package_installed('future.apply')
  if(fa_installed){
    expect_equal(select_apply('sapply'),future.apply::future_sapply)
  }else{
    expect_equal(select_apply('sapply'),sapply)
  }
})
test_that('check if correct lapply is selected',{
  fa_installed = check_package_installed('future.apply')
  if(fa_installed){
    expect_equal(select_apply('apply'),future.apply::future_apply)
  }else{
    expect_equal(select_apply('apply'),apply)
  }
})