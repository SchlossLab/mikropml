df <- data.frame(outcome = c("normal", "normal", "cancer"),
                 var1 = 1:3,
                 var2 = 4:6)

test_that("get_outcome_value works with method=fewer", {
    expect_equal(get_outcome_value(df, 'outcome', 'fewer'), "cancer")
})
test_that("get_outcome_value works with method=first", {
    expect_equal(get_outcome_value(df, 'outcome', 'first'), 'normal')
})
test_that("get_outcome_value errors with unsupported method", {
    expect_error(get_outcome_value(df, 'outcome', 'not_a_method'))
})
