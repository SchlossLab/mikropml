test_df <- data.frame(
  outcome = c("normal", "normal", "cancer"),
  var1 = 1:3,
  var2 = c('a','b','c'),
  var3 = c('no','yes','no'),
  var4 = c(0,1,0),
  var5 = c(0,0,0)
)

test_that('preprocess_data works',{
  expect_equal(preprocess_data(test_df, 'outcome'), 
               data.frame(
                 outcome = c("normal", "normal", "cancer"),
                 var1 = c(-1,0,1),
                 var2a = c(1,0,0),
                 var2b = c(0,1,0),
                 var2c = c(0,0,1),
                 var3yes = c(0,1,0),
                 var4 = c(0,1,0)
               ))
  expect_equal(preprocess_data(test_df, 'outcome',method = c('range','nzv')), 
               data.frame(
                 outcome = c("normal", "normal", "cancer"),
                 var1 = c(0,0.5,1),
                 var2a = c(1,0,0),
                 var2b = c(0,1,0),
                 var2c = c(0,0,1),
                 var3yes = c(0,1,0),
                 var4 = c(0,1,0)
               ))
  expect_error(preprocess_data(test_df, 'outcome',method = c('asdf')))
})
  