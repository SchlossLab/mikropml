test_df <- data.frame(
  outcome = c("normal", "normal", "cancer"),
  var1 = 1:3,
  var2 = c('a','b','c'),
  var3 = c('0','1','0'),
  var4 = c(0,0,0)
)

test_that('preprocess_data works',{
  expect_equal(preprocess_data(test_df, 'outcome'), 
               data.frame(
                 outcome = c("normal", "normal", "cancer"),
                 var1 = -1,0,1,
                 var2a = c(1,0,0),
                 var3 = c('0','1','0'),
                 var4 = c(0,0,0)
               ))
})