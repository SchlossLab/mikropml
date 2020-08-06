test_that("define_cv works for 5-fold cv on otu_sm data", {
  expect_equal(
    define_cv(train_data_sm, "dx", nfolds = 5, seed = 2019),
    otu_sm_cv5
  )
})
