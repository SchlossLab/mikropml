test_that("define_cv works for 5-fold cv on otu_sm data", {
    set.seed(2019)
    expect_equal(define_cv(train_data_sm, "dx", nfolds = 5),
               otu_sm_cv5)
})
