# tests for functions in get_features_importance

# group_correlated_features
test_that('correlated groups correct',{
  corr = dplyr::tibble(feature1=c('A','C','D'),feature2=c('B','A','E'))
  test_data = dplyr::tibble(inf=NA,A=NA,B=NA,C=NA,D=NA,E=NA,F=NA)
  expect_equal(sort(group_correlated_features(corr,test_data)),c('B|A|C','E|D','F'))
})
test_that('no correlated groups correct',{
  corr = dplyr::tibble(feature1=c(character()),feature2=character())
  test_data = dplyr::tibble(inf=NA,A=NA,B=NA,C=NA,D=NA,E=NA,F=NA)
  expect_equal(sort(group_correlated_features(corr,test_data)),c('A','B','C','D','E','F'))
})
test_that('empty dataframe correct',{
  corr = dplyr::tibble(feature1=c(character()),feature2=character())
  test_data = dplyr::tibble()
  expect_equal(sort(group_correlated_features(corr,test_data)),c('NA','NA'))
})

# find_permuted_auc
test_that('permuted auc returns correct value for non-correlated feature',{
  fn_output = find_permuted_auc(mikRopML::trained_model_sm, mikRopML::test_data_sm, 'dx', 'Otu00049', 'cancer')
  expect_equal(fn_output,c(auc=0.9,auc_diff=0))
})

test_that('permuted auc returns correct value for [fake] correlated feature',{
  fn_output = find_permuted_auc(mikRopML::trained_model_sm, mikRopML::test_data_sm, 'dx', 'Otu00049|Otu00050', 'cancer')
  expect_equal(fn_output,c(auc=0.9,auc_diff=0))
})

# get_feature_importance
test_that('feature importances are correct',{
  f_imps = get_feature_importance(mikRopML::train_data_sm, mikRopML::trained_model_sm, mikRopML::test_data_sm, 'dx', 'cancer')
  load('tests/testthat/feat_imps.rda')
  expect_equal(f_imps,feat_imps)
})
  


