# tests for split_by_group

group <- c('A','B','A','B','C','C','A','A','D')

test_that("createGroupedDataPartition works", {
  set.seed(0)
  train_ind <- createGroupedDataPartition(group,0.8)
  expect_equal(train_ind, c(1L, 3L, 5L, 6L, 7L, 8L, 9L))
  expect_false(any(group[train_ind] %in% group[-train_ind]))
  expect_false(any(group[-train_ind] %in% group[train_ind]))
  expect_true(length(train_ind)/length(group) <= 0.8)
})

test_that("groupKMultiFolds works", {
  set.seed(0)
  folds <- groupKMultiFolds(group, kfold=2, cv_times=2)
  expect_equal(folds, list(Fold1.Rep1 = c(1L, 3L, 5L, 6L, 7L, 8L), Fold2.Rep1 = c(2L, 
                                                                           4L, 9L), Fold1.Rep2 = c(2L, 4L), Fold2.Rep2 = c(1L, 3L, 5L, 6L, 
                                                                                                                           7L, 8L, 9L)))
  fold_grps <- sapply(folds, function(x) group[x])
  expect_false(any(fold_grps$Fold1.Rep1 %in% fold_grps$Fold2.Rep1))
  expect_false(any(fold_grps$Fold1.Rep2 %in% fold_grps$Fold2.Rep2))
  set.seed(5)
  expect_error(groupKMultiFolds(group, kfold=2, cv_times=2),"Could not split the data into train and validate folds. This could mean you do not have enough samples or groups to perform an ML analysis using the grouping functionality. Alternatively, you can try another seed, or decrease kfold or cv_times.")
})

