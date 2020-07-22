test_that("tune grid works for regLogistic", {
    hyperparams <- get_method_hyperparams('regLogistic',
                                          mikRopML::default_hyperparams)
    grid <- expand.grid(cost = hyperparams$cost,
                        epsilon = hyperparams$epsilon,
                        loss = hyperparams$loss)
    expect_equal(get_tuning_grid("regLogistic", hyperparams), grid)
})
test_that("tune grid works for svmRadial", {
    hyperparams <- get_method_hyperparams("svmRadial",
                                          mikRopML::default_hyperparams)
    grid <- expand.grid(C = hyperparams$C,
                        sigma = hyperparams$sigma)
    expect_equal(get_tuning_grid("svmRadial", hyperparams), grid)
})
test_that("tune grid works for rpart2", {
    hyperparams <- get_method_hyperparams("rpart2",
                                          mikRopML::default_hyperparams)
    grid <- expand.grid(maxdepth = hyperparams$maxdepth)
    expect_equal(get_tuning_grid("rpart2", hyperparams), grid)
})
test_that("tune grid works for rf", {
    hyperparams <- get_method_hyperparams("rf",
                                          mikRopML::default_hyperparams)
    grid <- expand.grid(mtry = hyperparams$mtry)
    expect_equal(get_tuning_grid("rf", hyperparams), grid)
})
test_that("tune grid works for xgbTree", {
    hyperparams <- get_method_hyperparams("xgbTree",
                                          mikRopML::default_hyperparams)
    grid <- expand.grid(
        colsample_bytree = hyperparams$colsample_bytree,
        eta = hyperparams$eta,
        gamma = hyperparams$gamma,
        max_depth = hyperparams$max_depth,
        min_child_weight = hyperparams$min_child_weight,
        nrounds = hyperparams$nrounds,
        subsample = hyperparams$subsample
    )
    expect_equal(get_tuning_grid("xgbTree", hyperparams), grid)
})
