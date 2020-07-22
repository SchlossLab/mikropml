test_that("tune grid works for regLogistic", {
    hyperparams <- get_method_hyperparams('regLogistic',
                                          mikRopML::default_hyperparams)
    grid <- expand.grid(cost = hyperparams$cost,
                        loss = "L2_primal",
                        epsilon = 0.01)
    expect_equal(generate_tuning_grid("regLogistic",
                                      mikRopML::default_hyperparams),
                 grid)
})
test_that("tune grid works for svmRadial", {
    hyperparams <- get_method_hyperparams("svmRadial",
                                          mikRopML::default_hyperparams)
    grid <- expand.grid(sigma = hyperparams$sigma,
                        C = hyperparams$C)
    expect_equal(generate_tuning_grid("svmRadial",
                                      mikRopML::default_hyperparams),
                 grid)
})
test_that("tune grid works for rpart2", {
    hyperparams <- get_method_hyperparams("rpart2",
                                          mikRopML::default_hyperparams)
    grid <- expand.grid(maxdepth = hyperparams$maxdepth)
    expect_equal(generate_tuning_grid("rpart2",
                                      mikRopML::default_hyperparams),
                 grid)
})
test_that("tune grid works for rf", {
    hyperparams <- get_method_hyperparams("rf",
                                          mikRopML::default_hyperparams)
    grid <- expand.grid(mtry = hyperparams$mtry)
    expect_equal(generate_tuning_grid("rf",
                                      mikRopML::default_hyperparams),
                 grid)
})
test_that("tune grid works for xgbTree", {
    hyperparams <- get_method_hyperparams("xgbTree",
                                          mikRopML::default_hyperparams)
    grid <- expand.grid(
        nrounds = hyperparams$nrounds,
        gamma = hyperparams$gamma,
        eta = hyperparams$eta,
        max_depth = hyperparams$max_depth,
        colsample_bytree = hyperparams$colsample_bytree,
        min_child_weight = hyperparams$min_child_weight,
        subsample = hyperparams$subsample
    )
    expect_equal(generate_tuning_grid("xgbTree",
                                      mikRopML::default_hyperparams),
                 grid)
})
test_that("tune grid errors when method is not supported", {
    expect_error(generate_tuning_grid("not_a_method",
                                      mikRopML::default_hyperparams))
})
