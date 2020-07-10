prediction <- c(0.003330436, 0.935320188, 0.003146670, 0.050361065,
                0.037302455, 0.225469013, 0.002964700, 0.002525344,
                0.006217845, 0.132653677, 0.187598964, 0.002360271)
outcomes <- c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
tolerance <- 1e-5

test_that("get_prediction works", {
    expect_equal(get_prediction(trained_model1, test_data1, 'cancer'),
               prediction)
})
test_that("recode_outcome works", {
    expect_equal(recode_outcome(test_data1, 'dx', 'cancer'),
                 outcomes)
    expect_equal(recode_outcome(test_data1, 'dx', 'normal'),
                 c(1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0))
})
test_that("calc_auroc works", {
    expect_equal(calc_auroc(prediction, outcomes), 0.5, tolerance = tolerance)
})
test_that("calc_auprc works", {
    expect_equal(calc_auprc(prediction, outcomes), 0.5649431, tolerance = tolerance)
})
test_that("calc_aucs works", {
    expect_equal(
        calc_aucs(trained_model1, test_data1, 'dx', 'cancer'),
        list(auroc = 0.5, auprc = 0.5649431),
        tolerance = tolerance
    )
})
