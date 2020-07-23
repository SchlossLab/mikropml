tol <- 1e-2
test_that("flatten_corr_mat works", {
    corr_mat <- data.frame(
        feature1 = as.factor(c('a', 'a', 'b')),
        feature2 = as.factor(c('b', 'c', 'c')),
        corr = c(0.8, 0.4, 0)
    )
    expect_equal(flatten_corr_mat(t(data.frame(
        a = c(1,0.8,0.4),
        b = c(0.8,1,0),
        c = c(0.4,0,1)
    ))),
    corr_mat, tolerance = tol)
})

test_that("get_corr_feats works", {
    cor_feats <- data.frame(
        feature1 = factor(c("a"), levels = c("a", "b")),
        feature2 = factor(c("b"), levels = c("b", "c")),
        corr = c(0.8)
    )
    expect_equal(get_corr_feats(data.frame(
            a = c(0.8966972, 0.2655087, 0.37212390, 0.5728534),
            b = c(0.9082078, 0.2016819, 0.89838968, 0.9446753),
            c = c(0.6607978, 0.6291140, 0.06178627, 0.2059746)
    ), 0.6), cor_feats, tolerance = tol)
})
