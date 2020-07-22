tol <- 1e-5
test_that("flatten_corr_mat works", {
    corr_mat <- data.frame(
        feature1 = as.factor(c(1, 1, 2)),
        feature2 = as.factor(c(2, 3, 3)),
        corr = c(-0.8990325, -0.7950170, 0.2171444)
    )
    set.seed(2019)
    expect_equal(flatten_corr_mat(data.frame(
        a = runif(4, -1, 1),
        b = runif(4, -1, 1),
        c = runif(4, -1, 1)
    )),
    corr_mat, tolerance = tol)
})
test_that("get_corr_feats works", {
    set.seed(0)
    feats <- data.frame(a = runif(4), b = runif(4), c = runif(4))
    cor_feats <- feats %>%
        stats::cor(method = "spearman") %>%
        flatten_corr_mat() %>% dplyr::filter(corr >= 0.6)
    set.seed(0)
    expect_equal(get_corr_feats(feats, 0.6), cor_feats, tolerance = tol)
})
