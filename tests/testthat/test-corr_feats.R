options( # for compatibility with R < 4.0
  stringsAsFactors = FALSE
)
tol <- 1e-5
test_that("flatten_corr_mat works", {
  corr_mat <- data.frame(
    feature1 = c("a", "a", "b"),
    feature2 = c("b", "c", "c"),
    corr = c(0.8, 0.4, 0)
  )
  expect_equivalent(expect_warning(flatten_corr_mat(t(data.frame(
    a = c(1, 0.8, 0.4),
    b = c(0.8, 1, 0),
    c = c(0.4, 0, 1)
  ))), "deprecated"),
  corr_mat,
  tolerance = tol
  )
})

test_that("get_corr_feats works", {
  cor_feats <- data.frame(
    feature1 = c("a"),
    feature2 = c("b"),
    corr = c(0.8)
  )
  expect_equivalent(expect_warning(
    get_corr_feats(data.frame(
      a = c(0.8966972, 0.2655087, 0.37212390, 0.5728534),
      b = c(0.9082078, 0.2016819, 0.89838968, 0.9446753),
      c = c(0.6607978, 0.6291140, 0.06178627, 0.2059746)
    ), 0.6),
    "deprecated"
  ),
  cor_feats,
  tolerance = tol
  )
  expect_equivalent(expect_warning(
    get_corr_feats(data.frame(
      a = c(0.3607978, 0.2911404, 0.36178627, 0.6059746),
      b = c(0.2099978, 0.3013719, 0.59891969, 0.6442003),
      c = c(0.9066972, 0.6655073, 0.37212390, 0.0728404)
    ), 0.9, corr_method = "pearson"),
    "deprecated"
  ),
  data.frame(
    feature1 = c("b"),
    feature2 = c("c"),
    corr = c(-0.96)
  ),
  tolerance = tol
  )
  expect_equivalent(expect_warning(get_corr_feats(data.frame(
    a = c(1, 1, 0, 0),
    b = c(1, 1, 0, 0),
    c = c(0, 0, 1, 1)
  )), "deprecated"),
  structure(list(
    feature1 = c("a", "a", "b"), feature2 = c(
      "b",
      "c", "c"
    ),
    corr = c(1, -1, -1)
  ), class = "data.frame", row.names = c(
    NA,
    -3L
  )),
  tolerance = tol
  )
  expect_equivalent(expect_warning(
    get_corr_feats(data.frame(
      a = c(1, 1, 0, 0),
      b = c(1, 1, 0, 0),
      c = c(0, 0, 1, 1)
    ), group_neg_corr = FALSE),
    "deprecated"
  ),
  structure(list(feature1 = "a", feature2 = "b", corr = 1), class = "data.frame", row.names = c(
    NA,
    -1L
  )),
  tolerance = tol
  )
})

test_that("group_correlated_features works", {
  expect_equal(
    group_correlated_features(data.frame(a = 1:3, b = 2:4, c = c(1, 0, 1))),
    c("a|b", "c")
  )
  expect_equal(
    group_correlated_features(data.frame(a = 1:3, b = c(3, 1, 2))),
    c("a", "b")
  )
  expect_equal(
    group_correlated_features(data.frame(
      a = c(1, 0, 0),
      b = c(3, 2, 4),
      c = c(1, 3, 4)
    ),
    corr_thresh = 0.9,
    corr_method = "pearson"
    ),
    c("a|c", "b")
  )

  features <- dplyr::tibble(
    a = 1:3, b = 2:4, c = c(1, 0, 1),
    d = (5:7), e = c(5, 1, 4), f = c(-1, 0, -1)
  )
  expect_equal(
    group_correlated_features(features),
    c("a|b|d", "c|f", "e")
  )

  expect_error(
    group_correlated_features(dplyr::tibble()),
    "The correlation matrix contains nothing. Hint: is the features data frame empty?"
  )
})
test_that("groups are the same even when column order changes", {
  feats1 <- data.frame(
    a = 1:3, b = 2:4, c = c(1, 0, 1),
    d = (5:7), e = c(5, 1, 4), f = c(-1, 0, -1)
  )
  feats2 <- data.frame(
    a = 1:3, d = (5:7), e = c(5, 1, 4),
    b = 2:4, c = c(1, 0, 1), f = c(-1, 0, -1)
  )
  expect_equal(
    group_correlated_features(feats1),
    group_correlated_features(feats2)
  )
})
test_that("correlation clustering helper functions work", {
  features <- data.frame(
    a = 1:3, b = 2:4, c = c(1, 0, 1),
    d = (5:7), e = c(5, 1, 4)
  )
  corr_mat <- matrix(c(
    1, 1, 0, 1, 0,
    1, 1, 0, 1, 0,
    0, 0, 1, 0, 0,
    1, 1, 0, 1, 0,
    0, 0, 0, 0, 1
  ),
  nrow = 5, ncol = 5,
  dimnames = list(
    c("a", "b", "c", "d", "e"),
    c("a", "b", "c", "d", "e")
  )
  )
  expect_equal(
    get_binary_corr_mat(features),
    corr_mat
  )
  null_mat <- matrix(numeric(0), nrow = 0, ncol = 0)
  expect_equal(
    get_binary_corr_mat(data.frame()),
    null_mat
  )

  expect_equal(
    cluster_corr_mat(corr_mat),
    c(a = 1L, b = 1L, c = 2L, d = 1L, e = 3L)
  )
  expect_error(
    cluster_corr_mat(null_mat),
    "The correlation matrix contains nothing. Hint: is the features data frame empty?"
  )

  expect_equal(
    get_groups_from_clusters(cluster_corr_mat(corr_mat)),
    c("a|b|d", "c", "e")
  )
})
