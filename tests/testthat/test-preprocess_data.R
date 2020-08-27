test_df <- data.frame(
  outcome = c("normal", "normal", "cancer",NA),
  var1 = 1:4,
  var2 = c("a", "b", "c","d"),
  var3 = c("no", "yes", "no","no"),
  var4 = c(0, 1, 0, 0),
  var5 = c(0, 0, 0, 0),
  var6 = c("no", "no", "no", "no"),
  var7 = c(1, 1, 0, 0)
)

test_that("preprocess_data works", {
  expect_equal(
    expect_message(preprocess_data(test_df, "outcome"), 'Removed '),
    list(dat_transformed = structure(list(outcome = c(
      "normal", "normal",
      "cancer"
    ), var1 = c(-1, 0, 1), var2a = c(1, 0, 0), grp1 = c(
      0,
      1, 0
    ), grp2 = c(1, 1, 0)), row.names = c(NA, -3L), class = c(
      "tbl_df",
      "tbl", "data.frame"
    )), grp_feats = list(
      var1 = "var1", var2a = "var2a",
      grp1 = c("var3yes", "var2b", "var4"), grp2 = c("var7", "var2c")
    ))
  )
  expect_equal(
    preprocess_data(test_df[1:3,], "outcome", rm_corr_feats = FALSE),
    list(
      dat_transformed = dplyr::tibble(
        outcome = c("normal", "normal", "cancer"),
        var1 = c(-1, 0, 1),
        var2a = c(1, 0, 0),
        var2b = c(0, 1, 0),
        var2c = c(0, 0, 1),
        var3yes = c(0, 1, 0),
        var4 = c(0, 1, 0),
        var7 = c(1, 1, 0)
      ),
      grp_feats = NULL
    )
  )
  expect_equal(
    preprocess_data(test_df[1:3, c("outcome", "var1")], "outcome"),
    list(
      dat_transformed = dplyr::tibble(
        outcome = c("normal", "normal", "cancer"),
        var1 = c(-1, 0, 1),
      ),
      grp_feats = NULL
    )
  )
  expect_equal(
    preprocess_data(test_df[1:3, c("outcome", "var2")], "outcome"),
    list(
      dat_transformed = dplyr::tibble(
        outcome = c("normal", "normal", "cancer"),
        var2a = c(1, 0, 0),
        var2b = c(0, 1, 0),
        var2c = c(0, 0, 1),
      ),
      grp_feats = NULL
    )
  )
  expect_equal(
    preprocess_data(test_df[1:3, c("outcome", "var3")], "outcome"),
    list(
      dat_transformed = dplyr::tibble(
        outcome = c("normal", "normal", "cancer"),
        var3yes = c(0, 1, 0),
      ),
      grp_feats = NULL
    )
  )
  expect_equal(
    preprocess_data(test_df[1:3, c("outcome", "var4")], "outcome"),
    list(
      dat_transformed = dplyr::tibble(
        outcome = c("normal", "normal", "cancer"),
        var4 = c(0, 1, 0),
      ),
      grp_feats = NULL
    )
  )
  expect_equal(
    preprocess_data(test_df[1:3,], "outcome", method = NULL),
    list(dat_transformed = structure(list(outcome = c(
      "normal", "normal",
      "cancer"
    ), var1 = c(1, 2, 3), var2a = c(1, 0, 0), grp1 = c(
      0,
      1, 0
    ), grp2 = c(1, 1, 0)), row.names = c(NA, -3L), class = c(
      "tbl_df",
      "tbl", "data.frame"
    )), grp_feats = list(
      var1 = "var1", var2a = "var2a",
      grp1 = c("var3yes", "var2b", "var4"), grp2 = c("var7", "var2c")
    ))
  )
  expect_error(preprocess_data(test_df[1:3, c("outcome", "var5")], "outcome"))
  expect_equal(
    preprocess_data(test_df[1:3,], "outcome", method = c("range")),
    list(dat_transformed = structure(list(outcome = c(
      "normal", "normal",
      "cancer"
    ), var1 = c(0, 0.5, 1), var2a = c(1, 0, 0), grp1 = c(
      0,
      1, 0
    ), grp2 = c(1, 1, 0)), row.names = c(NA, -3L), class = c(
      "tbl_df",
      "tbl", "data.frame"
    )), grp_feats = list(
      var1 = "var1", var2a = "var2a",
      grp1 = c("var3yes", "var2b", "var4"), grp2 = c("var7", "var2c")
    ))
  )
  expect_error(
    preprocess_data(test_df[1:3,], "outcome", rm_nzv = FALSE),
    "`rm_nzv` must be true if `rm_corr_feats` is true. If you would like to group features based on correlation, please re-run this function with `rm_nzv` = TRUE"
  )
  expect_equal(
    preprocess_data(test_df[1:3,], "outcome", rm_nzv = FALSE, rm_corr_feats = FALSE),
    list(
      dat_transformed = dplyr::tibble(
        outcome = c("normal", "normal", "cancer"),
        var1 = c(-1, 0, 1),
        var2a = c(1, 0, 0),
        var2b = c(0, 1, 0),
        var2c = c(0, 0, 1),
        var3yes = c(0, 1, 0),
        var4 = c(0, 1, 0),
        var7 = c(1, 1, 0),
        var5 = c(0, 0, 0),
        var6 = c(0, 0, 0)
      ),
      grp_feats = NULL
    )
  )
  expect_error(preprocess_data(test_df[1:3,], "outcome", method = c("asdf")))
})

test_that("rm_missing_outcome works", {
  expect_equal(expect_message(rm_missing_outcome(test_df, 'outcome'),
                              'Removed'),
               structure(list(outcome = c("normal", "normal", "cancer"), var1 = 1:3, 
                              var2 = c("a", "b", "c"), var3 = c("no", "yes", "no"), var4 = c(0, 
                                                                                             1, 0), var5 = c(0, 0, 0), var6 = c("no", "no", "no"), var7 = c(1, 
                                                                                                                                                            1, 0)), class = "data.frame", row.names = c(NA, -3L)))
  expect_equal(rm_missing_outcome(test_df[1:3,],'outcome'),
               structure(list(outcome = c("normal", "normal", "cancer"), var1 = 1:3, 
                              var2 = c("a", "b", "c"), var3 = c("no", "yes", "no"), var4 = c(0, 
                                                                                             1, 0), var5 = c(0, 0, 0), var6 = c("no", "no", "no"), var7 = c(1, 
                                                                                                                                                            1, 0)), class = "data.frame", row.names = c(NA, -3L)))
})

test_that("process_novar_feats works", {
  expect_equal(
    process_novar_feats(test_df[1:3, 2:ncol(test_df)]),
    list(
      novar_feats = dplyr::tibble(
        var5 = c(0, 0, 0),
        var6 = c(0, 0, 0)
      ),
      var_feats = dplyr::tibble(
        var1 = 1:3,
        var2 = c("a", "b", "c"),
        var3 = c("no", "yes", "no"),
        var4 = c(0, 1, 0),
        var7 = c(1, 1, 0)
      )
    )
  )
  expect_equal(
    process_novar_feats(test_df[1:3, 2:5]),
    list(
      novar_feats = NULL,
      var_feats = dplyr::tibble(
        var1 = 1:3,
        var2 = c("a", "b", "c"),
        var3 = c("no", "yes", "no"),
        var4 = c(0, 1, 0)
      )
    )
  )
  expect_error(process_novar_feats(test_df[1:3, 6:7]))
  expect_error(process_novar_feats(NULL))
})

test_that("process_bin_feats works", {
  expect_equal(
    process_bin_feats(test_df[1:3, 2:5]),
    list(
      bin_feats = dplyr::tibble(
        var3yes = c(0, 1, 0),
        var4 = c(0, 1, 0)
      ),
      nonbin_feats = dplyr::tibble(
        var1 = c(1, 2, 3),
        var2 = c("a", "b", "c")
      )
    )
  )
  expect_equal(
    process_bin_feats(test_df[1:3, 4:5]),
    list(
      bin_feats = dplyr::tibble(
        var3yes = c(0, 1, 0),
        var4 = c(0, 1, 0)
      ),
      nonbin_feats = NULL
    )
  )
  expect_error(process_bin_feats(NULL))
})

test_that("process_nonbin_feats works", {
  expect_equal(
    process_nonbin_feats(test_df[1:3, 2:3], c("center", "scale")),
    dplyr::tibble(
      var1 = c(-1, 0, 1),
      var2a = c(1, 0, 0),
      var2b = c(0, 1, 0),
      var2c = c(0, 0, 1)
    )
  )
  expect_error(process_nonbin_feats(data.frame(test_df[1:3, 7]), c("center", "scale")))
  expect_error(expect_warning(process_nonbin_feats(data.frame(test_df[1:3, 6:7]), c("center", "scale"))))
  expect_error(process_nonbin_feats(NULL))
})

test_that("get_caret_processed_df works", {
  expect_equal(
    get_caret_processed_df(test_df[1:3, 2:3], c("center", "scale")),
    data.frame(
      var1 = c(-1, 0, 1),
      var2 = c("a", "b", "c")
    )
  )
  expect_error(get_caret_processed_df(NULL))
})

test_that("get_caret_dummyvars_df works", {
  expect_equal(
    get_caret_dummyvars_df(test_df[1:3, 2:3], FALSE),
    dplyr::tibble(
      var1 = c(1, 2, 3),
      var2a = c(1, 0, 0),
      var2b = c(0, 1, 0),
      var2c = c(0, 0, 1)
    )
  )
  expect_equal(
    get_caret_dummyvars_df(test_df[1:3, 2:3], TRUE),
    dplyr::tibble(
      var1 = c(1, 2, 3),
      var2b = c(0, 1, 0),
      var2c = c(0, 0, 1)
    )
  )
  expect_error(get_caret_dummyvars_df(test_df[1:3, c(4, 7)]))
  expect_error(get_caret_dummyvars_df(NULL))
})

test_that("rm_corr_feats works", {
  expect_equal(rm_corr_feats(test_df[1:3, c(2, 5, 8)]), list(
    features = structure(list(
      var1 = 1:3, var4 = c(0, 1, 0),
      var7 = c(1, 1, 0)
    ), row.names = c(NA, -3L), class = "data.frame"),
    grp_feats = NULL
  ))
  expect_equal(
    rm_corr_feats(cbind(test_df[1:3, c(2, 5, 8)], var8 = c(1, 1, 0))),
    list(
      features = structure(list(
        var1 = 1:3, var4 = c(0, 1, 0),
        grp1 = c(1, 1, 0)
      ), row.names = c(NA, -3L), class = "data.frame"),
      grp_feats = list(var1 = "var1", var4 = "var4", grp1 = c(
        "var8",
        "var7"
      ))
    )
  )
  expect_equal(
    rm_corr_feats(dplyr::as_tibble(test_df[1:3, c(2)])),
    list(features = structure(list(value = 1:3), row.names = c(
      NA,
      -3L
    ), class = c("tbl_df", "tbl", "data.frame")), grp_feats = NULL)
  )
  expect_error(
    rm_corr_feats(test_df[1:3, c(2, 5, 6, 7)]),
    "Some features are charactors or factors. Please remove these before proceeding with `rm_corr_feats`."
  )
  expect_error(
    rm_corr_feats(test_df[1:3, c(2, 5, 6, 8)]),
    "Some features have no variation. Please remove these before proceeding with `rm_corr_feats`."
  )
})
