options( # for compatibility with R < 4.0
  stringsAsFactors = FALSE
)
test_df <- data.frame(
  outcome = c("normal", "normal", "cancer", NA),
  var1 = 1:4,
  var2 = c("a", "b", "c", "d"),
  var3 = c("no", "yes", "no", "no"),
  var4 = c(0, 1, 0, 0),
  var5 = c(0, 0, 0, 0),
  var6 = c("no", "no", "no", "no"),
  var7 = c(1, 1, 0, 0),
  var8 = c(5, 6, NA, 7),
  var9 = c(NA, "x", "y", "z"),
  var10 = c(1, 0, NA, NA),
  var11 = c(1, 1, NA, NA),
  var12 = c("1", "2", "3", "4")
)

test_that("preprocess_data works", {
  expect_equal(
    expect_message(
      preprocess_data(test_df, "outcome",
        prefilter_threshold = -1
      ),
      "Removed "
    ),
    list(
      dat_transformed = structure(list(outcome = c(
        "normal", "normal",
        "cancer"
      ), grp1 = c(0, 1, 0), grp2 = c(1, 0, 0), grp3 = c(
        -1,
        0, 1
      ), grp4 = c(0, 0, 1), var8 = c(
        -0.707106781186547, 0.707106781186547,
        0
      )), row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame")), grp_feats = list(grp1 = c(
        "var10_0", "var2_b", "var3_yes",
        "var4_1", "var9_x"
      ), grp2 = c("var10_1", "var2_a"), grp3 = c(
        "var1",
        "var12"
      ), grp4 = c("var2_c", "var7_1", "var9_y"), var8 = "var8"),
      removed_feats = c("var5", "var6", "var11")
    )
  )
  expect_equal(
    expect_message(preprocess_data(test_df, "outcome",
      prefilter_threshold = -1,
      group_neg_corr = FALSE
    )),
    list(dat_transformed = structure(list(outcome = c(
      "normal", "normal",
      "cancer"
    ), grp1 = c(0, 1, 0), grp2 = c(1, 0, 0), grp3 = c(
      -1,
      0, 1
    ), grp4 = c(0, 0, 1), var7_1 = c(1, 1, 0), var8 = c(
      -0.707106781186547,
      0.707106781186547, 0
    )), row.names = c(NA, -3L), class = c(
      "tbl_df",
      "tbl", "data.frame"
    )), grp_feats = list(
      grp1 = c(
        "var10_0", "var2_b",
        "var3_yes", "var4_1", "var9_x"
      ), grp2 = c("var10_1", "var2_a"), grp3 = c("var1", "var12"), grp4 = c("var2_c", "var9_y"), var7_1 = "var7_1",
      var8 = "var8"
    ), removed_feats = c("var5", "var6", "var11"))
  )
  expect_equal(
    preprocess_data(test_df[1:3, c("outcome", "var1")], "outcome"),
    list(
      dat_transformed = dplyr::tibble(
        outcome = c("normal", "normal", "cancer"),
        var1 = c(-1, 0, 1)
      ),
      grp_feats = NULL,
      removed_feats = character(0)
    )
  )
  expect_equal(
    preprocess_data(test_df[1:3, c("outcome", "var2")], "outcome"),
    list(
      dat_transformed = dplyr::tibble(
        outcome = c("normal", "normal", "cancer"),
        var2_a = c(1, 0, 0),
        var2_b = c(0, 1, 0),
        var2_c = c(0, 0, 1),
      ),
      grp_feats = NULL,
      removed_feats = character(0)
    )
  )
  expect_equal(
    preprocess_data(test_df[1:3, c("outcome", "var3")], "outcome"),
    list(
      dat_transformed = dplyr::tibble(
        outcome = c("normal", "normal", "cancer"),
        var3_yes = c(0, 1, 0),
      ),
      grp_feats = NULL,
      removed_feats = character(0)
    )
  )
  expect_equal(
    preprocess_data(test_df[1:3, c("outcome", "var4")], "outcome",
      prefilter_threshold = -1
    ),
    list(
      dat_transformed = dplyr::tibble(
        outcome = c("normal", "normal", "cancer"),
        var4_1 = c(0, 1, 0),
      ),
      grp_feats = NULL,
      removed_feats = character(0)
    )
  )
  expect_equal(
    expect_message(preprocess_data(test_df[1:3, ], "outcome",
      method = NULL,
      prefilter_threshold = -1
    )),
    list(
      dat_transformed = structure(list(outcome = c(
        "normal", "normal",
        "cancer"
      ), grp1 = c(0, 1, 0), grp2 = c(1, 0, 0), grp3 = c(
        1,
        2, 3
      ), grp4 = c(0, 0, 1), var8 = c(5, 6, 5.5)), row.names = c(
        NA,
        -3L
      ), class = c("tbl_df", "tbl", "data.frame")), grp_feats = list(
        grp1 = c("var10_0", "var2_b", "var3_yes", "var4_1", "var9_x"), grp2 = c("var10_1", "var2_a"), grp3 = c("var1", "var12"), grp4 = c("var2_c", "var7_1", "var9_y"), var8 = "var8"
      ),
      removed_feats = c("var5", "var6", "var11")
    )
  )
  expect_error(
    preprocess_data(test_df[1:3, c("outcome", "var5")], "outcome"),
    "All features have zero variance"
  )
  expect_equal(
    expect_message(preprocess_data(test_df[1:3, ],
      "outcome",
      method = c("range"),
      prefilter_threshold = -1
    )),
    list(
      dat_transformed = structure(list(outcome = c(
        "normal", "normal",
        "cancer"
      ), grp1 = c(0, 1, 0), grp2 = c(1, 0, 0), grp3 = c(
        0,
        0.5, 1
      ), grp4 = c(0, 0, 1), var8 = c(0, 1, 0.5)), row.names = c(
        NA,
        -3L
      ), class = c("tbl_df", "tbl", "data.frame")), grp_feats = list(
        grp1 = c("var10_0", "var2_b", "var3_yes", "var4_1", "var9_x"), grp2 = c("var10_1", "var2_a"), grp3 = c("var1", "var12"), grp4 = c("var2_c", "var7_1", "var9_y"), var8 = "var8"
      ),
      removed_feats = c("var5", "var6", "var11")
    )
  )
  expect_equal(
    expect_message(preprocess_data(test_df[1:3, ],
      "outcome",
      remove_var = "zv",
      prefilter_threshold = -1
    )),
    list(
      dat_transformed = structure(list(outcome = c(
        "normal", "normal",
        "cancer"
      ), grp1 = c(0, 1, 0), grp2 = c(1, 0, 0), grp3 = c(
        -1,
        0, 1
      ), grp4 = c(0, 0, 1), var8 = c(
        -0.707106781186547, 0.707106781186547,
        0
      )), row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame")), grp_feats = list(grp1 = c(
        "var10_0", "var2_b", "var3_yes",
        "var4_1", "var9_x"
      ), grp2 = c("var10_1", "var2_a"), grp3 = c(
        "var1",
        "var12"
      ), grp4 = c("var2_c", "var7_1", "var9_y"), var8 = "var8"),
      removed_feats = c("var5", "var6", "var11")
    )
  )
  expect_equal(
    expect_message(
      preprocess_data(test_df[1:3, ], "outcome",
        remove_var = NULL, prefilter_threshold = -1
      ),
      "Removing"
    ),
    list(
      dat_transformed = structure(list(outcome = c(
        "normal", "normal",
        "cancer"
      ), grp1 = c(0, 1, 0), grp2 = c(1, 0, 0), grp3 = c(
        -1,
        0, 1
      ), grp4 = c(0, 0, 1), var8 = c(
        -0.707106781186547, 0.707106781186547,
        0
      )), row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame")), grp_feats = list(grp1 = c(
        "var10_0", "var2_b", "var3_yes",
        "var4_1", "var9_x"
      ), grp2 = c("var10_1", "var2_a"), grp3 = c(
        "var1",
        "var12"
      ), grp4 = c("var2_c", "var7_1", "var9_y"), var8 = "var8"),
      removed_feats = c("var5", "var6", "var11")
    )
  )
  expect_equal(
    expect_message(preprocess_data(test_df[1:3, ],
      "outcome",
      remove_var = NULL,
      collapse_corr_feats = FALSE,
      prefilter_threshold = -1
    )),
    list(
      dat_transformed = structure(list(outcome = c(
        "normal", "normal",
        "cancer"
      ), var1 = c(-1, 0, 1), var8 = c(
        -0.707106781186547, 0.707106781186547,
        0
      ), var12 = c(-1, 0, 1), var3_yes = c(0, 1, 0), var4_1 = c(
        0,
        1, 0
      ), var7_1 = c(1, 1, 0), var2_a = c(1, 0, 0), var2_b = c(
        0,
        1, 0
      ), var2_c = c(0, 0, 1), var9_x = c(0, 1, 0), var9_y = c(
        0,
        0, 1
      ), var10_0 = c(0, 1, 0), var10_1 = c(1, 0, 0), var5 = c(
        0,
        0, 0
      ), var6 = c(0, 0, 0), var11 = c(1, 1, 1)), row.names = c(
        NA,
        -3L
      ), class = c("tbl_df", "tbl", "data.frame")), grp_feats = NULL,
      removed_feats = character(0)
    )
  )
  expect_error(expect_message(preprocess_data(test_df[1:3, ],
    "outcome",
    method = c("asdf")
  )))
  expect_equal(
    expect_message(preprocess_data(test_df,
      "outcome",
      to_numeric = FALSE
    )),
    list(dat_transformed = structure(list(outcome = c(
      "normal", "normal",
      "cancer"
    ), var1 = c(-1, 0, 1), grp1 = c(0, 1, 0), grp2 = c(
      1,
      0, 0
    ), grp3 = c(0, 0, 1), var8 = c(
      -0.707106781186547, 0.707106781186547,
      0
    )), row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame")), grp_feats = list(var1 = "var1", grp1 = c(
      "var10_0", "var12_2",
      "var2_b", "var3_yes", "var4_1", "var9_x"
    ), grp2 = c(
      "var10_1",
      "var12_1", "var2_a"
    ), grp3 = c(
      "var12_3", "var2_c", "var7_1",
      "var9_y"
    ), var8 = "var8"), removed_feats = c(
      "var5", "var6",
      "var11"
    ))
  )
})

test_that("rm_missing_outcome works", {
  expect_equal(
    expect_message(
      rm_missing_outcome(test_df[, 1:8], "outcome"),
      "Removed"
    ),
    structure(list(
      outcome = c("normal", "normal", "cancer"), var1 = 1:3,
      var2 = c("a", "b", "c"), var3 = c("no", "yes", "no"), var4 = c(
        0,
        1, 0
      ), var5 = c(0, 0, 0), var6 = c("no", "no", "no"), var7 = c(
        1,
        1, 0
      )
    ), class = "data.frame", row.names = c(NA, -3L))
  )
  expect_equal(
    rm_missing_outcome(test_df[1:3, 1:8], "outcome"),
    structure(list(
      outcome = c("normal", "normal", "cancer"), var1 = 1:3,
      var2 = c("a", "b", "c"), var3 = c("no", "yes", "no"), var4 = c(
        0,
        1, 0
      ), var5 = c(0, 0, 0), var6 = c("no", "no", "no"), var7 = c(
        1,
        1, 0
      )
    ), class = "data.frame", row.names = c(NA, -3L))
  )
})

test_that("preprocess_data preserves numeric outcomes", {
  test_df_int <- data.frame(outcome = 1:4, var1 = 1:4)
  expect_warning(
    expect_equal(
      preprocess_data(test_df_int, "outcome")$dat_transformed$outcome,
      test_df_int$outcome
    ),
    "Data is being considered numeric, but all outcome values are integers."
  )
  test_df_dbl <- data.frame(outcome = c(1.1, 2.2, 3.3, 4.4), var1 = 1:4)
  expect_equal(
    preprocess_data(test_df_dbl, "outcome")$dat_transformed$outcome,
    test_df_dbl$outcome
  )
})

test_that("change_to_num works", {
  expect_equal(
    change_to_num(dplyr::as_tibble(test_df[, 13])),
    structure(list(value = c(1, 2, 3, 4)), row.names = c(NA, -4L), class = c(
      "tbl_df",
      "tbl", "data.frame"
    ))
  )
  expect_equal(
    change_to_num(dplyr::as_tibble(test_df)),
    structure(list(
      outcome = c("normal", "normal", "cancer", NA),
      var1 = c(1, 2, 3, 4), var2 = c("a", "b", "c", "d"), var3 = c(
        "no",
        "yes", "no", "no"
      ), var4 = c(0, 1, 0, 0), var5 = c(
        0, 0,
        0, 0
      ), var6 = c("no", "no", "no", "no"), var7 = c(
        1, 1, 0,
        0
      ), var8 = c(5, 6, NA, 7), var9 = c(NA, "x", "y", "z"), var10 = c(
        1,
        0, NA, NA
      ), var11 = c(1, 1, NA, NA), var12 = c(1, 2, 3, 4)
    ), row.names = c(NA, -4L), class = c("tbl_df", "tbl", "data.frame"))
  )
})

test_that("process_novar_feats works", {
  expect_equal(
    expect_message(process_novar_feats(test_df[1:3, 2:ncol(test_df)])),
    list(novar_feats = structure(list(var5 = c(0, 0, 0), var6 = c(
      0,
      0, 0
    ), var11 = c(1, 1, 1)), row.names = c(NA, -3L), class = c(
      "tbl_df",
      "tbl", "data.frame"
    )), var_feats = structure(list(
      var1 = 1:3,
      var2 = c("a", "b", "c"), var3 = c("no", "yes", "no"), var4 = c(
        0,
        1, 0
      ), var7 = c(1, 1, 0), var8 = c(5, 6, NA), var9 = c(
        NA,
        "x", "y"
      ), var10 = c(1, 0, NA), var12 = c("1", "2", "3")
    ), row.names = c(
      NA,
      -3L
    ), class = c("tbl_df", "tbl", "data.frame")))
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
  expect_error(process_novar_feats(NA))
})

test_that("process_cat_feats works", {
  expect_equal(
    process_cat_feats(test_df[1:3, c(2:5)]),
    list(cat_feats = structure(list(var3_yes = c(0, 1, 0), var4_1 = c(
      0,
      1, 0
    ), value_a = c(1, 0, 0), value_b = c(0, 1, 0), value_c = c(
      0,
      0, 1
    )), row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame")), cont_feats = structure(list(var1 = 1:3), row.names = c(
      NA,
      -3L
    ), class = c("tbl_df", "tbl", "data.frame")))
  )
  expect_equal(
    expect_message(process_cat_feats(test_df[1:3, c(2:5, 8:11)])),
    list(cat_feats = structure(list(var3_yes = c(0, 1, 0), var4_1 = c(
      0,
      1, 0
    ), var7_1 = c(1, 1, 0), var2_a = c(1, 0, 0), var2_b = c(
      0,
      1, 0
    ), var2_c = c(0, 0, 1), var9_x = c(0, 1, 0), var9_y = c(
      0,
      0, 1
    ), var10_0 = c(0, 1, 0), var10_1 = c(1, 0, 0)), row.names = c(
      NA,
      -3L
    ), class = c("tbl_df", "tbl", "data.frame")), cont_feats = structure(list(
      var1 = 1:3, var8 = c(5, 6, NA)
    ), row.names = c(NA, -3L), class = c(
      "tbl_df",
      "tbl", "data.frame"
    )))
  )

  expect_error(process_cat_feats(NA))
})

test_that("process_cont_feats works", {
  expect_equal(
    process_cont_feats(dplyr::as_tibble(test_df[1:3, 2]), method = c("center", "scale")),
    list(transformed_cont = structure(list(value = c(-1, 0, 1)), row.names = c(
      NA,
      -3L
    ), class = c("tbl_df", "tbl", "data.frame")), removed_cont = character(0))
  )
  expect_equal(
    expect_message(process_cont_feats(test_df[1:3, c(2, 9)], method = c("center", "scale"))),
    list(transformed_cont = structure(list(var1 = c(-1, 0, 1), var8 = c(
      -0.707106781186547,
      0.707106781186547, 0
    )), row.names = c(NA, -3L), class = c(
      "tbl_df",
      "tbl", "data.frame"
    )), removed_cont = character(0))
  )
  expect_equal(process_cont_feats(NULL), list(transformed_cont = NULL, removed_cont = NULL))
  expect_error(process_cont_feats(NA), "Argument `features` must be a `data.frame` or `tibble`")
})

test_that("get_caret_processed_df works", {
  expect_equal(
    get_caret_processed_df(test_df[1:3, 2:3], c("center", "scale")),
    list(processed = structure(list(var1 = c(-1, 0, 1), var2 = c(
      "a",
      "b", "c"
    )), row.names = c(NA, 3L), class = "data.frame"), removed = character(0))
  )
  expect_equal(
    get_caret_processed_df(test_df[1:3, c(2:3, 10), ], c("center", "scale")),
    list(
      processed = structure(list(var1 = c(-1, 0, 1), var2 = c(
        "a",
        "b", "c"
      ), var9 = c(NA, "x", "y")), row.names = c(NA, 3L), class = "data.frame"),
      removed = character(0)
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
  expect_equal(
    get_caret_dummyvars_df(test_df[1:4, c(2:3, 10)], FALSE),
    structure(list(var1 = c(1, 2, 3, 4), var2a = c(1, 0, 0, 0), var2b = c(
      0,
      1, 0, 0
    ), var2c = c(0, 0, 1, 0), var2d = c(0, 0, 0, 1), var9x = c(
      NA,
      1, 0, 0
    ), var9y = c(NA, 0, 1, 0), var9z = c(NA, 0, 0, 1)), row.names = c(
      NA,
      -4L
    ), class = c("tbl_df", "tbl", "data.frame"))
  )
  expect_error(get_caret_dummyvars_df(test_df[1:3, c(4, 7)]))
  expect_error(get_caret_dummyvars_df(NULL))
})

test_that("collapse_correlated_features works", {
  expect_equal(
    collapse_correlated_features(test_df[1:3, c(2, 5, 8)]),
    list(
      features = structure(list(
        var1 = 1:3, var4 = c(0, 1, 0),
        var7 = c(1, 1, 0)
      ), row.names = c(NA, -3L), class = "data.frame"),
      grp_feats = NULL
    )
  )
  expect_equal(
    collapse_correlated_features(cbind(test_df[1:3, c(2, 5, 8)], var8 = c(1, 1, 0))),
    list(
      features = structure(list(
        var1 = 1:3, var4 = c(0, 1, 0),
        grp1 = c(1, 1, 0)
      ), row.names = c(NA, -3L), class = "data.frame"),
      grp_feats = list(
        var1 = "var1", var4 = "var4",
        grp1 = c("var7", "var8")
      )
    )
  )
  expect_equal(
    collapse_correlated_features(dplyr::as_tibble(test_df[1:3, c(2)])),
    list(features = structure(list(value = 1:3), row.names = c(
      NA,
      -3L
    ), class = c("tbl_df", "tbl", "data.frame")), grp_feats = NULL)
  )
  expect_equal(
    collapse_correlated_features(dplyr::as_tibble(test_df[1:3, c(2, 5, 9, 11)])),
    list(
      features = structure(list(
        var1 = 1:3, var10 = c(1, 0, NA),
        var4 = c(0, 1, 0), var8 = c(5, 6, NA)
      ),
      row.names = c(NA, -3L),
      class = c("tbl_df", "tbl", "data.frame")
      ),
      grp_feats = NULL
    )
  )
  expect_error(
    collapse_correlated_features(test_df[1:3, c(2, 5, 6, 7)]),
    "Some features are charactors or factors. Please remove these before proceeding with `collapse_correlated_features`."
  )
  expect_error(
    collapse_correlated_features(test_df[1:3, c(2, 5, 6, 8)]),
    "Some features have no variation. Please remove these before proceeding with `collapse_correlated_features`."
  )
})


test_that("remove_singleton_columns works", {
  expect_equal(
    remove_singleton_columns(data.frame(
      a = 1:3,
      b = c(0, 1, 0),
      c = 4:6
    )),
    list(
      dat = data.frame(a = 1:3, c = 4:6),
      removed_feats = c("b")
    )
  )
  expect_equal(
    remove_singleton_columns(data.frame(
      a = 1:3,
      b = c(0, 1, NA),
      c = 4:6
    )),
    list(
      dat = data.frame(a = 1:3, c = 4:6),
      removed_feats = c("b")
    )
  )
  expect_equal(
    remove_singleton_columns(data.frame(
      a = 1:3,
      b = c(0, 1, NA),
      c = 4:6
    ),
    threshold = 0
    ),
    list(
      dat = data.frame(
        a = 1:3,
        b = c(0, 1, NA),
        c = 4:6
      ),
      removed_feats = character(0)
    )
  )
  expect_equal(
    remove_singleton_columns(data.frame(
      a = 1:3,
      b = c(1, 1, 1),
      c = 4:6
    )),
    list(
      dat = data.frame(a = 1:3, b = c(1, 1, 1), c = 4:6),
      removed_feats = character(0)
    )
  )
  expect_equal(
    remove_singleton_columns(test_df),
    list(
      dat = data.frame(
        outcome = c("normal", "normal", "cancer", NA),
        var1 = 1:4,
        var2 = c("a", "b", "c", "d"),
        var3 = c("no", "yes", "no", "no"),
        var6 = c("no", "no", "no", "no"),
        var7 = c(1, 1, 0, 0),
        var8 = c(5, 6, NA, 7),
        var9 = c(NA, "x", "y", "z"),
        var11 = c(1, 1, NA, NA),
        var12 = c("1", "2", "3", "4")
      ),
      removed_feats = c("var4", "var5", "var10")
    )
  )
})

test_that("preprocess_data replaces spaces in outcome column values (class labels)", {
  dat <- data.frame(
    dx = c("outcome 1", "outcome 2", "outcome 1"),
    a = 1:3, b = c(5, 7, 1)
  )
  dat_proc <- data.frame(
    dx = c("outcome_1", "outcome_2", "outcome_1"),
    a = c(-1, 0, 1),
    b = c(0.218217890235992, 0.87287156094397, -1.09108945117996)
  )
  expect_equal(
    preprocess_data(dat, "dx")$dat_transformed %>% as.data.frame(),
    dat_proc
  )
})
