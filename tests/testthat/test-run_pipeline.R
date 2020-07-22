# test_that("run_pipeline oracle works", {
#     expect_equal(
#         run_pipeline(
#             otu_small,
#             'L2_Logistic_Regression',
#             outcome_colname = 'dx',
#             outcome_value = 'cancer',
#             hyperparameters = mikRopML::default_hyperparams,
#             permute = FALSE,
#             seed = 2019
#         ),
#         otu_sm_results,
#         tolerance = 0.1
#     )
# })
test_that("run_pipeline errors for unsupported method", {
  expect_error(
    run_pipeline(
      otu_small,
      "not_a_method"
    )
  )
})
test_that("run_pipeline errors if outcome_colname not in dataframe", {
  expect_error(
    run_pipeline(
      otu_small,
      "rf",
      outcome_colname = "not_a_colname"
    )
  )
})
test_that("run_pipeline errors if outcome_value not in outcome column", {
  expect_error(
    run_pipeline(
      otu_small,
      "rf",
      outcome_colname = "dx",
      outcome_value = "not_an_outcome"
    )
  )
})
test_that("run_pipeline errors if outcome is not binary", {
  expect_error(
    run_pipeline(
      c(
        dx = c("cancer", "adenoma", "normal"),
        otu1 = 1:3
      ),
      "rf",
      outcome_colname = "dx"
    )
  )
})
