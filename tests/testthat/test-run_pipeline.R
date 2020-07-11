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
