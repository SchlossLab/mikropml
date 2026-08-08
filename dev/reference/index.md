# Package index

## Main

The foundations for training machine learning models.

- [`mikropml`](http://www.schlosslab.org/mikropml/dev/reference/mikropml-package.md)
  [`mikropml-package`](http://www.schlosslab.org/mikropml/dev/reference/mikropml-package.md)
  : mikropml: User-Friendly R Package for Robust Machine Learning
  Pipelines
- [`preprocess_data()`](http://www.schlosslab.org/mikropml/dev/reference/preprocess_data.md)
  : Preprocess data prior to running machine learning
- [`run_ml()`](http://www.schlosslab.org/mikropml/dev/reference/run_ml.md)
  : Run the machine learning pipeline

## Model evaluation

Evaluate and interpret models.

- [`get_feature_importance()`](http://www.schlosslab.org/mikropml/dev/reference/get_feature_importance.md)
  : Get feature importance using the permutation method
- [`get_performance_tbl()`](http://www.schlosslab.org/mikropml/dev/reference/get_performance_tbl.md)
  : Get model performance metrics as a one-row tibble
- [`calc_model_sensspec()`](http://www.schlosslab.org/mikropml/dev/reference/sensspec.md)
  [`calc_mean_roc()`](http://www.schlosslab.org/mikropml/dev/reference/sensspec.md)
  [`calc_mean_prc()`](http://www.schlosslab.org/mikropml/dev/reference/sensspec.md)
  : Calculate and summarize performance for ROC and PRC plots
- [`calc_mean_perf()`](http://www.schlosslab.org/mikropml/dev/reference/calc_mean_perf.md)
  : Generic function to calculate mean performance curves for multiple
  models
- [`calc_baseline_precision()`](http://www.schlosslab.org/mikropml/dev/reference/calc_baseline_precision.md)
  : Calculate the fraction of positives, i.e. baseline precision for a
  PRC curve
- [`calc_balanced_precision()`](http://www.schlosslab.org/mikropml/dev/reference/calc_balanced_precision.md)
  : Calculate balanced precision given actual and baseline precision
- [`compare_models()`](http://www.schlosslab.org/mikropml/dev/reference/compare_models.md)
  : Perform permutation tests to compare the performance metric across
  all pairs of a group variable.
- [`permute_p_value()`](http://www.schlosslab.org/mikropml/dev/reference/permute_p_value.md)
  : Calculated a permuted p-value comparing two models
- [`bootstrap_performance()`](http://www.schlosslab.org/mikropml/dev/reference/bootstrap_performance.md)
  : Calculate a bootstrap confidence interval for the performance on a
  single train/test split

## Plotting helpers

Visualize results to help you tune hyperparameters and choose model
methods.

- [`plot_mean_roc()`](http://www.schlosslab.org/mikropml/dev/reference/plot_curves.md)
  [`plot_mean_prc()`](http://www.schlosslab.org/mikropml/dev/reference/plot_curves.md)
  : Plot ROC and PRC curves
- [`plot_hp_performance()`](http://www.schlosslab.org/mikropml/dev/reference/plot_hp_performance.md)
  : Plot hyperparameter performance metrics
- [`plot_model_performance()`](http://www.schlosslab.org/mikropml/dev/reference/plot_model_performance.md)
  : Plot performance metrics for multiple ML runs with different
  parameters
- [`tidy_perf_data()`](http://www.schlosslab.org/mikropml/dev/reference/tidy_perf_data.md)
  : Tidy the performance dataframe
- [`get_hp_performance()`](http://www.schlosslab.org/mikropml/dev/reference/get_hp_performance.md)
  : Get hyperparameter performance metrics
- [`combine_hp_performance()`](http://www.schlosslab.org/mikropml/dev/reference/combine_hp_performance.md)
  : Combine hyperparameter performance metrics for multiple train/test
  splits

## Package Data

### datasets

- [`otu_small`](http://www.schlosslab.org/mikropml/dev/reference/otu_small.md)
  : Small OTU abundance dataset
- [`otu_mini_bin`](http://www.schlosslab.org/mikropml/dev/reference/otu_mini_bin.md)
  : Mini OTU abundance dataset
- [`otu_mini_multi`](http://www.schlosslab.org/mikropml/dev/reference/otu_mini_multi.md)
  : Mini OTU abundance dataset with 3 categorical variables
- [`otu_mini_multi_group`](http://www.schlosslab.org/mikropml/dev/reference/otu_mini_multi_group.md)
  : Groups for otu_mini_multi
- [`otu_data_preproc`](http://www.schlosslab.org/mikropml/dev/reference/otu_data_preproc.md)
  : Mini OTU abundance dataset - preprocessed

### ML results

- [`otu_mini_bin_results_glmnet`](http://www.schlosslab.org/mikropml/dev/reference/otu_mini_bin_results_glmnet.md)
  :

  Results from running the pipeline with L2 logistic regression on
  `otu_mini_bin` with feature importance and grouping

- [`otu_mini_bin_results_rf`](http://www.schlosslab.org/mikropml/dev/reference/otu_mini_bin_results_rf.md)
  :

  Results from running the pipeline with random forest on `otu_mini_bin`

- [`otu_mini_bin_results_rpart2`](http://www.schlosslab.org/mikropml/dev/reference/otu_mini_bin_results_rpart2.md)
  :

  Results from running the pipeline with rpart2 on `otu_mini_bin`

- [`otu_mini_bin_results_svmRadial`](http://www.schlosslab.org/mikropml/dev/reference/otu_mini_bin_results_svmRadial.md)
  :

  Results from running the pipeline with svmRadial on `otu_mini_bin`

- [`otu_mini_bin_results_xgbTree`](http://www.schlosslab.org/mikropml/dev/reference/otu_mini_bin_results_xgbTree.md)
  :

  Results from running the pipeline with xbgTree on `otu_mini_bin`

- [`otu_mini_cont_results_glmnet`](http://www.schlosslab.org/mikropml/dev/reference/otu_mini_cont_results_glmnet.md)
  :

  Results from running the pipeline with glmnet on `otu_mini_bin` with
  `Otu00001` as the outcome

- [`otu_mini_cont_results_nocv`](http://www.schlosslab.org/mikropml/dev/reference/otu_mini_cont_results_nocv.md)
  :

  Results from running the pipeline with glmnet on `otu_mini_bin` with
  `Otu00001` as the outcome column, using a custom train control scheme
  that does not perform cross-validation

- [`otu_mini_multi_results_glmnet`](http://www.schlosslab.org/mikropml/dev/reference/otu_mini_multi_results_glmnet.md)
  :

  Results from running the pipeline with glmnet on `otu_mini_multi` for
  multiclass outcomes

### misc

- [`otu_mini_cv`](http://www.schlosslab.org/mikropml/dev/reference/otu_mini_cv.md)
  :

  Cross validation on `train_data_mini` with grouped features.

- [`replace_spaces()`](http://www.schlosslab.org/mikropml/dev/reference/replace_spaces.md)
  : Replace spaces in all elements of a character vector with
  underscores

## Pipeline customization

Customize various steps of the pipeline beyond the arguments provided by
run_ml() and preprocess_data().

- [`remove_singleton_columns()`](http://www.schlosslab.org/mikropml/dev/reference/remove_singleton_columns.md)
  :

  Remove columns appearing in only `threshold` row(s) or fewer.

- [`get_caret_processed_df()`](http://www.schlosslab.org/mikropml/dev/reference/get_caret_processed_df.md)
  : Get preprocessed dataframe for continuous variables

- [`randomize_feature_order()`](http://www.schlosslab.org/mikropml/dev/reference/randomize_feature_order.md)
  : Randomize feature order to eliminate any position-dependent effects

- [`get_partition_indices()`](http://www.schlosslab.org/mikropml/dev/reference/get_partition_indices.md)
  : Select indices to partition the data into training & testing sets.

- [`get_outcome_type()`](http://www.schlosslab.org/mikropml/dev/reference/get_outcome_type.md)
  : Get outcome type.

- [`get_hyperparams_list()`](http://www.schlosslab.org/mikropml/dev/reference/get_hyperparams_list.md)
  : Set hyperparameters based on ML method and dataset characteristics

- [`get_tuning_grid()`](http://www.schlosslab.org/mikropml/dev/reference/get_tuning_grid.md)
  : Generate the tuning grid for tuning hyperparameters

- [`define_cv()`](http://www.schlosslab.org/mikropml/dev/reference/define_cv.md)
  : Define cross-validation scheme and training parameters

- [`get_perf_metric_name()`](http://www.schlosslab.org/mikropml/dev/reference/get_perf_metric_name.md)
  : Get default performance metric name

- [`get_perf_metric_fn()`](http://www.schlosslab.org/mikropml/dev/reference/get_perf_metric_fn.md)
  : Get default performance metric function

- [`train_model()`](http://www.schlosslab.org/mikropml/dev/reference/train_model.md)
  :

  Train model using
  [`caret::train()`](https://rdrr.io/pkg/caret/man/train.html).

- [`calc_perf_metrics()`](http://www.schlosslab.org/mikropml/dev/reference/calc_perf_metrics.md)
  : Get performance metrics for test data

- [`group_correlated_features()`](http://www.schlosslab.org/mikropml/dev/reference/group_correlated_features.md)
  : Group correlated features
