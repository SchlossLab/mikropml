# Tidy the performance dataframe

Used by
[`plot_model_performance()`](http://www.schlosslab.org/mikropml/reference/plot_model_performance.md).

## Usage

``` r
tidy_perf_data(performance_df)
```

## Arguments

- performance_df:

  dataframe of performance results from multiple calls to
  [`run_ml()`](http://www.schlosslab.org/mikropml/reference/run_ml.md)

## Value

Tidy dataframe with model performance metrics.

## Author

Begüm Topçuoglu, <topcuoglu.begum@gmail.com>

Kelly Sovacool, <sovacool@umich.edu>

## Examples

``` r
if (FALSE) { # \dontrun{
# call `run_ml()` multiple times with different seeds
results_lst <- lapply(seq(100, 104), function(seed) {
  run_ml(otu_small, "glmnet", seed = seed)
})
# extract and combine the performance results
perf_df <- lapply(results_lst, function(result) {
  result[["performance"]]
}) %>%
  dplyr::bind_rows()
# make it pretty!
tidy_perf_data(perf_df)
} # }
```
