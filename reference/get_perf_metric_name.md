# Get default performance metric name

Get default performance metric name for cross-validation.

## Usage

``` r
get_perf_metric_name(outcome_type)
```

## Arguments

- outcome_type:

  Type of outcome (one of: `"continuous"`,`"binary"`,`"multiclass"`).

## Value

Performance metric name.

## Author

Zena Lapp, <zenalapp@umich.edu>

## Examples

``` r
get_perf_metric_name("continuous")
#> [1] "RMSE"
get_perf_metric_name("binary")
#> [1] "AUC"
get_perf_metric_name("multiclass")
#> [1] "logLoss"
```
