# Get hyperparameter performance metrics

Get hyperparameter performance metrics

## Usage

``` r
get_hp_performance(trained_model)
```

## Arguments

- trained_model:

  trained model (e.g. from
  [`run_ml()`](http://www.schlosslab.org/mikropml/dev/reference/run_ml.md))

## Value

Named list:

- `dat`: Dataframe of performance metric for each group of
  hyperparameters.

- `params`: Hyperparameters tuned.

- `metric`: Performance metric used.

## Author

Zena Lapp, <zenalapp@umich.edu>

Kelly Sovacool <sovacool@umich.edu>

## Examples

``` r
get_hp_performance(otu_mini_bin_results_glmnet$trained_model)
#> $dat
#>   alpha lambda       AUC
#> 1     0  1e-04 0.6082552
#> 2     0  1e-03 0.6082552
#> 3     0  1e-02 0.6086458
#> 4     0  1e-01 0.6166789
#> 5     0  1e+00 0.6221737
#> 6     0  1e+01 0.6187408
#> 
#> $params
#> [1] "lambda"
#> 
#> $metric
#> [1] "AUC"
#> 
```
