# Split hyperparameters dataframe into named lists for each parameter

Using
[`get_hyperparams_list`](http://www.schlosslab.org/mikropml/dev/reference/get_hyperparams_list.md)
is preferred over this function.

## Usage

``` r
get_hyperparams_from_df(hyperparams_df, ml_method)
```

## Arguments

- hyperparams_df:

  dataframe of hyperparameters with columns `param`, `value`, and
  `method`

- ml_method:

  machine learning method

## Value

named list of lists of hyperparameters

## Author

Begüm Topçuoğlu, <topcuoglu.begum@gmail.com>

Kelly Sovacool, <sovacool@umich.edu>

## Examples

``` r
if (FALSE) { # \dontrun{
hparams_df <- dplyr::tibble(
  param = c("alpha", "lambda", "lambda"),
  value = c(1, 0, 1),
  method = rep("glmnet", 3)
)
get_hyperparams_from_df(hparams_df, "glmnet")
} # }
```
