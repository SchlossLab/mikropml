# Get dummyvars dataframe (i.e. design matrix)

Get dummyvars dataframe (i.e. design matrix)

## Usage

``` r
get_caret_dummyvars_df(features, full_rank = FALSE, progbar = NULL)
```

## Arguments

- features:

  dataframe of features for machine learning

- full_rank:

  whether matrix should be full rank or not (see
  \`[caret::dummyVars](https://rdrr.io/pkg/caret/man/dummyVars.html))

- progbar:

  optional progress bar (default: `NULL`)

## Value

design matrix

## Author

Zena Lapp, <zenalapp@umich.edu>

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(
  outcome = c("normal", "normal", "cancer"),
  var1 = 1:3,
  var2 = c("a", "b", "c"),
  var3 = c("no", "yes", "no"),
  var4 = c(0, 1, 0)
)
get_caret_dummyvars_df(df, TRUE)
} # }
```
