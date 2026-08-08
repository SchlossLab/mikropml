# Preprocess continuous features

Preprocess continuous features

## Usage

``` r
process_cont_feats(features, method)
```

## Arguments

- features:

  Dataframe of features for machine learning

- method:

  Methods to preprocess the data, described in
  [`caret::preProcess()`](https://rdrr.io/pkg/caret/man/preProcess.html)
  (default: `c("center","scale")`, use `NULL` for no normalization).

## Value

dataframe of preprocessed features

## Author

Zena Lapp, <zenalapp@umich.edu>

## Examples

``` r
if (FALSE) { # \dontrun{
process_cont_feats(mikropml::otu_small[, 2:ncol(otu_small)], c("center", "scale"))
} # }
```
