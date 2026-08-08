# Process categorical features

Process categorical features

## Usage

``` r
process_cat_feats(features, progbar = NULL)
```

## Arguments

- features:

  dataframe of features for machine learning

- progbar:

  optional progress bar (default: `NULL`)

## Value

list of two dataframes: categorical (processed) and continuous features
(unprocessed)

## Author

Zena Lapp, <zenalapp@umich.edu>

## Examples

``` r
if (FALSE) { # \dontrun{
process_cat_feats(mikropml::otu_small[, 2:ncol(otu_small)])
} # }
```
