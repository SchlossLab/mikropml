# Process features with no variation

Process features with no variation

## Usage

``` r
process_novar_feats(features, progbar = NULL)
```

## Arguments

- features:

  dataframe of features for machine learning

- progbar:

  optional progress bar (default: `NULL`)

## Value

list of two dataframes: features with variability (unprocessed) and
without (processed)

## Author

Zena Lapp, <zenalapp@umich.edu>

## Examples

``` r
if (FALSE) { # \dontrun{
process_novar_feats(mikropml::otu_small[, 2:ncol(otu_small)])
} # }
```
