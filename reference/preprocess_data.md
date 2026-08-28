# Preprocess data prior to running machine learning

Function to preprocess your data for input into
[`run_ml()`](http://www.schlosslab.org/mikropml/reference/run_ml.md).

## Usage

``` r
preprocess_data(dataset, ...)

# S4 method for class 'TreeSummarizedExperiment'
preprocess_data(
  dataset,
  outcome_colname,
  assay.type = "counts",
  col.var = NULL,
  altexp = NULL,
  name = "preprocessed",
  ...
)

# S4 method for class 'ANY'
preprocess_data(
  dataset,
  outcome_colname,
  method = c("center", "scale"),
  remove_var = "nzv",
  collapse_corr_feats = TRUE,
  corr_method = "spearman",
  corr_thresh = 1,
  to_numeric = TRUE,
  group_neg_corr = TRUE,
  prefilter_threshold = 1,
  ...
)
```

## Arguments

- dataset:

  Data frame with an outcome variable and other columns as features.
  Alternatively, the input can be in `TreeSummarizedExperiment` format.

- ...:

  All additional arguments are passed on to
  [`caret::train()`](https://rdrr.io/pkg/caret/man/train.html), such as
  case weights via the `weights` argument or `ntree` for `rf` models.
  See the [`caret::train()`](https://rdrr.io/pkg/caret/man/train.html)
  docs for more details.

- outcome_colname:

  Column name as a string of the outcome variable (default `NULL`; the
  first column will be chosen automatically).

- assay.type:

  The name of assay from `dataset` when the object is in
  `TreeSummarizedExperiment` format. This assay is used as an input.

- col.var:

  The name of sample matdata variables from `colData` slot of `dataset`
  when the object is in `TreeSummarizedExperiment` format. These
  variables are used as predictors.

- altexp:

  The name of alternative experiment (`altExp`) from `dataset` when the
  object is in `TreeSummarizedExperiment` format. This can be used to
  select an experiment for the input.

- name:

  Name of results used when the input is `TreeSummarizedExperiment`.
  This same name is used for `assay` and `altExp`.

- method:

  Methods to preprocess the data, described in
  [`caret::preProcess()`](https://rdrr.io/pkg/caret/man/preProcess.html)
  (default: `c("center","scale")`, use `NULL` for no normalization).

- remove_var:

  Whether to remove variables with near-zero variance (`'nzv'`;
  default), zero variance (`'zv'`), or none (`NULL`).

- collapse_corr_feats:

  Whether to keep only one of correlated features (see `corr_method` and
  `corr_thresh`)

- corr_method:

  Correlation method. Options are the same as those supported by
  [`stats::cor`](https://rdrr.io/r/stats/cor.html): spearman, pearson,
  kendall. (default: spearman)

- corr_thresh:

  group correlations above or equal to `corr_thresh` (range `0` to `1`;
  default: `1`).

- to_numeric:

  Whether to change features to numeric where possible.

- group_neg_corr:

  Whether to group negatively correlated features together (e.g. c(0,1)
  and c(1,0)).

- prefilter_threshold:

  Remove features which only have non-zero & non-NA values in N rows or
  fewer (default: 1). Set this to -1 to keep all columns at this step.
  This step will also be skipped if `to_numeric` is set to `FALSE`.

## Value

Named list including:

- `dat_transformed`: Preprocessed data.

- `grp_feats`: If features were grouped together, a named list of the
  features corresponding to each group.

- `removed_feats`: Any features that were removed during preprocessing
  (e.g. because there was zero variance or near-zero variance for those
  features).

If the input is `TreeSummarizedExperiment`, the output is added as an
additional data to the input object. If the set of features match in
output and input, the results are stored directly to `assay` slot. If
they do not match, the output is stored to `altExp` slot of the object.

If the `progressr` package is installed, a progress bar with time
elapsed and estimated time to completion can be displayed.

## More details

See the [preprocessing
vignette](http://www.schlosslab.org/mikropml/articles/preprocess.md) for
more details.

Note that if any values in `outcome_colname` contain spaces, they will
be converted to underscores for compatibility with `caret`.

## Author

Zena Lapp, <zenalapp@umich.edu>

Kelly Sovacool, <sovacool@umich.edu>

## Examples

``` r
preprocess_data(mikropml::otu_small, "dx")
#> Using 'dx' as the outcome column.
#> $dat_transformed
#> # A tibble: 200 × 61
#>    dx    Otu00001 Otu00002 Otu00003 Otu00004 Otu00005 Otu00006 Otu00007 Otu00008
#>    <chr>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#>  1 norm…   -0.420   -0.219   -0.174  -0.591   -0.0488  -0.167    -0.569 -0.0624 
#>  2 norm…   -0.105    1.75    -0.718   0.0381   1.54    -0.573    -0.643 -0.132  
#>  3 norm…   -0.708    0.696    1.43    0.604   -0.265   -0.0364   -0.612 -0.207  
#>  4 norm…   -0.494   -0.665    2.02   -0.593   -0.676   -0.586    -0.552 -0.470  
#>  5 norm…    1.11    -0.395   -0.754  -0.586   -0.754    2.73      0.191 -0.676  
#>  6 norm…   -0.685    0.614   -0.174  -0.584    0.376    0.804    -0.337 -0.00608
#>  7 canc…   -0.770   -0.496   -0.318   0.159   -0.658    2.20     -0.717  0.0636 
#>  8 norm…   -0.424   -0.478   -0.397  -0.556   -0.391   -0.0620    0.376 -0.0222 
#>  9 norm…   -0.556    1.14     1.62   -0.352   -0.275   -0.465    -0.804  0.294  
#> 10 canc…    1.46    -0.451   -0.694  -0.0567  -0.706    0.689    -0.370  1.59   
#> # ℹ 190 more rows
#> # ℹ 52 more variables: Otu00009 <dbl>, Otu00010 <dbl>, Otu00011 <dbl>,
#> #   Otu00012 <dbl>, Otu00013 <dbl>, Otu00014 <dbl>, Otu00015 <dbl>,
#> #   Otu00016 <dbl>, Otu00017 <dbl>, Otu00018 <dbl>, Otu00019 <dbl>,
#> #   Otu00020 <dbl>, Otu00021 <dbl>, Otu00022 <dbl>, Otu00023 <dbl>,
#> #   Otu00024 <dbl>, Otu00025 <dbl>, Otu00026 <dbl>, Otu00027 <dbl>,
#> #   Otu00028 <dbl>, Otu00029 <dbl>, Otu00030 <dbl>, Otu00031 <dbl>, …
#> 
#> $grp_feats
#> NULL
#> 
#> $removed_feats
#> character(0)
#> 

# the function can show a progress bar if you have the progressr package installed
## optionally, specify the progress bar format
progressr::handlers(progressr::handler_progress(
  format = ":message :bar :percent | elapsed: :elapsed | eta: :eta",
  clear = FALSE,
  show_after = 0
))
## tell progressor to always report progress
if (FALSE) { # \dontrun{
progressr::handlers(global = TRUE)
## run the function and watch the live progress updates
dat_preproc <- preprocess_data(mikropml::otu_small, "dx")

# Create TreeSE object
library(TreeSummarizedExperiment)
df <- mikropml::otu_small
assay <- df[, !colnames(df) %in% c("dx"), drop = FALSE] |> t() |> as.matrix()
tse <- TreeSummarizedExperiment(assays = SimpleList(counts = assay))
colData(tse)[["dx"]] <- df[["dx"]]

# Preprocess
tse <- preprocess_data(
  dataset = tse,
  assay.type = "counts",
  outcome_colname = "dx"
)
# The result is in assay slot
tse
} # }
```
