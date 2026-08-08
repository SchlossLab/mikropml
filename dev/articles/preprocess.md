# Preprocessing data

Before training a model, it’s often necessary and prudent to preprocess
your input data. We provide a function
([`preprocess_data()`](http://www.schlosslab.org/mikropml/dev/reference/preprocess_data.md))
to preprocess input data. The defaults we chose are based on best
practices used in
[FIDDLE](https://gitlab.eecs.umich.edu/mld3/FIDDLE/-/tree/master/) (Tang
et al. 2020). Feel free to check out FIDDLE for more information about
data preprocessing!

[`preprocess_data()`](http://www.schlosslab.org/mikropml/dev/reference/preprocess_data.md)
takes an input dataset where the rows are the samples and the columns
are the outcome variable and features. We preprocess the data as
follows:

- Remove missing outcome values.
- Convert any spaces in outcome names to underscores (`_`).
- Leave binary features as-is (except that categorical variables are
  converted to 0 and 1, and binary variables with missing features are
  split into two rows - see below for more details).
- Normalize continuous features using
  [`caret::preProcess()`](https://rdrr.io/pkg/caret/man/preProcess.html)
  based on the method provided.
- Convert categorical features with more than 2 categories to 0 and 1 in
  multiple columns (one for each category, so each category has it’s own
  column).
- Replace missing categorical data with 0.
- Impute missing continuous values with the median of the feature.
- By default, remove all features with near-zero variance (option to
  also remove only features with zero variance).
- By default, collapse correlated features.

## It’s running so slow!

Since I assume a lot of you won’t read this entire vignette, I’m going
to say this at the beginning. If the
[`preprocess_data()`](http://www.schlosslab.org/mikropml/dev/reference/preprocess_data.md)
function is running super slow, you should consider parallelizing it so
it goes faster!
[`preprocess_data()`](http://www.schlosslab.org/mikropml/dev/reference/preprocess_data.md)
also can report live progress updates. See
[`vignette("parallel")`](http://www.schlosslab.org/mikropml/dev/articles/parallel.md)
for details.

## Examples

We’re going to start off simple and get more complicated, but if you
want the whole shebang at once, just scroll to the bottom.

First, we have to load `mikropml`:

``` r

library(mikropml)
```

### Binary data

Let’s start with only binary variables:

``` r

# raw binary dataset
bin_df <- data.frame(
  outcome = c("normal", "normal", "cancer"),
  var1 = c("no", "yes", "no"),
  var2 = c(0, 1, 1),
  var3 = factor(c("a", "a", "b"))
)
bin_df
#>   outcome var1 var2 var3
#> 1  normal   no    0    a
#> 2  normal  yes    1    a
#> 3  cancer   no    1    b
```

In addition to the dataframe itself, you have to provide the name of the
outcome column to
[`preprocess_data()`](http://www.schlosslab.org/mikropml/dev/reference/preprocess_data.md).
Here’s what the preprocessed data looks like:

``` r

# preprocess raw binary data
preprocess_data(dataset = bin_df, outcome_colname = "outcome")
#> Using 'outcome' as the outcome column.
#> $dat_transformed
#> # A tibble: 3 × 4
#>   outcome var1_yes var2_1 var3_b
#>   <chr>      <dbl>  <dbl>  <dbl>
#> 1 normal         0      0      0
#> 2 normal         1      1      0
#> 3 cancer         0      1      1
#> 
#> $grp_feats
#> NULL
#> 
#> $removed_feats
#> character(0)
```

The output is a list: `dat_transformed` which has the transformed data,
`grp_feats` which is a list of grouped features, and `removed_feats`
which is a list of features that were removed. Here, `grp_feats` is
`NULL` because there are no perfectly correlated features
(e.g. `c(0,1,0)` and `c(0,1,0)`, or `c(0,1,0)` and `c(1,0,1)` - see
below for more details).

The first column (`var1`) in `dat_transformed` is a character and is
changed to `var1_yes` that has zeros (no) and ones (yes). The values in
the second column (`var2`) stay the same because it’s already binary,
but the name changes to `var2_1`. The third column (`var3`) is a factor
and is also changed to binary where b is 1 and a is 0, as denoted by the
new column name `var3_b`.

### Categorical data

On to non-binary categorical data:

``` r

# raw categorical dataset
cat_df <- data.frame(
  outcome = c("normal", "normal", "cancer"),
  var1 = c("a", "b", "c")
)
cat_df
#>   outcome var1
#> 1  normal    a
#> 2  normal    b
#> 3  cancer    c
```

``` r

# preprocess raw categorical data
preprocess_data(dataset = cat_df, outcome_colname = "outcome")
#> Using 'outcome' as the outcome column.
#> $dat_transformed
#> # A tibble: 3 × 4
#>   outcome var1_a var1_b var1_c
#>   <chr>    <dbl>  <dbl>  <dbl>
#> 1 normal       1      0      0
#> 2 normal       0      1      0
#> 3 cancer       0      0      1
#> 
#> $grp_feats
#> NULL
#> 
#> $removed_feats
#> character(0)
```

As you can see, this variable was split into 3 different columns - one
for each type (a, b, and c). And again, `grp_feats` is `NULL`.

### Continuous data

Now, looking at continuous variables:

``` r

# raw continuous dataset
cont_df <- data.frame(
  outcome = c("normal", "normal", "cancer"),
  var1 = c(1, 2, 3)
)
cont_df
#>   outcome var1
#> 1  normal    1
#> 2  normal    2
#> 3  cancer    3
```

``` r

# preprocess raw continuous data
preprocess_data(dataset = cont_df, outcome_colname = "outcome")
#> Using 'outcome' as the outcome column.
#> $dat_transformed
#> # A tibble: 3 × 2
#>   outcome  var1
#>   <chr>   <dbl>
#> 1 normal     -1
#> 2 normal      0
#> 3 cancer      1
#> 
#> $grp_feats
#> NULL
#> 
#> $removed_feats
#> character(0)
```

Wow! Why did the numbers change? This is because the default is to
normalize the data using `"center"` and `"scale"`. While this is often
best practice, you may not want to normalize the data, or you may want
to normalize the data in a different way. If you don’t want to normalize
the data, you can use `method=NULL`:

``` r

# preprocess raw continuous data, no normalization
preprocess_data(dataset = cont_df, outcome_colname = "outcome", method = NULL)
```

You can also normalize the data in different ways. You can choose any
method supported by the `method` argument of
[`caret::preProcess()`](https://rdrr.io/pkg/caret/man/preProcess.html)
(see the
[`caret::preProcess()`](https://rdrr.io/pkg/caret/man/preProcess.html)
docs for details). Note that these methods are only applied to
continuous variables.

Another feature of
[`preprocess_data()`](http://www.schlosslab.org/mikropml/dev/reference/preprocess_data.md)
is that if you provide continuous variables as characters, they will be
converted to numeric:

``` r

# raw continuous dataset as characters
cont_char_df <- data.frame(
  outcome = c("normal", "normal", "cancer"),
  var1 = c("1", "2", "3")
)
cont_char_df
#>   outcome var1
#> 1  normal    1
#> 2  normal    2
#> 3  cancer    3
```

``` r

# preprocess raw continuous character data as numeric
preprocess_data(dataset = cont_char_df, outcome_colname = "outcome")
```

If you don’t want this to happen, and you want character data to remain
character data even if it can be converted to numeric, you can use
`to_numeric=FALSE` and they will be kept as categorical:

``` r

# preprocess raw continuous character data as characters
preprocess_data(dataset = cont_char_df, outcome_colname = "outcome", to_numeric = FALSE)
#> Using 'outcome' as the outcome column.
#> $dat_transformed
#> # A tibble: 3 × 4
#>   outcome var1_1 var1_2 var1_3
#>   <chr>    <dbl>  <dbl>  <dbl>
#> 1 normal       1      0      0
#> 2 normal       0      1      0
#> 3 cancer       0      0      1
#> 
#> $grp_feats
#> NULL
#> 
#> $removed_feats
#> character(0)
```

As you can see from this output, in this case the features are treated
as groups rather than numbers (e.g. they are not normalized).

### Collapse perfectly correlated features

By default,
[`preprocess_data()`](http://www.schlosslab.org/mikropml/dev/reference/preprocess_data.md)
collapses features that are perfectly positively or negatively
correlated. This is because having multiple copies of those features
does not add information to machine learning, and it makes `run_ml`
faster.

``` r

# raw correlated dataset
corr_df <- data.frame(
  outcome = c("normal", "normal", "cancer"),
  var1 = c("no", "yes", "no"),
  var2 = c(0, 1, 0),
  var3 = c(1, 0, 1)
)
corr_df
#>   outcome var1 var2 var3
#> 1  normal   no    0    1
#> 2  normal  yes    1    0
#> 3  cancer   no    0    1
```

``` r

# preprocess raw correlated dataset
preprocess_data(dataset = corr_df, outcome_colname = "outcome")
#> Using 'outcome' as the outcome column.
#> $dat_transformed
#> # A tibble: 3 × 2
#>   outcome  grp1
#>   <chr>   <dbl>
#> 1 normal      0
#> 2 normal      1
#> 3 cancer      0
#> 
#> $grp_feats
#> $grp_feats$grp1
#> [1] "var1_yes" "var3_1"  
#> 
#> 
#> $removed_feats
#> [1] "var2"
```

As you can see, we end up with only one variable, as all 3 are grouped
together. Also, the second element in the list is no longer `NULL`.
Instead, it tells you that `grp1` contains `var1`, `var2`, and `var3`.

You can control the correlation method used by
[`stats::cor()`](https://rdrr.io/r/stats/cor.html)with `corr_method` and
the correlation threshold with `corr_thresh`.

``` r

preprocess_data(
  dataset = corr_df,
  outcome_colname = "outcome",
  corr_method = "pearson",
  corr_thresh = 0.95
)
#> Using 'outcome' as the outcome column.
#> $dat_transformed
#> # A tibble: 3 × 2
#>   outcome  grp1
#>   <chr>   <dbl>
#> 1 normal      0
#> 2 normal      1
#> 3 cancer      0
#> 
#> $grp_feats
#> $grp_feats$grp1
#> [1] "var1_yes" "var3_1"  
#> 
#> 
#> $removed_feats
#> [1] "var2"
```

If you want to group positively correlated features, but not negatively
correlated features (e.g. for interpretability, or another downstream
application), you can do that by using `group_neg_corr=FALSE`:

``` r

# preprocess raw correlated dataset; don't group negatively correlated features
preprocess_data(
  dataset = corr_df,
  outcome_colname = "outcome",
  group_neg_corr = FALSE
)
#> Using 'outcome' as the outcome column.
#> $dat_transformed
#> # A tibble: 3 × 3
#>   outcome var1_yes var3_1
#>   <chr>      <dbl>  <dbl>
#> 1 normal         0      1
#> 2 normal         1      0
#> 3 cancer         0      1
#> 
#> $grp_feats
#> NULL
#> 
#> $removed_feats
#> [1] "var2"
```

Here, `var3` is kept on it’s own because it’s negatively correlated with
`var1` and `var2`. You can also choose to keep all features separate,
even if they are perfectly correlated, by using
`collapse_corr_feats=FALSE`:

``` r

# preprocess raw correlated dataset; don't group negatively correlated features
preprocess_data(
  dataset = corr_df,
  outcome_colname = "outcome",
  collapse_corr_feats = FALSE
)
#> Using 'outcome' as the outcome column.
#> $dat_transformed
#> # A tibble: 3 × 3
#>   outcome var1_yes var3_1
#>   <chr>      <dbl>  <dbl>
#> 1 normal         0      1
#> 2 normal         1      0
#> 3 cancer         0      1
#> 
#> $grp_feats
#> NULL
#> 
#> $removed_feats
#> [1] "var2"
```

In this case, `grp_feats` will always be `NULL`.

### Data with near-zero variance

What if we have variables that are all zero, or all “no”? Those ones
won’t contribute any information, so we remove them:

``` r

# raw dataset with non-variable features
nonvar_df <- data.frame(
  outcome = c("normal", "normal", "cancer"),
  var1 = c("no", "yes", "no"),
  var2 = c(0, 1, 1),
  var3 = c("no", "no", "no"),
  var4 = c(0, 0, 0),
  var5 = c(12, 12, 12)
)
nonvar_df
#>   outcome var1 var2 var3 var4 var5
#> 1  normal   no    0   no    0   12
#> 2  normal  yes    1   no    0   12
#> 3  cancer   no    1   no    0   12
```

Here, `var3`, `var4`, and `var5` all have no variability, so these
variables are removed during preprocessing:

``` r

# remove features with near-zero variance
preprocess_data(dataset = nonvar_df, outcome_colname = "outcome")
#> Using 'outcome' as the outcome column.
#> $dat_transformed
#> # A tibble: 3 × 3
#>   outcome var1_yes var2_1
#>   <chr>      <dbl>  <dbl>
#> 1 normal         0      0
#> 2 normal         1      1
#> 3 cancer         0      1
#> 
#> $grp_feats
#> NULL
#> 
#> $removed_feats
#> [1] "var4" "var3" "var5"
```

You can read the
[`caret::preProcess()`](https://rdrr.io/pkg/caret/man/preProcess.html)
documentation for more information. By default, we remove features with
“near-zero variance” (`remove_var='nzv'`). This uses the default
arguments from
[`caret::nearZeroVar()`](https://rdrr.io/pkg/caret/man/nearZeroVar.html).
However, particularly with smaller datasets, you might not want to
remove features with near-zero variance. If you want to remove only
features with zero variance, you can use `remove_var='zv'`:

``` r

# remove features with zero variance
preprocess_data(dataset = nonvar_df, outcome_colname = "outcome", remove_var = "zv")
#> Using 'outcome' as the outcome column.
#> $dat_transformed
#> # A tibble: 3 × 3
#>   outcome var1_yes var2_1
#>   <chr>      <dbl>  <dbl>
#> 1 normal         0      0
#> 2 normal         1      1
#> 3 cancer         0      1
#> 
#> $grp_feats
#> NULL
#> 
#> $removed_feats
#> [1] "var4" "var3" "var5"
```

If you want to include all features, you can use the argument
`remove_zv=NULL`. For this to work, you cannot collapse correlated
features (otherwise it errors out because of the underlying `caret`
function we use).

``` r

# don't remove features with near-zero or zero variance
preprocess_data(dataset = nonvar_df, outcome_colname = "outcome", remove_var = NULL, collapse_corr_feats = FALSE)
#> Using 'outcome' as the outcome column.
#> $dat_transformed
#> # A tibble: 3 × 5
#>   outcome var1_yes var2_1  var3  var5
#>   <chr>      <dbl>  <dbl> <dbl> <dbl>
#> 1 normal         0      0     0    12
#> 2 normal         1      1     0    12
#> 3 cancer         0      1     0    12
#> 
#> $grp_feats
#> NULL
#> 
#> $removed_feats
#> [1] "var4"
```

If you want to be more nuanced in how you remove near-zero variance
features (e.g. change the default 10% cutoff for the percentage of
distinct values out of the total number of samples), you can use the
[`caret::preProcess()`](https://rdrr.io/pkg/caret/man/preProcess.html)
function after running `preprocess_data` with `remove_var=NULL` (see the
[`caret::nearZeroVar()`](https://rdrr.io/pkg/caret/man/nearZeroVar.html)
function for more information).

### Missing data

[`preprocess_data()`](http://www.schlosslab.org/mikropml/dev/reference/preprocess_data.md)
also deals with missing data. It:

- Removes missing outcome variables.
- Maintains zero variability in a feature if it already has no
  variability (i.e. the feature is removed if removing features with
  near-zero variance).
- Replaces missing binary and categorical variables with zero (after
  splitting into multiple columns).
- Replaces missing continuous data with the median value of that
  feature.

If you’d like to deal with missing data in a different way, please do
that prior to inputting the data to
[`preprocess_data()`](http://www.schlosslab.org/mikropml/dev/reference/preprocess_data.md).

#### Remove missing outcome variables

``` r

# raw dataset with missing outcome value
miss_oc_df <- data.frame(
  outcome = c("normal", "normal", "cancer", NA),
  var1 = c("no", "yes", "no", "no"),
  var2 = c(0, 1, 1, 1)
)
miss_oc_df
#>   outcome var1 var2
#> 1  normal   no    0
#> 2  normal  yes    1
#> 3  cancer   no    1
#> 4    <NA>   no    1
```

``` r

# preprocess raw dataset with missing outcome value
preprocess_data(dataset = miss_oc_df, outcome_colname = "outcome")
#> Using 'outcome' as the outcome column.
#> Removed 1/4 (25%) of samples because of missing outcome value (NA).
#> $dat_transformed
#> # A tibble: 3 × 3
#>   outcome var1_yes var2_1
#>   <chr>      <dbl>  <dbl>
#> 1 normal         0      0
#> 2 normal         1      1
#> 3 cancer         0      1
#> 
#> $grp_feats
#> NULL
#> 
#> $removed_feats
#> character(0)
```

#### Maintain zero variability in a feature if it already has no variability

``` r

# raw dataset with missing value in non-variable feature
miss_nonvar_df <- data.frame(
  outcome = c("normal", "normal", "cancer"),
  var1 = c("no", "yes", "no"),
  var2 = c(NA, 1, 1)
)
miss_nonvar_df
#>   outcome var1 var2
#> 1  normal   no   NA
#> 2  normal  yes    1
#> 3  cancer   no    1
```

``` r

# preprocess raw dataset with missing value in non-variable feature
preprocess_data(dataset = miss_nonvar_df, outcome_colname = "outcome")
#> Using 'outcome' as the outcome column.
#> There are 1 missing value(s) in features with no variation. Missing values were replaced with the non-varying value.
#> $dat_transformed
#> # A tibble: 3 × 2
#>   outcome var1_yes
#>   <chr>      <dbl>
#> 1 normal         0
#> 2 normal         1
#> 3 cancer         0
#> 
#> $grp_feats
#> NULL
#> 
#> $removed_feats
#> [1] "var2"
```

Here, the non-variable feature with missing data is removed because we
removed features with near-zero variance. If we maintained that feature,
it’d be all ones:

``` r

# preprocess raw dataset with missing value in non-variable feature
preprocess_data(dataset = miss_nonvar_df, outcome_colname = "outcome", remove_var = NULL, collapse_corr_feats = FALSE)
#> Using 'outcome' as the outcome column.
#> There are 1 missing value(s) in features with no variation. Missing values were replaced with the non-varying value.
#> $dat_transformed
#> # A tibble: 3 × 3
#>   outcome var1_yes  var2
#>   <chr>      <dbl> <dbl>
#> 1 normal         0     1
#> 2 normal         1     1
#> 3 cancer         0     1
#> 
#> $grp_feats
#> NULL
#> 
#> $removed_feats
#> character(0)
```

#### Replace missing binary and categorical variables with zero

``` r

# raw dataset with missing value in categorical feature
miss_cat_df <- data.frame(
  outcome = c("normal", "normal", "cancer"),
  var1 = c("no", "yes", NA),
  var2 = c(NA, 1, 0)
)
miss_cat_df
#>   outcome var1 var2
#> 1  normal   no   NA
#> 2  normal  yes    1
#> 3  cancer <NA>    0
```

``` r

# preprocess raw dataset with missing value in non-variable feature
preprocess_data(dataset = miss_cat_df, outcome_colname = "outcome")
#> Using 'outcome' as the outcome column.
#> 2 categorical missing value(s) (NA) were replaced with 0. Note that the matrix is not full rank so missing values may be duplicated in separate columns.
#> $dat_transformed
#> # A tibble: 3 × 3
#>   outcome var1_no var1_yes
#>   <chr>     <dbl>    <dbl>
#> 1 normal        1        0
#> 2 normal        0        1
#> 3 cancer        0        0
#> 
#> $grp_feats
#> NULL
#> 
#> $removed_feats
#> [1] "var2"
```

Here each binary variable is split into two, and the missing value is
considered zero for both of them.

#### Replace missing continuous data with the median value of that feature

``` r

# raw dataset with missing value in continuous feature
miss_cont_df <- data.frame(
  outcome = c("normal", "normal", "cancer", "normal"),
  var1 = c(1, 2, 2, NA),
  var2 = c(1, 2, 3, NA)
)
miss_cont_df
#>   outcome var1 var2
#> 1  normal    1    1
#> 2  normal    2    2
#> 3  cancer    2    3
#> 4  normal   NA   NA
```

Here we’re not normalizing continuous features so it’s easier to see
what’s going on (i.e. the median value is used):

``` r

# preprocess raw dataset with missing value in continuous feature
preprocess_data(dataset = miss_cont_df, outcome_colname = "outcome", method = NULL)
#> Using 'outcome' as the outcome column.
#> 2 missing continuous value(s) were imputed using the median value of the feature.
#> $dat_transformed
#> # A tibble: 4 × 3
#>   outcome  var1  var2
#>   <chr>   <dbl> <dbl>
#> 1 normal      1     1
#> 2 normal      2     2
#> 3 cancer      2     3
#> 4 normal      2     2
#> 
#> $grp_feats
#> NULL
#> 
#> $removed_feats
#> character(0)
```

### Putting it all together

Here’s some more complicated example raw data that puts everything we
discussed together:

``` r

test_df <- data.frame(
  outcome = c("normal", "normal", "cancer", NA),
  var1 = 1:4,
  var2 = c("a", "b", "c", "d"),
  var3 = c("no", "yes", "no", "no"),
  var4 = c(0, 1, 0, 0),
  var5 = c(0, 0, 0, 0),
  var6 = c("no", "no", "no", "no"),
  var7 = c(1, 1, 0, 0),
  var8 = c(5, 6, NA, 7),
  var9 = c(NA, "x", "y", "z"),
  var10 = c(1, 0, NA, NA),
  var11 = c(1, 1, NA, NA),
  var12 = c("1", "2", "3", "4")
)
test_df
#>   outcome var1 var2 var3 var4 var5 var6 var7 var8 var9 var10 var11 var12
#> 1  normal    1    a   no    0    0   no    1    5 <NA>     1     1     1
#> 2  normal    2    b  yes    1    0   no    1    6    x     0     1     2
#> 3  cancer    3    c   no    0    0   no    0   NA    y    NA    NA     3
#> 4    <NA>    4    d   no    0    0   no    0    7    z    NA    NA     4
```

Let’s throw this into the preprocessing function with the default
values:

``` r

preprocess_data(dataset = test_df, outcome_colname = "outcome")
#> Using 'outcome' as the outcome column.
#> Removed 1/4 (25%) of samples because of missing outcome value (NA).
#> There are 1 missing value(s) in features with no variation. Missing values were replaced with the non-varying value.
#> 2 categorical missing value(s) (NA) were replaced with 0. Note that the matrix is not full rank so missing values may be duplicated in separate columns.
#> 1 missing continuous value(s) were imputed using the median value of the feature.
#> $dat_transformed
#> # A tibble: 3 × 6
#>   outcome  grp1 var2_a  grp2  grp3   var8
#>   <chr>   <dbl>  <dbl> <dbl> <dbl>  <dbl>
#> 1 normal     -1      1     0     0 -0.707
#> 2 normal      0      0     1     0  0.707
#> 3 cancer      1      0     0     1  0    
#> 
#> $grp_feats
#> $grp_feats$grp1
#> [1] "var1"  "var12"
#> 
#> $grp_feats$var2_a
#> [1] "var2_a"
#> 
#> $grp_feats$grp2
#> [1] "var2_b"   "var3_yes" "var9_x"  
#> 
#> $grp_feats$grp3
#> [1] "var2_c" "var7_1" "var9_y"
#> 
#> $grp_feats$var8
#> [1] "var8"
#> 
#> 
#> $removed_feats
#> [1] "var4"  "var5"  "var10" "var6"  "var11"
```

As you can see, we got several messages:

- One of the samples (row 4) was removed because the outcome value was
  missing.
- One of the variables in a feature with no variation had a missing
  value that was replaced with the the non-varying value (`var11`).
- Four categorical missing values were replaced with zero (`var9`).
  There are 4 missing rather than just 1 (like in the raw data) because
  we split the categorical variable into 4 different columns first.
- One missing continuous value was imputed using the median value of
  that feature (`var8`).

Additionally, you can see that the continuous variables were normalized,
the categorical variables were all changed to binary, and several
features were grouped together. The variables in each group can be found
in `grp_feats`.

### Next step: train and evaluate your model!

After you preprocess your data (either using
[`preprocess_data()`](http://www.schlosslab.org/mikropml/dev/reference/preprocess_data.md)
or by preprocessing the data on your own), you’re ready to train and
evaluate machine learning models! Please see
[`run_ml()`](http://www.schlosslab.org/mikropml/dev/reference/run_ml.md)
information about training models.

Tang, Shengpu, Parmida Davarmanesh, Yanmeng Song, Danai Koutra, Michael
W. Sjoding, and Jenna Wiens. 2020. “Democratizing EHR Analyses with
FIDDLE: A Flexible Data-Driven Preprocessing Pipeline for Structured
Clinical Data.” *J Am Med Inform Assoc*, ahead of print, October.
<https://doi.org/10.1093/jamia/ocaa139>.
