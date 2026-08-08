# Introduction to mikropml

The goal of `mikropml` is to make supervised machine learning (ML) easy
for you to run while implementing good practices for machine learning
pipelines. All you need to run the ML pipeline is one function:
[`run_ml()`](http://www.schlosslab.org/mikropml/dev/reference/run_ml.md).
We’ve selected sensible default arguments related to good practices
(Topçuoğlu et al. 2020; Tang et al. 2020), but we allow you to change
those arguments to tailor
[`run_ml()`](http://www.schlosslab.org/mikropml/dev/reference/run_ml.md)
to the needs of your data.

This document takes you through all of the
[`run_ml()`](http://www.schlosslab.org/mikropml/dev/reference/run_ml.md)
inputs, both required and optional, as well as the outputs.

In summary, you provide:

- A dataset with an outcome column and feature columns (rows are
  samples; unfortunately we do not support multi-label classification)
- Model choice (i.e. method)

And the function outputs:

- The trained model
- Model performance metrics
- (Optional) feature importance metrics

## It’s running so slow!

Since I assume a lot of you won’t read this entire vignette, I’m going
to say this at the beginning. If the
[`run_ml()`](http://www.schlosslab.org/mikropml/dev/reference/run_ml.md)
function is running super slow, you should consider parallelizing. See
[`vignette("parallel")`](http://www.schlosslab.org/mikropml/dev/articles/parallel.md)
for examples.

## Understanding the inputs

### The input data

The input data to
[`run_ml()`](http://www.schlosslab.org/mikropml/dev/reference/run_ml.md)
is a dataframe where each row is a sample or observation. One column
(assumed to be the first) is the outcome of interest, and all of the
other columns are the features. We package `otu_mini_bin` as a small
example dataset with `mikropml`.

``` r

# install.packages("devtools")
# devtools::install_github("SchlossLab/mikropml")
library(mikropml)
head(otu_mini_bin)
#>       dx Otu00001 Otu00002 Otu00003 Otu00004 Otu00005 Otu00006 Otu00007
#> 1 normal      350      268      213        1      208      230       70
#> 2 normal      568     1320       13      293      671      103       48
#> 3 normal      151      756      802      556      145      271       57
#> 4 normal      299       30     1018        0       25       99       75
#> 5 normal     1409      174        0        3        2     1136      296
#> 6 normal      167      712      213        4      332      534      139
#>   Otu00008 Otu00009 Otu00010
#> 1      230      235       64
#> 2      204      119      115
#> 3      176       37      710
#> 4       78      255      197
#> 5        1      537      533
#> 6      251      155      122
```

Here, `dx` is the outcome column (normal or cancer), and there are 10
features (`Otu00001` through `Otu00010`). Because there are only 2
outcomes, we will be performing binary classification in the majority of
the examples below. At the bottom, we will also briefly provide examples
of multi-class and continuous outcomes. As you’ll see, you run them in
the same way as for binary classification!

The feature columns are the amount of each [Operational Taxonomic Unit
(OTU)](https://en.wikipedia.org/wiki/Operational_taxonomic_unit) in
microbiome samples from patients with cancer and without cancer. The
goal is to predict `dx`, which stands for diagnosis. This diagnosis can
be cancer or not based on an individual’s microbiome. No need to
understand exactly what that means, but if you’re interested you can
read more about it from the original paper (Topçuoğlu et al. 2020).

For real machine learning applications you’ll need to use more features,
but for the purposes of this vignette we’ll stick with this example
dataset so everything runs faster.

### The methods we support

All of the methods we use are supported by a great ML wrapper package
[`caret`](https://topepo.github.io/caret/), which we use to train our
machine learning models.

The methods we have tested (and their backend packages) are:

- Logistic/multiclass/linear regression (`"glmnet"`)
- Random forest (`"rf"`)
- Decision tree (`"rpart2"`)
- Support vector machine with a radial basis kernel (`"svmRadial"`)
- xgboost (`"xgbTree"`)

For documentation on these methods, as well as many others, you can look
at the [available
models](https://topepo.github.io/caret/available-models.html) (or see
[here](https://topepo.github.io/caret/train-models-by-tag.html) for a
list by tag). While we have not vetted the other models used by `caret`,
our function is general enough that others might work. While we can’t
promise that we can help with other models, feel free to \[start a new
discussion on
GitHub\]<https://github.com/SchlossLab/mikropml/discussions>) if you
have questions about other models and we *might* be able to help.

We will first focus on `glmnet`, which is our default implementation of
L2-regularized logistic regression. Then we will cover a few other
examples towards the end.

## Before running ML

Before you execute
[`run_ml()`](http://www.schlosslab.org/mikropml/dev/reference/run_ml.md),
you should consider preprocessing your data, either on your own or with
the
[`preprocess_data()`](http://www.schlosslab.org/mikropml/dev/reference/preprocess_data.md)
function. You can learn more about this in the preprocessing vignette:
[`vignette("preprocess")`](http://www.schlosslab.org/mikropml/dev/articles/preprocess.md).

## The simplest way to `run_ml()`

As mentioned above, the minimal input is your dataset (`dataset`) and
the machine learning model you want to use (`method`).

You may also want to provide:

- The outcome column name. By default
  [`run_ml()`](http://www.schlosslab.org/mikropml/dev/reference/run_ml.md)
  will pick the first column, but it’s best practice to specify the
  column name explicitly.
- A seed so that the results will be reproducible, and so that you get
  the same results as those you see here (i.e have the same train/test
  split).

Say we want to use logistic regression, then the method we will use is
`glmnet`. To do so, run the ML pipeline with:

``` r

results <- run_ml(otu_mini_bin,
  "glmnet",
  outcome_colname = "dx",
  seed = 2019
)
```

You’ll notice a few things:

1.  It takes a little while to run. This is because of some of the
    parameters we use.
2.  There is a message stating that ‘dx’ is being used as the outcome
    column. This is what we want, but it’s a nice sanity check!
3.  There was a warning. Don’t worry about this warning right now - it
    just means that some of the hyperparameters aren’t a good fit - but
    if you’re interested in learning more, see
    [`vignette("tuning")`](http://www.schlosslab.org/mikropml/dev/articles/tuning.md).

Now, let’s dig into the output a bit. The results is a list of 4 things:

``` r

names(results)
#> [1] "trained_model"      "test_data"          "performance"       
#> [4] "feature_importance"
```

`trained_model` is the trained model from `caret`. There is a bunch of
info in this that we won’t get into, because you can learn more from the
[`caret::train()`](https://rdrr.io/pkg/caret/man/train.html)
documentation.

``` r

names(results$trained_model)
#>  [1] "method"       "modelInfo"    "modelType"    "results"      "pred"        
#>  [6] "bestTune"     "call"         "dots"         "metric"       "control"     
#> [11] "finalModel"   "preProcess"   "trainingData" "ptype"        "resample"    
#> [16] "resampledCM"  "perfNames"    "maximize"     "yLimits"      "times"       
#> [21] "levels"
```

`test_data` is the partition of the dataset that was used for testing.
In machine learning, it’s always important to have a held-out test
dataset that is not used in the training stage. In this pipeline we do
that using
[`run_ml()`](http://www.schlosslab.org/mikropml/dev/reference/run_ml.md)
where we split your data into training and testing sets. The training
data are used to build the model (e.g. tune hyperparameters, learn the
data) and the test data are used to evaluate how well the model
performs.

``` r

head(results$test_data)
#>        dx Otu00009 Otu00005 Otu00010 Otu00001 Otu00008 Otu00004 Otu00003
#> 9  normal      119      142      248      256      363      112      871
#> 14 normal       60      209       70       86       96        1      123
#> 16 cancer      205        5      180     1668       95       22        3
#> 17 normal      188      356      107      381     1035      915      315
#> 27 normal        4       21      161        7        1       27        8
#> 30 normal       13      166        5       31       33        5       58
#>    Otu00002 Otu00007 Otu00006
#> 9       995        0      137
#> 14      426       54       40
#> 16       20      590      570
#> 17      357      253      341
#> 27       25      322        5
#> 30      179        6       30
```

`performance` is a dataframe of (mainly) performance metrics (1 column
for cross-validation performance metric, several for test performance
metrics, and 2 columns at the end with ML method and seed):

``` r

results$performance
#> # A tibble: 1 × 17
#>   cv_metric_AUC logLoss   AUC prAUC Accuracy Kappa    F1 Sensitivity Specificity
#>           <dbl>   <dbl> <dbl> <dbl>    <dbl> <dbl> <dbl>       <dbl>       <dbl>
#> 1         0.622   0.684 0.647 0.606    0.590 0.179   0.6         0.6       0.579
#> # ℹ 8 more variables: Pos_Pred_Value <dbl>, Neg_Pred_Value <dbl>,
#> #   Precision <dbl>, Recall <dbl>, Detection_Rate <dbl>,
#> #   Balanced_Accuracy <dbl>, method <chr>, seed <dbl>
```

When using logistic regression for binary classification, area under the
receiver-operator characteristic curve (AUC) is a useful metric to
evaluate model performance. Because of that, it’s the default that we
use for `mikropml`. However, it is crucial to evaluate your model
performance using multiple metrics. Below you can find more information
about other performance metrics and how to use them in our package.

`cv_metric_AUC` is the AUC for the cross-validation folds for the
training data. This gives us a sense of how well the model performs on
the training data.

Most of the other columns are performance metrics for the test data —
the data that wasn’t used to build the model. Here, you can see that the
AUC for the test data is not much above 0.5, suggesting that this model
does not predict much better than chance, and that the model is overfit
because the cross-validation AUC (`cv_metric_AUC`, measured during
training) is much higher than the testing AUC. This isn’t too surprising
since we’re using so few features with this example dataset, so don’t be
discouraged. The default option also provides a number of other
performance metrics that you might be interested in, including area
under the precision-recall curve (prAUC).

The last columns of `results$performance` are the method and seed (if
you set one) to help with combining results from multiple runs (see
[`vignette("parallel")`](http://www.schlosslab.org/mikropml/dev/articles/parallel.md)).

`feature_importance` has information about feature importance values if
`find_feature_importance = TRUE` (the default is `FALSE`). Since we used
the defaults, there’s nothing here:

``` r

results$feature_importance
#> [1] "Skipped feature importance"
```

## Customizing parameters

There are a few arguments that allow you to change how you execute
[`run_ml()`](http://www.schlosslab.org/mikropml/dev/reference/run_ml.md).
We’ve chosen reasonable defaults for you, but we encourage you to change
these if you think something else would be better for your data.

### Changing `kfold`, `cv_times`, and `training_frac`

- `kfold`: The number of folds to run for cross-validation (default: 5).
- `cv_times`: The number of times to run repeated cross-validation
  (default: 100).
- `training_frac`: The fraction of data for the training set (default:
  0.8). The rest of the data is used for testing.

Here’s an example where we change some of the default parameters:

``` r

results_custom <- run_ml(otu_mini_bin,
  "glmnet",
  kfold = 2,
  cv_times = 5,
  training_frac = 0.5,
  seed = 2019
)
#> Using 'dx' as the outcome column.
#> Training the model...
#> Loading required package: ggplot2
#> Loading required package: lattice
#> 
#> Attaching package: 'caret'
#> The following object is masked from 'package:mikropml':
#> 
#>     compare_models
#> Warning in (function (w) : `caret::train()` issued the following warning:
#>  
#> simpleWarning in nominalTrainWorkflow(x = x, y = y, wts = weights, info = trainInfo, : There were missing values in resampled performance measures.
#> 
#> This warning usually means that the model didn't converge in some cross-validation folds because it is predicting something close to a constant. As a result, certain performance metrics can't be calculated. This suggests that some of the hyperparameters chosen are doing very poorly.
#> Training complete.
```

You might have noticed that this one ran faster — that’s because we
reduced `kfold` and `cv_times`. This is okay for testing things out and
may even be necessary for smaller datasets. But in general it may be
better to have larger numbers for these parameters; we think the
defaults are a good starting point (Topçuoğlu et al. 2020).

#### Custom training indices

When `training_frac` is a fraction between 0 and 1, a random sample of
observations in the dataset are chosen for the training set to satisfy
the `training_frac` using
[`get_partition_indices()`](http://www.schlosslab.org/mikropml/dev/reference/get_partition_indices.md).
However, in some cases you might wish to control exactly which
observations are in the training set. You can instead assign
`training_frac` a vector of indices that correspond to which rows of the
dataset should go in the training set (all remaining sequences will go
in the testing set). Here’s an example with ~80% of the data in the
training set:

``` r

n_obs <- otu_mini_bin %>% nrow()
training_size <- 0.8 * n_obs
training_rows <- sample(n_obs, training_size)
results_custom_train <- run_ml(otu_mini_bin,
  "glmnet",
  kfold = 2,
  cv_times = 5,
  training_frac = training_rows,
  seed = 2019
)
#> Using 'dx' as the outcome column.
#> Using the custom training set indices provided by `training_frac`.
#>       The fraction of data in the training set will be 0.8
#> Training the model...
#> Training complete.
```

### Changing the performance metric

There are two arguments that allow you to change what performance metric
to use for model evaluation, and what performance metrics to calculate
using the test data.

`perf_metric_function` is the function used to calculate the performance
metrics.

The default for classification is
[`caret::multiClassSummary()`](https://rdrr.io/pkg/caret/man/postResample.html)
and the default for regression is
[`caret::defaultSummary()`](https://rdrr.io/pkg/caret/man/postResample.html).
We’d suggest not changing this unless you really know what you’re doing.

`perf_metric_name` is the column name from the output of
`perf_metric_function`. We chose reasonable defaults (AUC for binary,
logLoss for multiclass, and RMSE for continuous), but the default
functions calculate a bunch of different performance metrics, so you can
choose a different one if you’d like.

The default performance metrics available for classification are:

    #>  [1] "logLoss"                "AUC"                    "prAUC"                 
    #>  [4] "Accuracy"               "Kappa"                  "Mean_F1"               
    #>  [7] "Mean_Sensitivity"       "Mean_Specificity"       "Mean_Pos_Pred_Value"   
    #> [10] "Mean_Neg_Pred_Value"    "Mean_Precision"         "Mean_Recall"           
    #> [13] "Mean_Detection_Rate"    "Mean_Balanced_Accuracy"

The default performance metrics available for regression are:

    #> [1] "RMSE"     "Rsquared" "MAE"

Here’s an example using prAUC instead of AUC:

``` r

results_pr <- run_ml(otu_mini_bin,
  "glmnet",
  cv_times = 5,
  perf_metric_name = "prAUC",
  seed = 2019
)
#> Using 'dx' as the outcome column.
#> Training the model...
#> Warning in (function (w) : `caret::train()` issued the following warning:
#>  
#> simpleWarning in nominalTrainWorkflow(x = x, y = y, wts = weights, info = trainInfo, : There were missing values in resampled performance measures.
#> 
#> This warning usually means that the model didn't converge in some cross-validation folds because it is predicting something close to a constant. As a result, certain performance metrics can't be calculated. This suggests that some of the hyperparameters chosen are doing very poorly.
#> Training complete.
```

You’ll see that the cross-validation metric is prAUC, instead of the
default AUC:

``` r

results_pr$performance
#> # A tibble: 1 × 17
#>   cv_metric_prAUC logLoss   AUC prAUC Accuracy  Kappa    F1 Sensitivity
#>             <dbl>   <dbl> <dbl> <dbl>    <dbl>  <dbl> <dbl>       <dbl>
#> 1           0.577   0.691 0.663 0.605    0.538 0.0539 0.690           1
#> # ℹ 9 more variables: Specificity <dbl>, Pos_Pred_Value <dbl>,
#> #   Neg_Pred_Value <dbl>, Precision <dbl>, Recall <dbl>, Detection_Rate <dbl>,
#> #   Balanced_Accuracy <dbl>, method <chr>, seed <dbl>
```

### Using groups

The optional `groups` is a vector of groups to keep together when
splitting the data into train and test sets and for cross-validation.
Sometimes it’s important to split up the data based on a grouping
instead of just randomly. This allows you to control for similarities
within groups that you don’t want to skew your predictions (i.e. batch
effects). For example, with biological data you may have samples
collected from multiple hospitals, and you might like to keep
observations from the same hospital in the same partition.

Here’s an example where we split the data into train/test sets based on
groups:

``` r

# make random groups
set.seed(2019)
grps <- sample(LETTERS[1:8], nrow(otu_mini_bin), replace = TRUE)
results_grp <- run_ml(otu_mini_bin,
  "glmnet",
  cv_times = 2,
  training_frac = 0.8,
  groups = grps,
  seed = 2019
)
#> Using 'dx' as the outcome column.
#> Fraction of data in the training set: 0.795 
#>  Groups in the training set: A B D F G H 
#>  Groups in the testing set: C E
#> Groups will be kept together in CV partitions
#> Training the model...
#> Training complete.
```

The one difference here is
[`run_ml()`](http://www.schlosslab.org/mikropml/dev/reference/run_ml.md)
will report how much of the data is in the training set if you run the
above code chunk. This can be a little finicky depending on how many
samples and groups you have. This is because it won’t be exactly what
you specify with `training_frac`, since you have to include all of one
group in either the training set *or* the test set.

#### Controlling how groups are assigned to partitions

When you use the `groups` parameter as above, by default
[`run_ml()`](http://www.schlosslab.org/mikropml/dev/reference/run_ml.md)
will assume that you want all of the observations from each group to be
placed in the same partition of the train/test split. This makes sense
when you want to use groups to control for batch effects. However, in
some cases you might prefer to control exactly which groups end up in
which partition, and you might even be okay with some observations from
the same group being assigned to different partitions.

For example, say you want groups A and B to be used for training, C and
D for testing, and you don’t have a preference for what happens to the
other groups. You can give the `group_partitions` parameter a named list
to specify which groups should go in the training set and which should
go in the testing set.

``` r

results_grp_part <- run_ml(otu_mini_bin,
  "glmnet",
  cv_times = 2,
  training_frac = 0.8,
  groups = grps,
  group_partitions = list(
    train = c("A", "B"),
    test = c("C", "D")
  ),
  seed = 2019
)
#> Using 'dx' as the outcome column.
#> Fraction of data in the training set: 0.785 
#>  Groups in the training set: A B E F G H 
#>  Groups in the testing set: C D
#> Groups will not be kept together in CV partitions because the number of groups in the training set is not larger than `kfold`
#> Training the model...
#> Training complete.
```

In the above case, all observations from A & B will be used for
training, all from C & D will be used for testing, and the remaining
groups will be randomly assigned to one or the other to satisfy the
`training_frac` as closely as possible.

In another scenario, maybe you want only groups A through F to be used
for training, but you also want to allow other observations not selected
for training from A through F to be used for testing:

``` r

results_grp_trainA <- run_ml(otu_mini_bin,
  "glmnet",
  cv_times = 2,
  kfold = 2,
  training_frac = 0.5,
  groups = grps,
  group_partitions = list(
    train = c("A", "B", "C", "D", "E", "F"),
    test = c("A", "B", "C", "D", "E", "F", "G", "H")
  ),
  seed = 2019
)
#> Using 'dx' as the outcome column.
#> Fraction of data in the training set: 0.5 
#>  Groups in the training set: A B C D E F 
#>  Groups in the testing set: A B C D E F G H
#> Groups will be kept together in CV partitions
#> Training the model...
#> Training complete.
```

If you need even more control than this, take a look at [setting custom
training indices](#custom-training-indices). You might also prefer to
provide your own train control scheme with the `cross_val` parameter in
[`run_ml()`](http://www.schlosslab.org/mikropml/dev/reference/run_ml.md).

### More arguments

Some ML methods take optional arguments, such as `ntree` for
`randomForest`-based models or [case
weights](https://topepo.github.io/caret/train-models-by-tag.html#Accepts_Case_Weights).
Any additional arguments you give to
[`run_ml()`](http://www.schlosslab.org/mikropml/dev/reference/run_ml.md)
are forwarded along to
[`caret::train()`](https://rdrr.io/pkg/caret/man/train.html) so you can
leverage those options.

#### Case weights

If you want to use case weights, you will also need to use custom
indices for the training data (i.e. perform the partition before
`run_ml(`) as [above](#custom-training-indices)). Here’s one way to do
this with the weights calculated from the proportion of each class in
the data set, with ~70% of the data in the training set:

``` r

set.seed(20221016)
library(dplyr)
train_set_indices <- get_partition_indices(otu_mini_bin %>% pull(dx),
  training_frac = 0.70
)
case_weights_dat <- otu_mini_bin %>%
  count(dx) %>%
  mutate(p = n / sum(n)) %>%
  select(dx, p) %>%
  right_join(otu_mini_bin, by = "dx") %>%
  select(-starts_with("Otu")) %>%
  mutate(
    row_num = row_number(),
    in_train = row_num %in% train_set_indices
  ) %>%
  filter(in_train)
head(case_weights_dat)
#>       dx    p row_num in_train
#> 1 cancer 0.49       1     TRUE
#> 2 cancer 0.49       2     TRUE
#> 3 cancer 0.49       3     TRUE
#> 4 cancer 0.49       4     TRUE
#> 5 cancer 0.49       5     TRUE
#> 6 cancer 0.49       6     TRUE
tail(case_weights_dat)
#>         dx    p row_num in_train
#> 136 normal 0.51     194     TRUE
#> 137 normal 0.51     195     TRUE
#> 138 normal 0.51     196     TRUE
#> 139 normal 0.51     197     TRUE
#> 140 normal 0.51     198     TRUE
#> 141 normal 0.51     200     TRUE
nrow(case_weights_dat) / nrow(otu_mini_bin)
#> [1] 0.705
```

``` r

results_weighted <- run_ml(otu_mini_bin,
  "glmnet",
  outcome_colname = "dx",
  seed = 2019,
  training_frac = case_weights_dat %>% pull(row_num),
  weights = case_weights_dat %>% pull(p)
)
```

See the caret docs for [a list of models that accept case
weights](https://topepo.github.io/caret/train-models-by-tag.html#Accepts_Case_Weights).

## Finding feature importance

To find which features are contributing to predictive power, you can use
`find_feature_importance = TRUE`. How we use permutation importance to
determine feature importance is described in (Topçuoğlu et al. 2020).
Briefly, it permutes each of the features individually (or correlated
ones together) and evaluates how much the performance metric decreases.
The more performance decreases when the feature is randomly shuffled,
the more important that feature is. The default is `FALSE` because it
takes a while to run and is only useful if you want to know what
features are important in predicting your outcome.

Let’s look at some feature importance results:

``` r

results_imp <- run_ml(otu_mini_bin,
  "rf",
  outcome_colname = "dx",
  find_feature_importance = TRUE,
  seed = 2019
)
```

Now, we can check out the feature importances:

``` r

results_imp$feature_importance
#>    perf_metric perf_metric_diff     pvalue   lower   upper     feat method
#> 1    0.5459125        0.0003375 0.51485149 0.49125 0.60250 Otu00001     rf
#> 2    0.5682625       -0.0220125 0.72277228 0.50625 0.63125 Otu00002     rf
#> 3    0.5482875       -0.0020375 0.55445545 0.50500 0.59000 Otu00003     rf
#> 4    0.6314375       -0.0851875 1.00000000 0.55250 0.71250 Otu00004     rf
#> 5    0.4991750        0.0470750 0.08910891 0.44125 0.57125 Otu00005     rf
#> 6    0.5364875        0.0097625 0.28712871 0.50125 0.57375 Otu00006     rf
#> 7    0.5382875        0.0079625 0.39603960 0.47500 0.58750 Otu00007     rf
#> 8    0.5160500        0.0302000 0.09900990 0.46750 0.55750 Otu00008     rf
#> 9    0.5293375        0.0169125 0.16831683 0.49500 0.55625 Otu00009     rf
#> 10   0.4976500        0.0486000 0.12871287 0.41000 0.56250 Otu00010     rf
#>    perf_metric_name seed
#> 1               AUC 2019
#> 2               AUC 2019
#> 3               AUC 2019
#> 4               AUC 2019
#> 5               AUC 2019
#> 6               AUC 2019
#> 7               AUC 2019
#> 8               AUC 2019
#> 9               AUC 2019
#> 10              AUC 2019
```

There are several columns:

1.  `perf_metric`: The performance value of the permuted feature.
2.  `perf_metric_diff`: The difference between the performance for the
    actual and permuted data (i.e. test performance minus permuted
    performance). Features with a larger `perf_metric_diff` are more
    important.
3.  `pvalue`: the probability of obtaining the actual performance value
    under the null hypothesis.
4.  `lower`: the lower bound for the 95% confidence interval of
    `perf_metric`.
5.  `upper`: the upper bound for the 95% confidence interval of
    `perf_metric`.
6.  `feat`: The feature (or group of correlated features) that was
    permuted.
7.  `method`: The [ML method](#the-methods-we-support) used.
8.  `perf_metric_name`: The [name of the performance
    metric](#id_-changing-the-performance-metric) represented by
    `perf_metric` & `perf_metric_diff`.
9.  `seed`: The seed (if set).

As you can see here, the differences are negligible (close to zero),
which makes sense since our model isn’t great. If you’re interested in
feature importance, it’s especially useful to run multiple different
train/test splits, as shown in our [example snakemake
workflow](https://github.com/SchlossLab/mikropml-snakemake-workflow/).

You can also choose to permute correlated features together using
`corr_thresh` (default: 1). Any features that are above the correlation
threshold are permuted together; i.e. perfectly correlated features are
permuted together when using the default value.

``` r

results_imp_corr <- run_ml(otu_mini_bin,
  "glmnet",
  cv_times = 5,
  find_feature_importance = TRUE,
  corr_thresh = 0.2,
  seed = 2019
)
#> Using 'dx' as the outcome column.
#> Training the model...
#> Warning in (function (w) : `caret::train()` issued the following warning:
#>  
#> simpleWarning in nominalTrainWorkflow(x = x, y = y, wts = weights, info = trainInfo, : There were missing values in resampled performance measures.
#> 
#> This warning usually means that the model didn't converge in some cross-validation folds because it is predicting something close to a constant. As a result, certain performance metrics can't be calculated. This suggests that some of the hyperparameters chosen are doing very poorly.
#> Training complete.
#> Finding feature importance...
#> Feature importance complete.
results_imp_corr$feature_importance
#>   perf_metric perf_metric_diff     pvalue     lower     upper
#> 1   0.4941842        0.1531842 0.05940594 0.3236842 0.6473684
#>                                                                                        feat
#> 1 Otu00001|Otu00002|Otu00003|Otu00004|Otu00005|Otu00006|Otu00007|Otu00008|Otu00009|Otu00010
#>   method perf_metric_name seed
#> 1 glmnet              AUC 2019
```

You can see which features were permuted together in the `feat` column.
Here all 3 features were permuted together (which doesn’t really make
sense, but it’s just an example).

If you previously executed
[`run_ml()`](http://www.schlosslab.org/mikropml/dev/reference/run_ml.md)
without feature importance but now wish to find feature importance after
the fact, see the example code in the
[`get_feature_importance()`](http://www.schlosslab.org/mikropml/dev/reference/get_feature_importance.md)
documentation.

[`get_feature_importance()`](http://www.schlosslab.org/mikropml/dev/reference/get_feature_importance.md)
can show a live progress bar, see
[`vignette("parallel")`](http://www.schlosslab.org/mikropml/dev/articles/parallel.md)
for examples.

## Tuning hyperparameters (using the `hyperparameter` argument)

This is important, so we have a whole vignette about them. The bottom
line is we provide default hyperparameters that you can start with, but
it’s important to tune your hyperparameters. For more information about
what the default hyperparameters are, and how to tune hyperparameters,
see
[`vignette("tuning")`](http://www.schlosslab.org/mikropml/dev/articles/tuning.md).

## Other models

Here are examples of how to train and evaluate other models. The output
for all of them is very similar, so we won’t go into those details.

### Random forest

``` r

results_rf <- run_ml(otu_mini_bin,
  "rf",
  cv_times = 5,
  seed = 2019
)
```

The `rf` engine takes an optional argument `ntree`: the number of trees
to use for random forest. This can’t be tuned using the `rf` package
implementation of random forest. Please refer to `caret` documentation
if you are interested in other packages with random forest
implementations.

``` r

results_rf_nt <- run_ml(otu_mini_bin,
  "rf",
  cv_times = 5,
  ntree = 1000,
  seed = 2019
)
```

### Decision tree

``` r

results_dt <- run_ml(otu_mini_bin,
  "rpart2",
  cv_times = 5,
  seed = 2019
)
```

### SVM

``` r

results_svm <- run_ml(otu_mini_bin,
  "svmRadial",
  cv_times = 5,
  seed = 2019
)
```

If you get a message “maximum number of iterations reached”, see [this
issue](https://github.com/topepo/caret/issues/425) in caret.

## Other data

### Multiclass data

We provide `otu_mini_multi` with a multiclass outcome (three or more
outcomes):

``` r

otu_mini_multi %>%
  dplyr::pull("dx") %>%
  unique()
#> [1] "adenoma"   "carcinoma" "normal"
```

Here’s an example of running multiclass data:

``` r

results_multi <- run_ml(otu_mini_multi,
  outcome_colname = "dx",
  seed = 2019
)
```

The performance metrics are slightly different, but the format of
everything else is the same:

``` r

results_multi$performance
#> # A tibble: 1 × 17
#>   cv_metric_logLoss logLoss   AUC prAUC Accuracy  Kappa Mean_F1 Mean_Sensitivity
#>               <dbl>   <dbl> <dbl> <dbl>    <dbl>  <dbl> <chr>              <dbl>
#> 1              1.07    1.11 0.506 0.353    0.382 0.0449 NA                 0.360
#> # ℹ 9 more variables: Mean_Specificity <dbl>, Mean_Pos_Pred_Value <chr>,
#> #   Mean_Neg_Pred_Value <dbl>, Mean_Precision <chr>, Mean_Recall <dbl>,
#> #   Mean_Detection_Rate <dbl>, Mean_Balanced_Accuracy <dbl>, method <chr>,
#> #   seed <dbl>
```

### Continuous data

And here’s an example for running continuous data, where the outcome
column is numerical:

``` r

results_cont <- run_ml(otu_mini_bin[, 2:11],
  "glmnet",
  outcome_colname = "Otu00001",
  seed = 2019
)
```

Again, the performance metrics are slightly different, but the format of
the rest is the same:

``` r

results_cont$performance
#> # A tibble: 1 × 6
#>   cv_metric_RMSE  RMSE Rsquared   MAE method  seed
#>            <dbl> <dbl>    <dbl> <dbl> <chr>  <dbl>
#> 1           622.  731.   0.0893  472. glmnet  2019
```

## References

Tang, Shengpu, Parmida Davarmanesh, Yanmeng Song, Danai Koutra, Michael
W. Sjoding, and Jenna Wiens. 2020. “Democratizing EHR Analyses with
FIDDLE: A Flexible Data-Driven Preprocessing Pipeline for Structured
Clinical Data.” *J Am Med Inform Assoc*, ahead of print, October.
<https://doi.org/10.1093/jamia/ocaa139>.

Topçuoğlu, Begüm D., Nicholas A. Lesniak, Mack T. Ruffin, Jenna Wiens,
and Patrick D. Schloss. 2020. “A Framework for Effective Application of
Machine Learning to Microbiome-Based Classification Problems.” *mBio* 11
(3). <https://doi.org/10.1128/mBio.00434-20>.
