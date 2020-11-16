
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mikropml

> meek-ROPE em el

<!-- badges: start -->

[![check](https://github.com/SchlossLab/mikropml/workflows/check/badge.svg)](https://github.com/SchlossLab/mikropml/actions?query=workflow%3Acheck+branch%3Amaster)
[![codecov](https://codecov.io/gh/SchlossLab/mikropml/branch/master/graph/badge.svg)](https://codecov.io/gh/SchlossLab/mikropml)
[![docs](https://img.shields.io/badge/docs-here-brightgreen)](http://www.schlosslab.org/mikropml/)
[![license](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/SchlossLab/mikropml/blob/master/LICENSE.md)
[![lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

User-Friendly R Package for Robust Machine Learning Pipelines

## Installation

You can install the development version from
[GitHub](https://github.com/SchlossLab/mikRopML) with:

``` r
# install.packages("devtools")
devtools::install_github("SchlossLab/mikropml")
```

### Dependencies

  - Imports: caret, dplyr, glmnet, kernlab, randomForest, rlang, stats,
    utils, xgboost
  - Suggests: doFuture, e1071, foreach, future, future.apply, ggplot2,
    knitr, MLmetrics, purrr, rmarkdown, rpart, testthat, tidyr

## Usage

Check out the [introductory
vignette](http://www.schlosslab.org/mikropml/articles/introduction.html)
for a quick start tutorial. For a more in-depth discussion, read [all
the vignettes](http://www.schlosslab.org/mikropml/articles/index.html)
and/or take a look at the [reference
documentation](http://www.schlosslab.org/mikropml/reference/index.html).

## Help & Contributing

If you come across a bug, [open an
issue](https://github.com/SchlossLab/mikropml/issues) and include a
[minimal reproducible example](https://www.tidyverse.org/help/).

If you’d like to contribute, see our guidelines
[here](http://www.schlosslab.org/mikropml/CONTRIBUTING.html).

## Code of Conduct

Please note that the mikropml project is released with a [Contributor
Code of
Conduct](http://www.schlosslab.org/mikropml/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.

## Why the name?

The word “mikrop” (pronounced “meek-ROPE”) is Turkish for “microbe”.
This package was originally implemented as a machine learning pipeline
for microbiome-based classification problems (see [Topçuoğlu *et al.*
2020](https://doi.org/10.1128/mBio.00434-20)). We realized that these
methods are applicable in many other fields too, but stuck with the name
because we like it\!
