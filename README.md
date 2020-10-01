
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mikropml

> meek-ROPE em el

<!-- badges: start -->

[![check](https://github.com/SchlossLab/mikropml/workflows/check/badge.svg)](https://github.com/SchlossLab/mikropml/actions)
[![codecov](https://codecov.io/gh/SchlossLab/mikropml/branch/master/graph/badge.svg)](https://codecov.io/gh/SchlossLab/mikropml)
[![docs](https://img.shields.io/badge/docs-here-brightgreen)](http://www.schlosslab.org/mikropml/)
[![license](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/SchlossLab/mikropml/LICENSE)
[![lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

User-Friendly Machine Learning Package for Classification Problems

Machine learning (ML) for classification of data into groups is a popular tool. It is now being used to make high stakes decisions in healthcare, economics, criminal justice and more.

However, implementing a robust ML classification pipeline can be time-consuming, confusing, and difficult. Here, we present __mikRopML__, an easy-to-use R package that acts as a wrapper around the R caret package and can be used for binary classification problems using L2-regularied logistic regression, SVM with radial basis kernel, decision tree and random forest, and xgBoost.

__mikRopML__ provides functions to facilitate the crucial steps of ML: pre-processing, cross-validation, testing, model evaluation, and model interpretation.




## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("SchlossLab/mikropml")
```

### Dependences

  - Required: caret, dplyr, LiblineaR, PRROC, rlang, stats, utils
  - Optional: doFuture, foreach, future, future.apply, kernlab,
    randomForest, rpart, xgboost
  - For developers: knitr, rmarkdown, testthat

## Usage

Check out the [introductory
vignette](http://www.schlosslab.org/mikropml/articles/introduction.html)
for a quick start tutorial.

## Help & Contributing

If you come across a bug, [open an
issue](https://github.com/SchlossLab/mikropml/issues) and include a
[minimal reproducible example](https://www.tidyverse.org/help/).

If you’d like to contribute, see our guidelines
[here](.github/CONTRIBUTING.md).

## Why the name?

The word “mikrop” (pronounced “meek-ROPE”) is Turkish for “microbe”.
This package was originally implemented as a machine learning pipeline
for microbiome-based classification problems (see [Topçuoğlu *et al.*
2020](https://doi.org/10.1128/mBio.00434-20)). We realized that these
methods are applicable in many other fields too, but stuck with the name
because we like it\!
