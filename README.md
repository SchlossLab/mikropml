
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mikRopML

> meek-ROPE em el

<!-- badges: start -->

[![check](https://github.com/SchlossLab/mikRopML/workflows/check/badge.svg)](https://github.com/SchlossLab/mikRopML/actions)
[![codecov](https://codecov.io/gh/SchlossLab/mikRopML/branch/master/graph/badge.svg)](https://codecov.io/gh/SchlossLab/mikRopML)
[![docs](https://img.shields.io/badge/docs-here-brightgreen)](https://schlosslab.org/mikRopML/)
[![license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

Machine learning (ML) for classification of data into groups is a popular tool. It is now being used to make high stakes decisions in healthcare, economics, criminal justice and more. 

However, implementing a robust ML classification pipeline can be time-consuming, confusing, and difficult. Here, we present __mikRopML__, an easy-to-use R package that acts as a wrapper around the R caret package and can be used for binary classification problems using L2-regularied logistic regression, SVM with radial basis kernel, decision tree and random forest, and xgBoost.

__mikRopML__ provides functions to facilitate the crucial steps of ML: pre-processing, cross-validation, testing, model evaluation, and model interpretation.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("SchlossLab/mikRopML")
```

## Example

TODO
