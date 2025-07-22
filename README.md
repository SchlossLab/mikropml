
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mikropml <a href='http://www.schlosslab.org/mikropml/'><img src='man/figures/logo.png' align="right" height="120" /></a>

> meek-ROPE em el

User-Friendly R Package for Supervised Machine Learning Pipelines

<!-- badges: start -->

[![check](https://github.com/SchlossLab/mikropml/workflows/check/badge.svg)](https://github.com/SchlossLab/mikropml/actions?query=workflow%3Acheck+branch%3Amain)
[![codecov](https://codecov.io/gh/SchlossLab/mikropml/branch/main/graph/badge.svg)](https://app.codecov.io/gh/SchlossLab/mikropml)
[![license](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/SchlossLab/mikropml/blob/main/LICENSE.md)
[![CRAN](https://img.shields.io/cran/v/mikropml?color=blue&label=CRAN&logo=R)](https://CRAN.R-project.org/package=mikropml)
[![Conda](https://img.shields.io/conda/vn/conda-forge/r-mikropml)](https://anaconda.org/conda-forge/r-mikropml)
[![DOI](https://joss.theoj.org/papers/10.21105/joss.03073/status.svg)](https://doi.org/10.21105/joss.03073)
<!-- badges: end -->

An interface to build machine learning models for classification and
regression problems. `mikropml` implements the ML pipeline described by
[Topçuoğlu *et al.* (2020)](https://doi.org/doi:10.1128/mBio.00434-20)
with reasonable default options for data preprocessing, hyperparameter
tuning, cross-validation, testing, model evaluation, and interpretation
steps. See the [website](http://www.schlosslab.org/mikropml/) for more
information, documentation, and examples.

## Installation

You can install the latest release from
[CRAN](https://cran.r-project.org/package=mikropml):
<img src='https://cranlogs.r-pkg.org/badges/grand-total/mikropml' align='right'/>

``` r
install.packages('mikropml')
```

or the development version from
[GitHub](https://github.com/SchlossLab/mikRopML):

``` r
# install.packages("devtools")
devtools::install_github("SchlossLab/mikropml")
```

or install from a terminal using
[conda](https://docs.conda.io/projects/conda/en/latest/index.html) or
[mamba](https://mamba.readthedocs.io/en/latest/):
<img src='https://anaconda.org/conda-forge/r-mikropml/badges/downloads.svg' align='right'/>

``` bash
mamba install -c conda-forge r-mikropml
```

### Dependencies

- Imports: caret, dplyr, e1071, glmnet, kernlab, MLmetrics,
  randomForest, rlang, rpart, stats, tidyselect, utils, xgboost
- Suggests: assertthat, doFuture, forcats, foreach, future,
  future.apply, furrr, ggplot2, knitr, progress, progressr, purrr,
  rmarkdown, rsample, testthat, tidyr

## Usage

Check out the [introductory
vignette](http://www.schlosslab.org/mikropml/articles/introduction.html)
for a quick start tutorial. For a more in-depth discussion, read [all
the vignettes](http://www.schlosslab.org/mikropml/articles/index.html)
and/or take a look at the [reference
documentation](http://www.schlosslab.org/mikropml/reference/index.html).

You can watch the Riffomonas Project series of [video
tutorials](https://www.youtube.com/playlist?list=PLmNrK_nkqBpKpzb9-vI4V7SdXC-jXEcmg)
covering mikropml and other skills related to machine learning.

We also provide a [Snakemake
workflow](https://github.com/SchlossLab/mikropml-snakemake-workflow) for
running `mikropml` locally or on an HPC. We highly recommend running
`mikropml` with Snakemake or another workflow management system for
reproducibility and scalability of ML analyses.

<a href="https://github.com/SchlossLab/mikropml-snakemake-workflow">
<img src="https://raw.githubusercontent.com/SchlossLab/mikropml-snakemake-workflow/main/figures/mikropml-snakemake-workflow.png" 
height="120" align="center" /> </a>

## Help & Contributing

If you come across a bug, [open an
issue](https://github.com/SchlossLab/mikropml/issues) and include a
[minimal reproducible example](https://www.tidyverse.org/help/).

If you have questions, create a new post in
[Discussions](https://github.com/SchlossLab/mikropml/discussions).

If you’d like to contribute, see our guidelines
[here](http://www.schlosslab.org/mikropml/CONTRIBUTING.html).

## Code of Conduct

Please note that the mikropml project is released with a [Contributor
Code of
Conduct](http://www.schlosslab.org/mikropml/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.

## License

The mikropml package is licensed under [the MIT
license](https://github.com/SchlossLab/mikropml/blob/main/LICENSE.md).
Text and images included in this repository, including the mikropml
logo, are licensed under the [CC BY 4.0
license](https://creativecommons.org/licenses/by/4.0/).

## Citation

To cite mikropml in publications, use:

> <p>
> Topçuoğlu BD, Lapp Z, Sovacool KL, Snitkin E, Wiens J, Schloss PD
> (2021). “mikropml: User-Friendly R Package for Supervised Machine
> Learning Pipelines.” <em>Journal of Open Source Software</em>,
> <b>6</b>(61), 3073.
> <a href="https://doi.org/10.21105/joss.03073">doi:10.21105/joss.03073</a>,
> <a href="https://joss.theoj.org/papers/10.21105/joss.03073">https://joss.theoj.org/papers/10.21105/joss.03073</a>.
> </p>

A BibTeX entry for LaTeX users is:

     @Article{,
      title = {{mikropml}: User-Friendly R Package for Supervised Machine Learning Pipelines},
      author = {Begüm D. Topçuoğlu and Zena Lapp and Kelly L. Sovacool and Evan Snitkin and Jenna Wiens and Patrick D. Schloss},
      journal = {Journal of Open Source Software},
      year = {2021},
      volume = {6},
      number = {61},
      pages = {3073},
      doi = {10.21105/joss.03073},
      url = {https://joss.theoj.org/papers/10.21105/joss.03073},
    } 

## Why the name?

The word “mikrop” (pronounced “meek-ROPE”) is Turkish for “microbe”.
This package was originally implemented as a machine learning pipeline
for microbiome-based classification problems (see [Topçuoğlu *et al.*
2020](https://doi.org/10.1128/mBio.00434-20)). We realized that these
methods are applicable in many other fields too, but stuck with the name
because we like it!
