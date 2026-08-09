# mikropml

> meek-ROPE em el

User-Friendly R Package for Supervised Machine Learning Pipelines

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
![](https://cranlogs.r-pkg.org/badges/grand-total/mikropml)

``` r

install.packages('mikropml')
```

or from [r-universe](https://schlosslab.r-universe.dev/mikropml):

``` r

install.packages('mikropml', repos = c('https://schlosslab.r-universe.dev',
                                       'https://cloud.r-project.org',
                                       'https://bioc.r-universe.dev'
                                       )
)
```

or install the development version from
[GitHub](https://github.com/SchlossLab/mikRopML):

``` r

# install.packages("remotes")
remotes::install_github("SchlossLab/mikropml")
```

or install from a terminal using
[conda](https://docs.conda.io/projects/conda/en/latest/index.html) or
[mamba](https://mamba.readthedocs.io/en/latest/):
![](https://anaconda.org/conda-forge/r-mikropml/badges/downloads.svg)

``` bash
mamba install -c conda-forge r-mikropml
```

### ⚠️ xgboost error

There is a bug in later versions of xgboost with caret. You will need to
downgrade your xgboost version to \<= 1.7 if you wish to use xgbTree.
See [\#362](https://github.com/SchlossLab/mikropml/issues/362) for more
details.

### Dependencies

- Imports: caret, dplyr, e1071, glmnet, kernlab, methods, MLmetrics,
  randomForest, rlang, rpart, S4Vectors, SingleCellExperiment, stats,
  SummarizedExperiment, tidyselect, TreeSummarizedExperiment, utils,
  xgboost
- Suggests: assertthat, doFuture, forcats, foreach, furrr, future,
  future.apply, ggplot2, knitr, progress, progressr, purrr, rmarkdown,
  roxygen2, rsample, styler, testthat, tidyr, usethis

## Usage

Check out the [introductory
vignette](http://www.schlosslab.org/mikropml/articles/introduction.md)
for a quick start tutorial. For a more in-depth discussion, read [all
the vignettes](http://www.schlosslab.org/mikropml/articles/index.md)
and/or take a look at the [reference
documentation](http://www.schlosslab.org/mikropml/reference/index.md).

You can watch the Riffomonas Project series of [video
tutorials](https://www.youtube.com/playlist?list=PLmNrK_nkqBpKpzb9-vI4V7SdXC-jXEcmg)
covering mikropml and other skills related to machine learning.

We also provide a [Snakemake
workflow](https://github.com/SchlossLab/mikropml-snakemake-workflow) for
running `mikropml` locally or on an HPC. We highly recommend running
`mikropml` with Snakemake or another workflow management system for
reproducibility and scalability of ML analyses.

[![](https://raw.githubusercontent.com/SchlossLab/mikropml-snakemake-workflow/main/figures/mikropml-snakemake-workflow.png)](https://github.com/SchlossLab/mikropml-snakemake-workflow)

## Help & Contributing

If you come across a bug, [open an
issue](https://github.com/SchlossLab/mikropml/issues) and include a
[minimal reproducible example](https://tidyverse.org/help/).

If you have questions, create a new post in
[Discussions](https://github.com/SchlossLab/mikropml/discussions).

If you’d like to contribute, see our guidelines
[here](http://www.schlosslab.org/mikropml/CONTRIBUTING.md).

## Code of Conduct

Please note that the mikropml project is released with a [Contributor
Code of Conduct](http://www.schlosslab.org/mikropml/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

## License

The mikropml package is licensed under [the MIT
license](https://github.com/SchlossLab/mikropml/blob/main/LICENSE.md).
Text and images included in this repository, including the mikropml
logo, are licensed under the [CC BY 4.0
license](https://creativecommons.org/licenses/by/4.0/).

## Citation

To cite mikropml in publications, use:

> Topçuoğlu BD, Lapp Z, Sovacool KL, Snitkin E, Wiens J, Schloss PD
> (2021). “mikropml: User-Friendly R Package for Supervised Machine
> Learning Pipelines.” *Journal of Open Source Software*, **6**(61),
> 3073. [doi:10.21105/joss.03073](https://doi.org/10.21105/joss.03073).
> <https://joss.theoj.org/papers/10.21105/joss.03073>.

A BibTeX entry for LaTeX users is:

``` R
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
```

## Why the name?

The word “mikrop” (pronounced “meek-ROPE”) is Turkish for “microbe”.
This package was originally implemented as a machine learning pipeline
for microbiome-based classification problems (see [Topçuoğlu *et al.*
2020](https://doi.org/10.1128/mBio.00434-20)). We realized that these
methods are applicable in many other fields too, but stuck with the name
because we like it!
