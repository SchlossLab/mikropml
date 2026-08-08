#!/usr/bin/env Rscript

install.packages('remotes', repos = "https://cloud.r-project.org")

# see issue #352
remotes::install_version('xgboost', c('> 1.6', '< 1.8'))

# install all dependencies, but do not upgrade already-installed pkg versions
remotes::install_local('/opt/mikropml', dependencies=TRUE, upgrade=FALSE)
# raise an error if mikropml was not actually installed
packageVersion('mikropml')

dir.create('/data')
readr::write_csv(tibble::as_tibble(installed.packages()), '/data/r-packages.csv')
