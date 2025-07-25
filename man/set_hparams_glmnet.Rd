% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hyperparameters.R
\name{set_hparams_glmnet}
\alias{set_hparams_glmnet}
\title{Set hyperparameters for regression models for use with glmnet}
\usage{
set_hparams_glmnet()
}
\value{
default lambda & alpha values
}
\description{
Alpha is set to \code{0} for ridge (L2). An alpha of \code{1} would make it lasso (L1).
}
\author{
Zena Lapp, \email{zenalapp@umich.edu}
}
\keyword{internal}
