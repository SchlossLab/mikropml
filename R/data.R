#' Large OTU abundance dataset
#'
#' A dataset containing relatives abundances of 6,920 OTUs for 490 human stool samples
#'
#' @format A data frame with 490 rows and 6,921 variables.
#' The `dx` column is the diagnosis: healthy or cancerous (colorectal).
#' All other columns are OTU relative abundances.
"otu_large"

#' Medium-sized OTU abundance dataset
#'
#' A dataset containing relatives abundances of 6,920 OTUs for 60 human stool samples
#'
#' @format A data frame with 60 rows and 6921 variables.
#' The `dx` column is the diagnosis: healthy or cancerous (colorectal).
#' All other columns are OTU relative abundances.
"otu_medium"

#' Small OTU abundance dataset
#'
#' A dataset containing relatives abundances of 60 OTUs for 60 human stool samples
#'
#' @format A data frame with 60 rows and 61 variables.
#' The `dx` column is the diagnosis: healthy or cancerous (colorectal).
#' All other columns are OTU relative abundances.
"otu_small"

#' Mini OTU abundance dataset
#'
#' A dataset containing relatives abundances of OTUs for human stool samples
#'
#' @format A data frame
#' The `dx` column is the diagnosis: healthy or cancerous (colorectal).
#' All other columns are OTU relative abundances.
"otu_mini"

#' Small training dataset
#'
#' A training data partition from `otu_small`
#'
#' @format A data frame with 30 rows and 61 variables.
#' The `dx` column is the diagnosis: healthy or cancerous (colorectal).
#' All other columns are OTU relative abundances.
"train_data_sm"

#' A training data partition from `otu_mini`
"train_data_mini"

#' A test data partition from `otu_small`
"test_data_sm"

#' A test data partition from `otu_mini`
"test_data_mini"

#' A trained model from L2 logistic regression on `train_data_mini`
"trained_model_mini"

#' 2-fold cross validation on `train_data_mini` with grouped features
"otu_mini_cv2"

#' Results from running the pipline with L2 logistic regression on `otu_mini` with feature importance and grouping
"otu_mini_results1"

#' Results from running the pipline with random forest on `otu_mini`
"otu_mini_results2"

#' Results from running the pipline with svmRadial on `otu_mini`
"otu_mini_results3"

#' Results from running the pipline with xbgTree on `otu_mini`
"otu_mini_results4"

#' Results from running the pipline with rpart2 on `otu_mini`
"otu_mini_results5"
