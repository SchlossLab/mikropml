
#' caret contr.ltfr
#' @importFrom caret contr.ltfr
#' @export
caret::contr.ltfr

#' dplyr pipe
#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`

#' rlang data pronoun
#' @importFrom rlang .data
#' @export
rlang::.data

#' @importFrom rlang !!
#' @export
rlang::`!!`

#' @importFrom rlang :=
#' @export
rlang::`:=`

# make R CMD CHECK shut up about the dot `.``
# See: \url{https://github.com/tidyverse/magrittr/issues/29}
utils::globalVariables(c("."))

# Suppress R CMD check note 'All declared Imports should be used'.
# These packages are used by caret to train models and evaluate performance.
# The datasets cannot be loaded if these packages aren't declared in Imports.
# See \url{https://community.rstudio.com/t/how-should-a-meta-package-handle-this-note-all-declared-imports-should-be-used/23400/3}
#' @importFrom MLmetrics AUC
#' @importFrom e1071 best.randomForest
#' @importFrom glmnet glmnet
#' @importFrom kernlab as.kernelMatrix
#' @importFrom randomForest getTree
#' @importFrom rpart rpart
#' @importFrom xgboost xgboost
NULL
