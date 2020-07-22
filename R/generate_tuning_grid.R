#' Generate the Tuning Grid for Tuning Hyperparameters
#'
#' @param model Model name
#' @param hyperparameters Dataframe with hyperparameters. This dataframe should have the first column named as "param" which is the hyperparameter name and the second column "val" which are the values to be tested and third column "model" which is the model being used.
#'
#' @return List of the grid and the training package being used.
#' @export
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#'
#'
#' @examples
#' generate_tuning_grid("L2_Logistic_Regression", default_hyperparams)
generate_tuning_grid <- function(model, hyperparameters) {

  hyperparameters <- hyperparameters[hyperparameters$model == model, ]
  hyperparameters <- split(hyperparameters$val, hyperparameters$param)


  # -------------------Classification Method Definition---------------------->

  # ------------------------------------------------------------------------->
  # For linear models we are using LiblineaR package
  #
  # LiblineaR can produce 10 types of (generalized) linear models:
  # The regularization can be
  #     1. L1
  #     2. L2
  # The losses can be:
  #     1. Regular L2-loss for SVM (hinge loss),
  #     2. L1-loss for SVM
  #     3. Logistic loss for logistic regression.
  #
  # Here we will use L1 and L2 regularization and hinge loss (L2-loss) for linear SVMs
  # We will use logistic loss for L2-resularized logistic regression
  # The liblinear 'type' choioces are below:
  #
  # for classification
  # • 0 – L2-regularized logistic regression (primal)---> we use this for l2-logistic
  #  • 1 – L2-regularized L2-loss support vector classification (dual)
  #  • 2 – L2-regularized L2-loss support vector classification (primal) ---> we use this for l2-linear SVM
  #  • 3 – L2-regularized L1-loss support vector classification (dual)
  #  • 4 – support vector classification by Crammer and Singer
  #  • 5 – L1-regularized L2-loss support vector classification---> we use this for l1-linear SVM
  #  • 6 – L1-regularized logistic regression
  #  • 7 – L2-regularized logistic regression (dual)
  #
  # for regression
  #  • 11 – L2-regularized L2-loss support vector regression (primal)
  #  • 12 – L2-regularized L2-loss support vector regression (dual)
  #  • 13 – L2-regularized L1-loss support vector regression (dual)
  # ------------------------------------------------------------------------>

  # Grid and caret method defined for each classification models
  # TODO: named list instead of if/else block? could probably use a quosure to delay evaluation
  if (model == "L2_Logistic_Regression") {
    grid <- expand.grid(
      cost = hyperparameters$cost,
      loss = "L2_primal",
      # This chooses type=0 for liblinear R package
      # which is logistic loss, primal solve for L2 regularized logistic regression
      epsilon = 0.01
    ) # default epsilon recommended from liblinear
    method <- "regLogistic"
  }
  else if (model == "RBF_SVM") {
    grid <- expand.grid(
      sigma = hyperparameters$sigma,
      C = hyperparameters$C
    )
    method <- "svmRadial"
  }
  else if (model == "Decision_Tree") {
    grid <- expand.grid(maxdepth = hyperparameters$maxdepth) # maybe change these default parameters?
    method <- "rpart2"
  }
  else if (model == "Random_Forest") {
    grid <- expand.grid(mtry = hyperparameters$mtry)
    method <- "rf"
  }
  else if (model == "XGBoost") {
    grid <- expand.grid(
      nrounds = hyperparameters$nrounds,
      gamma = hyperparameters$gamma,
      eta = hyperparameters$eta,
      max_depth = hyperparameters$max_depth,
      colsample_bytree = hyperparameters$colsample_bytree,
      min_child_weight = hyperparameters$min_child_weight,
      subsample = hyperparameters$subsample
    )
    method <- "xgbTree"
  }
  else {
    message("Model not available")
  }
  # Return:
  #     1. the hyper-parameter grid to tune
  #     2. the caret function to train with
  params <- list(grid = grid, method = method)
  return(params)
}
