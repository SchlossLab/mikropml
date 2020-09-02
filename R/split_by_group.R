# split by group (e.g. facility)
# try to get ~split defined by user (training_frac)

#' Split into train and test set while splitting by group
#'
#' @param group vector of groups whose length matches the number of rows in the dataset
#' @param p maximum percentage of data that goes to training (maybe less depending on group sizes)
#'
#' @return row position integers corresponding to the training data
#' @export
#'
#' @examples 
#' group <- c('A','B','A','B','C','C','A','A','D')
#' set.seed(0)
#' train_ind <- createGroupedDataPartition(group,0.8)
createGroupedDataPartition = function(group, p) {
  # get indices
  indices = seq(along = group)
  # get unique groups
  grps = unlist(unique(group))
  # initialize train groups & set
  train_grps = grps
  train_set = indices
  train_set_grp = group
  # initialize fraction of data in train set
  frac_in_train = length(train_set)/length(indices)
  # keep removing data from train set until fraction in train set <= p (e.g. 0.8)
  while(frac_in_train > p){
    # randomly choose a group
    grp = sample(train_grps, size=1)
    # remove group from train groups & set
    train_grps = train_grps[train_grps != grp]
    train_set = train_set[train_set_grp != grp]
    train_set_grp = train_set_grp[train_set_grp != grp]
    # calcuate fraction of data in train set
    frac_in_train = length(train_set)/length(indices)
  }
  message(paste0("Fraction of data in the training set: ", frac_in_train, '.'))
  # get train group indices
  return(train_set)
} 


#' Splitting into folds for cross-validation when using groups
#'
#' Like \link[caret]{createMultiFolds} but still splitting by group using \link[caret]{groupKFold}. Code modified from \link[caret]{createMultiFolds}.
#' 
#' @param group equivalent to y in caret::createMultiFolds
#' @param kfold equivalent to k in caret::createMultiFolds
#' @param cv_times equivalent to cv_times in caret::createMultiFolds
#' @inheritParams run_ml
#'
#' @return indices of folds for CV
#' @export
#'
#' @examples
#' set.seed(0)
#' group <- c('A','B','A','B','C','C','A','A','D')
#' folds <- groupKMultiFolds(group, kfold=2, cv_times=2)
groupKMultiFolds <- function (group, kfold = 10, cv_times = 5) 
{
  if (class(group)[1] == "Surv") 
    group <- group[, "time"]
  prettyNums <- paste("Rep", gsub(" ", "0", format(1:cv_times)), 
                      sep = "")
  for (i in 1:cv_times) {
    tmp <- caret::groupKFold(group, k = kfold)
    names(tmp) <- paste("Fold", gsub(" ", "0", format(seq(along = tmp))), 
                        ".", prettyNums[i], sep = "")
    out <- if (i == 1) 
      tmp
    else c(out, tmp)
  }
  if(any(sapply(out, length) == 0)){
    stop("Could not split the data into train and validate folds. This could mean you do not have enough samples or groups to perform an ML analysis using the grouping functionality. Alternatively, you can try another seed, or decrease kfold or cv_times.")
  }
  out
}
