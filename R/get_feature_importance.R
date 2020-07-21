

#' Check if package is installed
#'
#' @param package_name name of package to check
#'
#' @return boolean - whether package is installed (T) or not F).
#' @export
#'
#' @examples check_package_installed("base"); check_package_installed("asdf")
check_package_installed <- function(package_name){
  return(package_name %in% rownames(installed.packages()))
}

#' Use future apply if available
#'
#' @param fun apply function to use (apply, lapply, sapply, etc.)
#' @param ... all arguments to input to the apply function (in the correct order)
#'
#' @return
#' @export
#'
#' @examples select_apply(fun='sapply',1:10,function(x) x*10)
select_apply <- function(fun='apply',...){
  installed <- check_package_installed('future.apply')
  if(installed){
    fun_future = paste0('future_',fun)
    return(getFromNamespace(fun_future,'future.apply')(...))
  }else{
    return(get(fun)(...))
  }
}

#' Group correlated features
#'
#' @param corr output of get_corr_feats (pairs of correlated features)
#' @param test_data test data from machine learning
#'
#' @return 
#' @export
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#'
group_correlated_features <- function(corr, test_data) {
  all_feats <- colnames(test_data)[2:ncol(test_data)]
  corr_feats <- unique(c(corr$feature2, corr$feature1))
  noncorr_feats <- all_feats[!all_feats %in% corr_feats]

  grps <- as.list(noncorr_feats)
  accounted_for <- rep(NA, length(all_feats))
  af_length <- sum(!is.na(accounted_for))
  c <- length(grps) + 1
  for (i in corr_feats) {
    if (i %in% accounted_for) next
    feats <- unique(c(i, corr$feature1[corr$feature2 == i], corr$feature2[corr$feature1 == i]))
    new_feats <- T
    while (new_feats) {
      len_feats <- length(feats)
      for (j in feats) {
        feats <- unique(c(feats, j, corr$feature1[corr$feature2 == j], corr$feature2[corr$feature1 == j]))
      }
      new_feats <- length(feats) > len_feats
    }
    grps[[c]] <- feats
    af_length_new <- sum(af_length, length(feats))
    accounted_for[(af_length + 1):af_length_new] <- feats
    af_length <- af_length_new
    c <- c + 1
  }
  grps <- sapply(grps, paste, collapse = "|")
  return(grps)
}

#' Get permuted AUROC difference for a single feature (or group of features)
#'
#' @param model TODO
#' @param test_data TODO
#' @param outcome TODO
#' @param feat TODO
#' @param fewer_samples TODO
#'
#' @return
#' @export
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
find_permuted_auc <- function(model, test_data, outcome, feat, fewer_samples) {
  # -----------Get the original testAUC from held-out test data--------->
  # Calculate the test-auc for the actual pre-processed held-out data
  test_auc <- calc_aucs(model, test_data, outcome, fewer_samples)$auroc
  # permute grouped features together
  fs <- strsplit(feat, "\\|")[[1]]
  # only include ones in the test data split
  fs <- fs[fs %in% colnames(test_data)]
  # get the new AUC and AUC differences
  auc_diffs <- select_apply(fun='sapply',0:99, function(s) {
    set.seed(s)
    full_permuted <- test_data
    if (length(fs) == 1) {
      full_permuted[, fs] <- sample(full_permuted[, fs])
    } else {
      full_permuted[, fs] <- t(sample(data.frame(t(full_permuted[, fs]))))
    }

    # Calculate the new auc
    new_auc <- calc_aucs(model, full_permuted, outcome, fewer_samples)$auroc
    # Return how does this feature being permuted effect the auc
    return(c(new_auc = new_auc, diff = (test_auc - new_auc)))
  })
  auc <- mean(auc_diffs["new_auc", ])
  auc_diff <- mean(auc_diffs["diff", ])
  return(c(auc = auc, auc_diff = auc_diff))
}

#' Title
#'
#' @param dataset TODO
#' @param model TODO
#' @param test_data TODO
#' @param outcome_colname TODO
#' @param outcome_value TODO
#'
#' @return
#' @export
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
get_feature_importance <- function(dataset, model, test_data, outcome_colname, outcome_value) {

  # FIX THESE TWO LINES WHEN WE FIX THE BIGGER STRUCTURE
  outcome <- select(dataset,outcome_colname)
  features <- dataset[, !grepl(outcome_colname, names(dataset))]

  # ADD IN OPTION TO CHOOSE CORRELATION THRESHOLD
  corr_mat <- get_corr_feats(features)
  drop_cols <- c('cor')
  corr_mat <- corr_mat[, !(names(corr_mat) %in% drop_cols)]

  grps <- group_correlated_features(corr_mat, test_data)

  # ----------- Get feature importance of OTUs------------>
  # Permutate each feature in the non-correlated dimensional feature vector
  # Here we are
  #     1. Permuting the values in the OTU column randomly for each OTU in the list
  #     2. Applying the trained model to the new test-data where 1 OTU is randomly shuffled
  #     3. Getting the new AUROC value
  #     4. Calculating how much different the new AUROC is from original AUROC
  # Because we do this with lapply we randomly permute each OTU one by one.
  # We get the impact each non-correlated OTU makes in the prediction performance (AUROC)
  imps <- do.call("rbind", select_apply(fun='lapply',grps, function(feat) {
    res <- find_permuted_auc(model, test_data, outcome_colname, feat, outcome_value)
    return(res)
  }))

  imps <- as.data.frame(imps) %>%
    dplyr::mutate(names = factor(grps))

  # Save the importances
  return(imps)
}
