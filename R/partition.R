#' Select indices to partition the data into training & testing sets.
#'
#' Use this function to get the row indices for the training set.
#'
#' If `groups` is `NULL`, uses \link[caret]{createDataPartition}.
#' Otherwise, uses `create_grouped_data_partition()`.
#'
#' Set the seed prior to calling this function if you would like your data
#' partitions to be reproducible (recommended).
#'
#' @param outcomes vector of outcomes
#' @inheritParams run_ml
#'
#' @return Vector of row indices for the training set.
#' @export
#' @author Kelly Sovacool, {sovacool@@umich.edu}
#'
#' @examples
#' training_inds <- get_partition_indices(otu_mini_bin$dx)
#' train_data <- otu_mini_bin[training_inds, ]
#' test_data <- otu_mini_bin[-training_inds, ]
get_partition_indices <- function(outcomes,
                                  training_frac = 0.8,
                                  groups = NULL,
                                  group_partitions = NULL) {
    check_training_frac(training_frac)
    if (is.null(groups)) {
      training_inds <- caret::createDataPartition(outcomes,
                                                  p = training_frac,
                                                  list = FALSE) %>% .[, 1]
    } else {
      training_inds <-
        create_grouped_data_partition(groups,
                                      group_partitions = group_partitions,
                                      training_frac = training_frac)
    }
    return(training_inds)
  }


# split by groups (e.g. facility)
# try to get ~split defined by user (training_frac)

#' Split into train and test set while splitting by groups.
#' When `group_partitions` is `NULL`, all samples from each group will go into
#' either the training set or the testing set.
#' Otherwise, the groups will be split according to `group_partitions`
#'
#' @inheritParams run_ml
#'
#' @return vector of row indices for the training set
#' @noRd
#' @author Zena Lapp, {zenalapp@@umich.edu}
#' @author Kelly Sovacool, {sovacool@@umich.edu}
#'
#' @examples
#' groups <- c("A", "B", "A", "B", "C", "C", "A", "A", "D")
#' set.seed(0)
#' create_grouped_data_partition(groups, training_frac = 0.8)
#' groups <- rep.int(c("A", "B", "C"), 3)
#' create_grouped_data_partition(groups, group_partitions = list(train = c("A"), test = c("A", "B", "C")))
create_grouped_data_partition <- function(groups, group_partitions = NULL, training_frac = 0.8) {
  # get indices
  indices <- seq(along = groups)
  # get unique groups
  unique_groups <- unlist(unique(groups))
  if (is.null(group_partitions)) {
    # initialize train groups & set
    train_grps <- unique_groups
    train_set <- indices
    train_set_grp <- groups
    # initialize fraction of data in train set
    frac_in_train <- length(train_set) / length(indices)
    # keep removing data from train set until fraction in train set <= p (e.g. 0.8)
    while (frac_in_train > training_frac) {
      # randomly choose a groups
      grp <- sample(train_grps, size = 1)
      # remove groups from train groups & set
      train_grps <- train_grps[train_grps != grp]
      train_set <- train_set[train_set_grp != grp]
      train_set_grp <- train_set_grp[train_set_grp != grp]
      # calculate fraction of data in train set
      frac_in_train <- length(train_set) / length(indices)
    }
  } else {
    names_unrecognized <- names(group_partitions[!names(group_partitions) %in% c("train", "test")])
    if (length(names_unrecognized) > 0) {
      stop(paste("Unrecognized name(s) in `group_partitions`:",
                  names_unrecognized))
    }
    in_train_only <- setdiff(group_partitions$train, group_partitions$test)
    in_test_only <- setdiff(group_partitions$test, group_partitions$train)
    in_both <- intersect(group_partitions$test, group_partitions$train)
    in_neither <- setdiff(unique_groups, union(group_partitions$test, group_partitions$train))

    # initialize train & test sets with samples that must be in one or the other
    train_set <- indices[groups %in% in_train_only]
    test_set <- indices[groups %in% in_test_only]

    # sample from remaining samples to reach target training fraction
    remaining <- indices[-c(train_set, test_set)]
    if (length(remaining) > 0) {
      num_needed = round(training_frac * length(indices) - length(train_set))
      if (num_needed > 0) {
        more_train_samples <- indices[sample(remaining, size = num_needed)]
        train_set <- c(train_set, more_train_samples)
      }
    }
  }
  frac_in_train <- length(train_set) / length(indices)
  message(paste0("Fraction of data in the training set: ",
                 round(frac_in_train, 3),
                 "."))
  return(train_set)
}
