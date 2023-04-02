#' Calculate the difference in the mean of the metric for two groups
#'
#' @param sub_data subset of the merged performance data frame for two groups
#' @param group_name name of column with group variable
#' @param metric metric to compare
#'
#' @return numeric difference in the average metric between the two groups
#'
#' @keywords internal
#' @author Courtney Armour, \email{armourc@@umich.edu}
#' @examples
#' \dontrun{
#' df <- dplyr::tibble(
#'   condition = c("a", "a", "b", "b"),
#'   AUC = c(.2, 0.3, 0.8, 0.9)
#' )
#' get_difference(df, "condition", "AUC")
#' }
get_difference <- function(sub_data, group_name, metric) {
  meanVal <- NULL # suppresses R CMD check Note "no visible binding for global variable"
  if (!is.numeric(sub_data %>% dplyr::pull(metric))) {
    stop(paste0(
      "The metric `", metric,
      "` is not numeric, please check that you specified the right column."
    ))
  }
  means <- sub_data %>%
    dplyr::group_by(.data[[group_name]]) %>%
    dplyr::summarise(meanVal = mean(.data[[metric]]), .groups = "drop") %>%
    dplyr::pull(meanVal)
  abs(diff(means))
}

#' Shuffle the rows in a column
#'
#' @param dat a data frame containing `col_name`
#' @param col_name column name to shuffle
#'
#' @return `dat` with the rows of `col_name` shuffled
#' @keywords internal
#' @author Courtney R Armour, \email{armourc@@umich.edu}
#' @examples
#' \dontrun{
#' set.seed(123)
#' df <- dplyr::tibble(
#'   condition = c("a", "a", "b", "b"),
#'   AUC = c(.2, 0.3, 0.8, 0.9)
#' )
#' shuffle_group(df, "condition")
#' }
shuffle_group <- function(dat, col_name) {
  if (!(col_name %in% colnames(dat))) {
    stop(paste0("The col_name `", col_name, "` does not exist in the data frame."))
  }
  group_vals <- dat %>%
    dplyr::pull({{ col_name }})
  group_vals_shuffled <- base::sample(group_vals)

  data_shuffled <- dat %>%
    dplyr::mutate(!!col_name := group_vals_shuffled)

  return(data_shuffled)
}

#' Calculated a permuted p-value comparing two models
#'
#' @inheritParams compare_models
#' @param group_1 name of one group to compare
#' @param group_2 name of other group to compare
#'
#' @return numeric p-value comparing two models
#' @export
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#' @author Courtney R Armour, \email{armourc@@umich.edu}
#'
#' @examples
#' df <- dplyr::tibble(
#'   model = c("rf", "rf", "glmnet", "glmnet", "svmRadial", "svmRadial"),
#'   AUC = c(.2, 0.3, 0.8, 0.9, 0.85, 0.95)
#' )
#' set.seed(123)
#' permute_p_value(df, "AUC", "model", "rf", "glmnet", nperm = 100)
permute_p_value <- function(merged_data, metric, group_name, group_1, group_2, nperm = 10000) {
  # check that the metric and group exist in data
  if (!(metric %in% colnames(merged_data))) {
    stop(paste0("The metric `", metric, "` does not exist in the data."))
  }
  if (!(group_name %in% colnames(merged_data))) {
    stop(paste0("The group_name `", group_name, "` does not exist in the data."))
  }
  # check that group_1 and group_2 exist in the data
  if (!(group_1 %in% (merged_data %>% dplyr::pull(group_name)))) {
    stop(paste0("group_1 `", group_1, "` does not exist in the data."))
  }
  if (!(group_2 %in% (merged_data %>% dplyr::pull(group_name)))) {
    stop(paste0("group_2 `", group_2, "` does not exist in the data."))
  }

  # subset results to select metric and group columns and
  # filter to only the two groups of interest
  sub_data <- merged_data %>%
    dplyr::select({{ metric }}, {{ group_name }}) %>%
    dplyr::filter(.data[[group_name]] == {{ group_1 }} | .data[[group_name]] == {{ group_2 }})

  # observed difference: quantify the absolute value of the difference
  # in metric between the two groups
  metric_obs <- get_difference(sub_data, {{ group_name }}, {{ metric }})

  # shuffled difference: quantify the absolute value of the difference
  # in metric between the two groups after shuffling group labels
  rep_fn <- select_apply("replicate")
  metric_null <- rep_fn(
    nperm,
    get_difference(
      shuffle_group(sub_data, group_name),
      group_name,
      metric
    )
  )

  p_value <- calc_pvalue(metric_null, metric_obs)
  return(p_value)
}


#' Perform permutation tests to compare the performance metric
#' across all pairs of a group variable.
#'
#' A wrapper for `permute_p_value()`.
#'
#' @param merged_data the concatenated performance data from `run_ml`
#' @param metric metric to compare, must be numeric
#' @param group_name column with group variables to compare
#' @param nperm number of permutations, default=10000
#'
#' @return a table of p-values for all pairs of group variable
#' @export
#' @author Courtney R Armour, \email{armourc@@umich.edu}
#'
#' @examples
#' df <- dplyr::tibble(
#'   model = c("rf", "rf", "glmnet", "glmnet", "svmRadial", "svmRadial"),
#'   AUC = c(.2, 0.3, 0.8, 0.9, 0.85, 0.95)
#' )
#' set.seed(123)
#' compare_models(df, "AUC", "model", nperm = 10)
compare_models <- function(merged_data, metric, group_name, nperm = 10000) {
  x <- y <- group1 <- group2 <- NULL # suppresses R CMD check Note "no visible binding for global variable"
  # check that the metric and group exist in data
  if (!(metric %in% colnames(merged_data))) {
    stop("The metric does not exist in the data.")
  }
  if (!(group_name %in% colnames(merged_data))) {
    stop("The group_name does not exist in the data.")
  }

  # identify all unique groups in group variable
  groups <- merged_data %>%
    dplyr::pull({{ group_name }}) %>%
    unique()

  # create a table with all possible comparisons of groups
  # without repeating pairings
  p_table <- tidyr::expand_grid(
    x = 1:length(groups),
    y = 1:length(groups)
  ) %>%
    dplyr::filter(x < y) %>%
    dplyr::mutate(
      group1 = groups[x],
      group2 = groups[y]
    ) %>%
    dplyr::select(-x, -y) %>%
    dplyr::group_by(group1, group2) %>%
    dplyr::summarize(
      p_value = permute_p_value(merged_data, metric, group_name, group1, group2, nperm),
      .groups = "drop"
    )

  return(as.data.frame(p_table))
}
