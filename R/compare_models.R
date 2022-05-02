
#' Average metric difference
#' calculate the difference in the mean of the metric for the two groups
#'
#' @param sub_data subset of the merged performance data for just two groups
#' @param group_name name of column with group variable
#' @param metric metric to compare
#'
#' @return numeric difference in the average metric between the two groups
#' 
#' @export
#' @author Courtney Armour, \email{armourc@@umich.edu}
#' 
#' @examples
#' \dontrun{
#' df <- tibble(condition=c("a","a","b","b","c","c"),
#'              AUC=c(.2,0.3,0.8,0.9,0.85,0.95))
#' sub_df <- df %>% filter(condition %in% c("a","b"))
#' get_difference(sub_df,"condition","AUC")
#' }
get_difference <- function(sub_data,group_name,metric){
  if(!is.numeric(sub_data %>% dplyr::pull(metric))){
    stop("The specified metric is not numeric, please check that you specified the right column.")
  }
  # get the mean metric value for each group
  means <- sub_data %>%
    dplyr::group_by(.data[[group_name]]) %>%
    dplyr::summarise(meanVal = mean(.data[[metric]]),.groups="drop") %>%
    dplyr::pull(meanVal)
  # find the difference in the mean between the two groups
  abs(diff(means))
}

#' Shuffle the values in the groups column
#'
#' @param sub_data subset of the performance data for just two groups
#' @param group_name column name to shuffle
#'
#' @return `sub_data` with the `group_name` column values shuffled
#' @export
#' @author Courtney R Armour, \email{armourc@@umich.edu}
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' df <- tibble(condition=c("a","a","b","b","c","c"),
#'              AUC=c(.2,0.3,0.8,0.9,0.85,0.95))
#' sub_df <- df %>% filter(condition %in% c("a","b"))
#' shuffle_group(sub_df,"condition")
#' }
shuffle_group <- function(sub_data,group_name){
  if(!(group_name %in% colnames(sub_data))){
    stop("The group_name does not exist in the data.")
  }
  # get the group labels
  group_vals <- sub_data %>% 
    dplyr::pull( {{ group_name }} )
  # shuffle the group labels
  group_vals_shuffled <- base::sample(group_vals)
  
  #assign shuffled groups to group column
  data_shuffled <- sub_data %>% 
    dplyr::mutate( !!group_name := group_vals_shuffled)
  
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
#' \dontrun{
#' df <- tibble(model=c("rf","rf","glmnet","glmnet","svmRadial","svmRadial"),
#'              AUC=c(.2,0.3,0.8,0.9,0.85,0.95))
#' set.seed(123)
#' permute_p_value(df,"AUC","model","rf","glmnet",nperm=100)
#' }
permute_p_value <- function(merged_data, metric, group_name, group_1, group_2, nperm=10000){
  # check that the metric and group exist in data
  if(!(metric %in% colnames(merged_data))){
    stop("The metric does not exist in the data.")
  }
  if(!(group_name %in% colnames(merged_data))){
    stop("The group_name does not exist in the data.")
  }
  # check that group_1 and group_2 exist in the data
  if(!(group_1 %in% (merged_data %>% dplyr::pull(group_name)))){
    stop("group_1 does not exist in the data.")
  }
  if(!(group_2 %in% (merged_data %>% dplyr::pull(group_name)))){
    stop("group_2 does not exist in the data.")
  }

  # subset results to select metric and group columns and
  # filter to only the two groups of interest
  sub_data <- merged_data %>%
    dplyr::select({{ metric }},{{ group_name }}) %>%
    dplyr::filter( .data[[group_name]] == {{group_1}}  | .data[[group_name]] == {{group_2}})
  
  # observed difference: quantify the absolute value of the difference 
  # in metric between the two groups 
  metric_obs <- get_difference(sub_data,{{group_name}},{{metric}})
  
  # shuffled difference: quantify the absolute value of the difference 
  # in metric between the two groups after shuffling group labels
  rep_fn <- select_apply("replicate")
  metric_null <-  rep_fn(nperm,get_difference(shuffle_group(sub_data,group_name),group_name,metric))
  
  # n: number of shuffled calculations
  n <- length(metric_null)
  # r: replications at least as extreme as observed effect
  r <- sum(abs(metric_null) >= metric_obs)
  
  # compute Monte Carlo p-value with correction (Davison & Hinkley, 1997)
  p_value=(r+1)/(n+1)
  return(p_value)
}


#' Compute all pairs of comparisons
#' calculate permuted p-value across all pairs of group variable. 
#' wrapper for `permute_p_value`
#'
#' @param merged_data the concatenated performance data from `run_ml`
#' @param metric metric to compare, must be numeric
#' @param group_name column with group variables to compare
#' @param nperm number of permutations, default=10000
#'
#' @return a table of p-values for all pairs of group varible
#' @export
#' @author Courtney R Armour, \email{armourc@@umich.edu}
#'
#' @examples
#' \dontrun{
#' df <- tibble(model=c("rf","rf","glmnet","glmnet","svmRadial","svmRadial"),
#'              AUC=c(.2,0.3,0.8,0.9,0.85,0.95))
#' set.seed(123)
#' compare_models(df,"AUC","model",nperm=100)
#' }
compare_models <- function(merged_data,metric,group_name,nperm=10000){
  # check that the metric and group exist in data
  if(!(metric %in% colnames(merged_data))){
    stop("The metric does not exist in the data.")
  }
  if(!(group_name %in% colnames(merged_data))){
    stop("The group_name does not exist in the data.")
  }
  
  # identify all unique groups in group variable
  groups <- merged_data %>% 
    dplyr::pull( {{group_name}} ) %>% 
    unique()
  
  # create a table with all possible comparisons of groups
  # without repeating pairings
  p_table <- tidyr::expand_grid(x=1:length(groups),
                                y=1:length(groups)) %>% 
    dplyr::filter(x < y) %>% 
    dplyr::mutate(group1 = groups[x],
                  group2 = groups[y])  %>% 
    dplyr::select(-x, -y) %>% 
    dplyr::group_by(group1,group2) %>% 
    dplyr::summarize(p_value = permute_p_value(merged_data,metric,group_name,group1,group2,nperm),
              .groups = "drop")
  
  return(as.data.frame(p_table))

}

#### TESTS ####
# df <- tibble(samp=c("a","a","b","b"),val=c(.2,0.3,0.8,0.9))
# get_difference(df,"samp","val")
# shuffle_group(df,"samp")
# permute_p_value(df,"val","samp","a","b",nperm=10)
# compare_models(df,"val","samp",nperm=10)

