#' Get Difference

# calculate the difference in the mean of the metric for the two groups
# sub_data: subset of the performance data for just two groups
# group_name: name of column with group variable
# metric: metric to compare
get_difference <- function(sub_data,group_name,metric){
  # get the mean metric value for each group
  means <- sub_data %>%
    dplyr::group_by(.data[[group_name]]) %>%
    dplyr::summarise(meanVal = mean(.data[[metric]]),.groups="drop") %>%
    dplyr::pull(meanVal)
  # find the difference in the mean between the two groups
  abs(diff(means))
}

#' Shuffle Group

# shuffle the groups across the data
# sub_data: subset of the performance data for just two groups
# group_name: column name to shuffle
shuffle_group <- function(sub_data,group_name){
  # get teh group labels
  group_vals <- sub_data %>% 
    dplyr::pull( {{ group_name }} )
  # shuffle the group labels
  group_vals_shuffled <- base::sample(group_vals)
  
  #assign shuffled groups to group column
  data_shuffled <- sub_data %>% 
    dplyr::mutate( !!group_name := group_vals_shuffled)
  
  return(data_shuffled)
}

#' Permute P-Value

# data: the concatenated performance (from what output?)
# metric: metric to compare, either AUC or cv_metric_AUC
# group_name: column with group variables to compare
# group_1: name of one group to compare
# group_2: name of other group to compare
permute_p_value <- function(data, metric, group_name, group_1, group_2, nperm){
  # check that the metric and group exist in data
  assertthat::has_name(data,metric)
  assertthat::has_name(data,group_name)
  # check that group_1 and group_2 exist in the data
  #assertthat::has_name(data %>% distinct({{group_name}}),{{ group_1 }})
  
  # subset results to select metric and group columns and
  # filter to only the two groups of interest
  sub_data <- data %>%
    dplyr::select({{ metric }},{{ group_name }}) %>%
    dplyr::filter( .data[[group_name]] == {{group_1}}  | .data[[group_name]] == {{group_2}})
  
  # observed difference: quantify the absolute value of the difference 
  # in metric between the two groups 
  metric_obs <- get_difference(sub_data,{{group_name}},{{metric}})
  
  # shuffled difference: quantify the absolute value of the difference 
  # in metric between the two groups after shuffling group labels
  metric_null <-  replicate(nperm,get_difference(shuffle_group(sub_data,group_name),group_name,metric))
  
  # n: number of shuffled calculations
  n <- length(metric_null)
  # r: replications at least as extreme as observed effect
  r <- sum(abs(metric_null) >= metric_obs)
  
  # compute Monte Carlo p-value with correction (Davison & Hinkley, 1997)
  p_value=(r+1)/(n+1)
  return(p_value)
}

# Wrapper to do all comparisons
# data: the concatenated performance (from what output?)
# metric: metric to compare, either AUC or cv_metric_AUC
# group_name: column with group variables to compare
compare_models <- function(merged_data,metric,group_name,nperm=10000){
  
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
    select(-x, -y) %>% 
    group_by(group1,group2) %>% 
    summarize(p_value = permute_p_value(merged_data,metric,group_name,group1,group2,nperm),
              .groups = "drop")
  
  return(p_table)

}

df <- tibble(samp=c("a","a","b","b"),val=c(.2,0.3,0.8,0.9))
get_difference(df,"samp","val")
shuffle_group(df,"samp")
permute_p_value(df,"val","samp","a","b",nperm=10)
compare_models(df,"val","samp",nperm=10)

#### TESTS ####

testthat::expect_equal(get_difference(tibble(AUC=c(0.5,0.8),
                                       type=c("a","b")),
                                      "type","AUC"),0.3)
