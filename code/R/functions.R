# Author: Begum Topcuoglu
# Date: 2018-12-06
#
######################################################################
# Place to store useful functions that will be used repeatedly throughout
######################################################################

deps = c("reshape2", "cowplot", "ggplot2","knitr","rmarkdown","vegan","gtools", "tidyverse", "ggsignif");
for (dep in deps){
  if (dep %in% installed.packages()[,"Package"] == FALSE){
    install.packages(as.character(dep), quiet=TRUE);
  }
  library(dep, verbose=FALSE, character.only=TRUE)
}

# -------------------- Make performance files tidy------------------>
# Instead of 2 columns with names cv_aucs and test_aucs
# We will have 1 column with name Performance that tells us if test or cv
melt_data <-  function(data) {
  data_melt <- data %>%
    melt(measure.vars=c('cv_aucs', 'test_aucs')) %>%
    rename(AUC=value) %>%
    mutate(Performance = case_when(variable == "cv_aucs" ~ 'Cross-validation', variable == "test_aucs" ~ 'Testing')) %>%
    group_by(Performance)
  return(data_melt)
}
# -------------------------------------------------------------------->


# -------------------- Read files ------------------------------------>
# Read in files as delim that are saved in a list with a pattern
read_files <- function(filenames){
  for(file in filenames){
    # Read the files generated in main.R
    # These files have cvAUCs and testAUCs for 100 data-splits
    data <- read.delim(file, header=T, sep=',')
  }
  return(data)
}
# -------------------------------------------------------------------->


# -------------------- Extract model name----------------------------->
# Get model name with sub from file name
get_model_name <- function(files){
  pat1 <- "data/process/walltime_"
  name_files <- sub(pat1, "", files)
  pat2 <- ".csv"
  names <- sub(pat2, "", name_files)
  return(names)
}
# -------------------------------------------------------------------->


# ------------------- Re-organize feature importance  ----------------->
# This function:
#     1. Takes in a dataframe (different data for each model) and the model name
#     2. If the models are linear, returns the median rank of the top ranked 5 features
#     3. If the models are not linear, returns the permutation importance results for:
#         - Correlated and non-correlated OTUs:
#         - Top 5 features or feature groups will be listed
#         - New AUROC which will differ from original AUROC after permutation
get_interp_info <- function(data, model_name){
  if("key" %in% colnames(data)){
    # If the models are linear, we used get_feature_rankings.R and then merge_feature_ranks.sh first
    # The created file after those 2 steps will be used in this function,
    # Data format is:
    #         The OTU names are in 1 column(repeated for 100 datasplits)
    #         The ranks based on absolute weights are in 1 column(for each of the datasplits)
 	#		  The weight value is on another column
  #     The sign of the weight is on another column
 	# We want to use/plot only the top 20 highest ranked OTUs
 	# Initial step is to get which are the highest 20 ranked OTUs by looking at their median rank
    # 1. We group by OTU name to make sure we are taking all the data-splits into account
    imp_first_20 <- data %>%
      # 2. Group by the OTU name and compute median rank for each OTU
      group_by(key) %>%
      summarise(median_rank = median(rank)) %>%
      # 3. Arrange from highest ranked 1, descending
      arrange(median_rank) %>%
      # 4. Grab only the highest ranked 20
      head(n=20) %>%
      select(key, median_rank)

    # Here we want to only grab the data (rank info from 100 datasplits) of only the top 20 median ranked OTUs
    # The imp data will be returned for Figure 3 where we plot each rank info for each data-split of the 20 top OTUs
    imp <- data %>%
      filter(key %in% imp_first_20$key) %>%
      group_by(key)



  }
  # If we want to calculate the permutation importance results for interpretation
  # Then we use the files without the weight information but with permutation results
  else{
    if("names" %in% colnames(data)){ # If the file has non-correlated OTUs
      non_correlated_data <- data %>%
        # 1. Group by the OTU names and calculate median and sd for auc change
        group_by(names) %>%
        summarise(imp = median(new_auc), sd_imp = sd(new_auc))
      	# Order the dataframe from smallest new_auc to largest.
      	# Because the smallest new_auc means that that OTU decreased AUC a lot when permuted
      imp <- non_correlated_data %>%
        arrange(imp)
    }
    else if("X1" %in% colnames(data)){
      # The file doesn't have "names" column which means these are correlated OTU groups
      # The file has correlated OTUs and their total percent auc change per group in one row
      # Each row has different groups of OTUs that are correlated together
      #     1. We will group by the first OTU (since it is only present in one group only)
      #         This will group all the datasplits for that OTU group together
      #     2. We then get the median percent auc change of that correlated OTU group
      correlated_data <- data %>%
        group_by(X1) %>%
        summarise(imp = median(new_auc), sd_imp = sd(new_auc))
      #     3. We will now only take the first 5 and add the other OTUs to the row.
      #       We have the new_auc for each correlated group of OTUs in a row
      #       We will also have all the OTU names in the group in the same row.
      imp <- correlated_data %>%
        arrange(imp) %>%
        head(5) %>%
        inner_join(data, by="X1") %>% # Add all the other OTUs in the group back to the data
        select(-new_auc, -model)
    }
      else{
        print("linear model")
        imp <- NULL
        }

  }
  return(imp)
}
# -------------------------------------------------------------------->


# Summarise walltime
summarise_walltime <- function(files){
  summarized_walltimes <- summarise(files, mean_walltime = mean(files[,1]), sd_AUC = sd(files[,1]))
  return(summarized_walltimes)
}

# Calculate unpaired two-samples Wilcoxon test to see if models differ from one another signigicantly
wilcoxon_test <- function(data, model_name_1, model_name_2){
  wilcox_result <- wilcox.test((data %>% filter(model==model_name_1))$test_aucs, (data %>% filter(model==model_name_2))$test_aucs)
  return(wilcox_result)
}


# subsampling test
perm_p_value <- function(data, model_name_1, model_name_2){
  test_subsample <- all %>%
    select(-cv_aucs) %>%
    filter(model == model_name_1 | model == model_name_2)
  install.packages("mosaic")
  library(mosaic)
  obs <- abs(diff(mosaic::mean(test_aucs ~ model, data = test_subsample)))
  auc.null <- do(10000) * diff(mosaic::mean(test_aucs ~ shuffle(model), data = test_subsample))
  n <- length(auc.null[,1])
  # r = #replications at least as extreme as observed effect
  r <- sum(abs(auc.null[,1]) >= obs)
  # compute Monte Carlo p-value with correction (Davison & Hinkley, 1997)
  p.value=(r+1)/(n+1)
 # plot <- ggplot(auc.null, aes(x=auc.null[,1], color=auc.null[,1]>=obs)) + geom_histogram(fill="white", alpha=0.5, position="identity") + scale_color_brewer(palette="Dark2")
  detach("package:mosaic", unload=TRUE)
  return(p.value)
}

# Find median and IQRs for each AUROC datasplit for each model
median_iqr <- function(data, model_name){
  median <- format(round(median((data %>% filter(model==model_name))$test_aucs), 3), nsmall=3)
  min_iqr <- format(round(rf_median - IQR((data %>% filter(model==model_name))$test_aucs), 3), nsmall=3)
  max_iqr <- format(round(rf_median + IQR((data %>% filter(model==model_name))$test_aucs), 3), nsmall=3)
  return(list(median, min_iqr, max_iqr))
}
