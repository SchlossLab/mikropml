######################################################################
# Date: 05-04-2020
# Author: Nick Lesniak
# Title: Setup data to be run in machine learning pipeline
######################################################################

######################################################################
# Description:

# This script will read in data 
#     - Feature data (shared file w/OTUs)
#     - Metadata 


# It will run the following:
#     - code/R/compute_correlation_matrix.R
######################################################################

######################################################################
# Dependencies and Outputs:

# Be in the project directory.

# The outputs are:
#   (1) data/process/input_data.csv - CSV with data for machine learning -
#			first column is outcome of interest, 
#			remaining columns are features, one per column.
#   (2) data/process/sig_flat_corr_matrix.csv - CSV with correlated features
######################################################################

meta_file <- ##### insert metadata file name #####
feature_file <- ##### insert features file name #####

################### IMPORT LIBRARIES and FUNCTIONS ###################
# The dependinces for this script are consolidated in the first part
deps = c("tidyverse", "caret", "Hmisc");
for (dep in deps){
	if (dep %in% installed.packages()[,"Package"] == FALSE){
		install.packages(as.character(dep), quiet=TRUE, repos = "http://cran.us.r-project.org", dependencies=TRUE);
	}
	library(dep, verbose=FALSE, character.only=TRUE)
}
# Load in needed functions and libraries
source('code/R/compute_correlation_matrix.R')
######################################################################



######################## DATA PREPARATION #############################


# ----------------------- Read in data --------------------------------
# Read in metadata
meta <- read_tsv(meta_file)  

# Read in OTU table and remove label and numOtus columns
features <- read_tsv(feature_file)
# ---------------------------------------------------------------------


# ----------------- Select samples and features -----------------------
# Filter metadata and select only sample names and outcome columns
# Merge metadata and feature data.
# Then remove the sample name column
data <- meta %>% 
	filter(###### insert edits here ########
		) %>% 
	select(###### select sample names and outcome column #####
		) %>% 
	inner_join(features, by=c("sample_names"))  %>% 
	select(-sample_names) %>% 
	drop_na()
# ---------------------------------------------------------------------


# ---------------------- Process model data ---------------------------
# Remove features with near zero variance and scale remaining from 0 to 1
preProcValues <- preProcess(data, method = c("nzv", "range"))
dataTransformed <- predict(preProcValues, data)
# Save data to be used in machine learning pipeline
write_csv(dataTransformed, 'data/process/input_data.csv')
# ---------------------------------------------------------------------


# ------------------- Create correlation matrix -----------------------
# Create correlation matrix of machine learning data
#   filters correlation >= cor_value and p values < p_value
#   default values are cor_value = 1, and p_value = 0.1
compute_correlation_matrix('data/process/input_data.csv', 
	##### insert outcome column name#####, cor_value, p_value)
# ---------------------------------------------------------------------