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
#   (1) data/process/LEVEL_input_data.csv - CSV with data for machine learning -
#			first column is outcome of interest, 
#			remaining columns are features, one per column.
#   (2) data/process/sig_flat_corr_matrix_LEVEL.csv - CSV with correlated features
######################################################################

'Setup Data for ML Pipeline Microbiome
Usage:
  setp_model_data.R --data=<csv> --outcome=<colname> --level=<level>
  setp_model_data.R --help
Options
  -h --help                  Display this help message.
  --data=<csv>               Raw Dataset filename in csv format.
  --outcome=<colname>        Outcome column name from the metadata file.
  --level=<level>     		 Name of experiment
' -> doc

################### IMPORT LIBRARIES and FUNCTIONS ###################
# The dependinces for this script are consolidated in the first part
deps = c("docopt", "tidyverse", "caret", "Hmisc")
for (dep in deps) {
  library(dep, character.only = TRUE)
}
args <- docopt(doc)
# Load in needed functions and libraries
source('code/R/compute_correlation_matrix.R')
######################################################################




######################## DATA PREPARATION #############################


# ----------------------- Read in data --------------------------------
# read in data with first column outcome and remaining columns are features
# if needing to edit your data remove this line and edit inputs to fit your data
data <- read_csv(args$data)

# ---------------------------------------------------------------------

if(length(unique(data[,1])) > 2){
	stop('Data not ready to be processed as is. There maybe columns present that are not outcome or features.
		Please input a file that has the first column outcome and remaining columns are features.
		Please edit this file to properly process your input file and remove unused samples/features/columns.')
}
# -------------------------------------------------------------------------
# Use space below to process you data, such as joining metadata to features
# subselecting samples/features, modifying features
#	
#	
#	# ----------------------- Read in data --------------------------------
#	meta_file <- ##### insert metadata file name #####
#	feature_file <- ##### insert features file name #####
#	
#	# Read in metadata
#	meta <- read_tsv(meta_file)  
#	
#	# Read in OTU table and remove label and numOtus columns
#	features <- read_tsv(feature_file)
#	# ---------------------------------------------------------------------
#	
#	
#	# ----------------- Select samples and features -----------------------
#	# Filter metadata and select only sample names and outcome columns
#	# Merge metadata and feature data.
#	# Then remove the sample name column
#	data <- meta %>% 
#		filter(###### insert edits here ########
#			) %>% 
#		select(###### select sample names and outcome column #####
#			) %>% 
#		inner_join(features, by=c("sample_names"))  %>% 
#		select(-sample_names) %>% 
#		drop_na()
#	# ---------------------------------------------------------------------
#	
# -------------------------------------------------------------------------


# ----------------------- Preprocess model data----------------------------

save_data <- function(data, level){
	# ---------------------- Process model data ---------------------------
	# Remove features with near zero variance and scale remaining from 0 to 1
	preProcValues <- preProcess(data, method = c("nzv", "range"))
	dataTransformed <- predict(preProcValues, data)
	# Save data to be used in machine learning pipeline
	write_csv(dataTransformed, paste0('data/process/', level, '_input_data.csv'))
	# ---------------------------------------------------------------------


	# ------------------- Create correlation matrix -----------------------
	# Create correlation matrix of machine learning data
	#   filters correlation >= cor_value and p values < p_value
	#   default values are cor_value = 1, and p_value = 0.1
	compute_correlation_matrix(input_file = paste0('data/process/', level, '_input_data.csv'), 
		outcome = args$outcome, level = level)
	# ---------------------------------------------------------------------
}

save_data(data, args$level)