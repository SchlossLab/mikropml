#!/bin/bash

# Author: Begum Topcuoglu edited by Pat Schloss and Sadie Gugel
# Date: 2018-04-24
#
#######################################################################################
#
# User will supply...
#		1. the model name to use (e.g. "L1_Linear_SVM" "L2_Logistic_Regression" etc)
#		2. whether to use all_hp_results or best_hp_results etc
#
# This script will:
#   1. Take 1 .csv files that have the model result per datasplit, but do it for 100 splits
#   2. Combine them together to have the results for 100 datasplits in one .csv file
#   3. We don't keep the header of each file when combined but only once.
#
# In the end, the combined_best file must be 101 lines. 1st line is the header and the 100 lines
#		have the data of 100 files. the combined_all file must have 100*(hyper-parameter number)+1
#		lines.
#
########################################################################################

# Define the model and data type to use
MODEL=$1	
#   1.  L1_Linear_SVM
#   2.  L2_Linear_SVM
#   3.  L2_Logistic_Regression
#   4.  RBF_SVM
#   5.  Decision_Tree
#   6.  Random_Forest
#   7.  XGBoost
DATA=$2 	
#   1. all_hp_results 
#   2. all_imp_features_cor_results
#   3. all_imp_features_non_cor_results
#   4. best_hp_results
#   5. walltime

# Define the directories we will use in the script
SEARCH_DIR=data/temp
FINAL_DIR=data/process

# 1. Keep the first line of File0 and remove the first line of all the other files (File[0-99]) and
#		output it to the FINAL_DIR location
cp $SEARCH_DIR/"$DATA"_"$MODEL"_0.csv $FINAL_DIR/combined_"$DATA"_"$MODEL".csv

#	2. Append the other files to the end, but we want to be sure to ignore the 0 file since we don't
#		want it printed twice
#        "tail -n +2" makes tail print lines from 2nd line to the end
#        "-q" tells it to not print the header with the file name
#        ">>" adds all the tail stuff from every file to the combined file
tail -n +2 -q $SEARCH_DIR/"$DATA"_"$MODEL"_{1..99}.csv >> $FINAL_DIR/combined_"$DATA"_"$MODEL".csv
