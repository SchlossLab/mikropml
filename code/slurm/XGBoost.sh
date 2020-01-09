#!/bin/bash

###############################
#                             #
#  1) Job Submission Options  #
#                             #
###############################

# Name
#SBATCH --job-name=randomForest

# Resources
# For MPI, increase ntasks-per-node
# For multithreading, increase cpus-per-task
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=4GB
#SBATCH --time=400:00:00

# Account
#SBATCH --account=pschloss1
#SBATCH --partition=standard

# Logs
#SBATCH --mail-user=begumtop@umich.edu
#SBATCH --mail-type=FAIL

# Environment
##SBATCH --array=1-100
##SBATCH --export=ALL

# --array is array parameter, the same job will be submitted the length of the input,
# each with its own unique array id ($SLURM_ARRAY_TASK_ID)

# Connecting node to internet
source /etc/profile.d/http_proxy.sh 

# List compute nodes allocated to the job
if [[ $SLURM_JOB_NODELIST ]] ; then
	echo "Running on"
	scontrol show hostnames $SLURM_JOB_NODELIST
	echo -e "\n"
fi


# Load Modules:
#  1) R/3.5.0   2) r-biomed-libs/3.5.0




#####################
#                   #
#  2) Job Commands  #
#                   #
#####################

# Vector index starts at 0 so shift array by one
seed=$(($SLURM_ARRAY_TASK_ID - 1))

# Print out which model is being run in each job
echo Using "Random XGBoost"

# Using $SLURM_ARRAY_TASK_ID to select parameter set
Rscript code/R/main.R --seed $seed --model XGBoost --data  test/data/small_input_data.csv --hyperparams test/data/hyperparams.csv --outcome dx

# Printing completed job info
scontrol show job $SLURM_ARRAY_JOB_ID
