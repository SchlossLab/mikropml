#!/bin/bash

#SBATCH --job-name=mikRopML

#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=100MB
#SBATCH --time=96:00:00

#SBATCH --output=log/hpc/slurm-%j_%x.out

####  Slurm account and partition specification here
####    These will change if you work on multiple projects, or need
####    special hardware, like large memory nodes or GPUs.

#SBATCH --account=YOUR_ACCOUNT_HERE
#SBATCH --partition=standard

#### #### ####  These are the least frequently changing options

####  Your e-mail address and when you want e-mail

#SBATCH --mail-user=YOUR_EMAIL_HERE
#SBATCH --mail-type=BEGIN,END

time snakemake --profile config/slurm --latency-wait 90
