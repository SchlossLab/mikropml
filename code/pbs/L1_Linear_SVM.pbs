
#!/bin/sh
#PBS -l procs=1,pmem=4gb
#PBS -l walltime=20:00:00
#PBS -j oe
#PBS -m a
#PBS -V
#PBS -M begumtop@umich.edu
#PBS -A pschloss_fluxod
#PBS -q fluxod
#PBS -l qos=flux
#PBS -t 1-100

# Load Modules:
#  1) R/3.5.0
#  2)  r-biomed-libs
#  3) run Rscript code/learning/load_caret_models_flux.R

# -t is array parameter, the same job will be submitted the length of the input,
# each with its own unique array id ($PBS_ARRAYID)


cat $PBS_NODEFILE
qstat -f $PBS_JOBID

cd $PBS_O_WORKDIR
# vector index starts at 0 so shift array by one
cd $PBS_O_WORKDIR
seed=$(($PBS_ARRAYID - 1))

# print out which model is being run in each job

echo Using "L1 Linear SVM"

# Using $PBS_ARRAYID to select parameter set
Rscript code/learning/load_caret_models_flux.R
#Rscript code/learning/main.R $seed "L1_Linear_SVM"
make data/temp/best_hp_results_L1_Linear_SVM_$seed.csv

echo "Script complete"
echo "qsub working directory absolute is"
echo $PBS_O_WORKDIR
qstat -f $PBS_JOBID
exit
