# ML Pipeline Microbiome

Contributors:
- [Begum Topcuoglu](https://github.com/BTopcuoglu)
- [Kelly Sovacool](https://github.com/kelly-sovacool)
- [Lucas Bishop](https://github.com/lucas-bishop)
- [Sarah Tomkovich](https://github.com/tomkoset)
- [William L. Close](https://github.com/wclose)
- [Nick Lesniak](https://github.com/nlesniak)
- [Ariangela J. Kozik](https://github.com/aj-kozik)
- [Pat Schloss](https://github.com/pschloss)
- [Samara Rifkin](https://github.com/sbrifkin)
- [Ande Garretto](https://github.com/agarretto96)
- [Katie McBride](https://github.com/ktmcb)
- [Joshua MA Stough](https://github.com/jmastough)


## Usage

### Dependencies

This pipeline depends on [R version >=3.5.3](https://www.r-project.org/) and the following R packages:

- docopt
- tictoc
- caret
- rpart
- xgboost
- randomForest
- kernlab
- LiblineaR
- pROC
- tidyverse
- yaml

You can install them with [`install.packages`](https://cran.r-project.org/doc/manuals/r-release/R-admin.html#Installing-packages) or your preferred package manager.

If you'd like to use [conda](https://conda.io/projects/conda/en/latest/), you can use the provided environment file:
```
conda env create -f config/environment.yml
```

See the [conda documentation](https://conda.io/projects/conda/en/latest/user-guide/getting-started.html#managing-environments) for more on managing & using conda environments.

### Command Line Interface

```
ML Pipeline Microbiome

Usage:
  main.R --seed=<num> --model=<name> --metadata=<csv> --hyperparams=<csv> --outcome=<colname> [--permutation]
  main.R --help

- Options
  -h --help                  Display this help message.
  --seed=<num>               Random seed.
  --model=<name>             Model name. options:
                                L2_Logistic_Regression
                                L1_Linear_SVM
                                L2_Linear_SVM
                                RBF_SVM Decision_Tree
                                Random_Forest
                                XGBoost
  --data=<csv>               Metadata filename in csv format.
  --hyperparams=<csv>        Hyperparameters filename in csv format.
  --outcome=<colname>        Outcome column name from the metadata file.
  --permutation              Whether to perform permutation.

```

### Example

```
Rscript code/R/main.R --seed 1 --permutation --model L2_Logistic_Regression --data test/data/small_input_data.csv --hyperparams test/data/hyperparams.csv --outcome dx
```

### Overview

	project
	|- README.md       	# the top level description of content (this doc)
	|- CONTRIBUTING.md	# instructions for how to contribute to your project
	|- LICENSE.md      	# the license for this project
  |- ml-pipeline-microbiome.Rproj	# Rstudio project file  
  |  
	|- code/          	# any programmatic code
	| |- R/    					# R code to build model
	| +- bash/     			# bash scripts to prepare repo
  |  
	|- data/           	# raw and primary data, are not changed once created
	| |- caret_models		# code for running caret (should probably in code/)
	| |- process/     	# final combined results as .tsv and .csv files
	| +- temp/     			# array jobs will dump all the files here.
  |  
  |- test/          	# self-contained testing repo
  | |- code/  				# any programmatic code to prepare test load_datasets
  | |- data/					# generated test data to run the model on
  | +- config.yml			# config file for running tests  
  |  
	+- config/					# conda configuration file


### How to regenerate this repository in R

Clone the Github Repository and change directory to the project directory.

```
git clone https://github.com/SchlossLab/ML_pipeline_microbiome.git
cd ML_pipeline_microbiome
```
### Quick start tutorial

This ML pipline is intended to predict a binary outcome.
NOTE: Everything needs to be run from the project directory.

To test the pipeline with a pre-prepared test dataset, go to `test/README.md`

1. Generate your own input data and match the formatting to the `test/data/small_input_data.csv` example.
Specifically:
	- First column should be the outcome of interest.
	- Remaining columns should be the features, one feature per column.

2. This pipeline consists of the following scripts:

	* To choose the model and model hyperparemeters:`code/R/tuning_grid.R`: This function takes an optional argument to specify your own hyperparameters to be used for cross-validation (`hyperparameters`). This argument should be a csv filename where the names of the list are the first column "param" is the name of the parameter, the second column "val" is the parameter values to be tested and the third column "model" is the model name. If `hyperparameters.csv` file is `NULL`, then default values will be used. 

	* To preprocess and split the dataset 80-20 and to train the model: `code/R/model_pipeline.R`

	* To interpret the models: `code/R/permutation_importance.R`

3. We want to run the pipeline 100 times with different seeds so that we can evaluate variability in modeling results. We can do this in many different ways.

	A) Run the scripts one by one with different seeds:

	`Rscript code/R/main.R --seed 1 --permutation --model L2_Logistic_Regression --data test/data/small_input_data.csv --hyperparams test/data/hyperparams.csv --outcome dx`

	`Rscript code/R/main.R --seed 2 --permutation --model L2_Logistic_Regression --data test/data/small_input_data.csv --hyperparams test/data/hyperparams.csv --outcome dx`


						`...`

	`Rscript code/R/main.R --seed 100 --permutation --model L2_Logistic_Regression --data test/data/small_input_data.csv --hyperparams test/data/hyperparams.csv --outcome dx`

  B) We can run it paralellized for each datasplit (seed). We do this in our High Performing Computer (Great Lakes) by submitting an array job where the seed is automatically assigned [0-100] and each script is submitted at the same time - an example is present in the `code/slurm/L2_Logistic_Regression.sh` script. 


7. After we run the pipeline 100 times, we will have saved 100 files for AUROC values, 100 files for training times, 100 files for AUROC values for each tuned hyperparameter, 100 files for feature importances of perfectly correlated features, 100 files for feature importances of non-perfectly correlated features. These individual files will all be saved to `data/temp`. We can merge these files and save them to `data/process`.

		`bash cat_csv_files.sh`
