# ML Pipeline Microbiome

Contributers:
- Begum Topcuoglu
- [Kelly Sovacool](https://github.com/kelly-sovacool)
- Lucas Bishop
- Sarah Tomkovich
- [William L. Close](https://github.com/wclose)
- [Nick Lesniak](https://github.com/nlesniak)
- [Ariangela J. Kozik](https://github.com/aj-kozik)
- [Pat Schloss](https://github.com/pschloss)
- [Samara Rifkin](https://github.com/sbrifkin)
- Katie McBride
-

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
	|- README         		# the top level description of content (this doc)
	|- CONTRIBUTING    		# instructions for how to contribute to your project
	|- LICENSE         		# the license for this project
	|
	|- data/           		# raw and primary data, are not changed once created
	| |- process/     		# combined results as .tsv and .csv files
	| |- temp/     			# single seed files generated with main.R that runs the models
	| |- baxter.0.03.subsample.shared      	# subsampled mothur generated file
	| |- metadata.tsv     		        # metadata with clinical information
	|- code/          		# any programmatic code
	| |- R/    				# R code to build model
	| |- bash/     			# bash scripts to prepare repo
	| |- pbs/				# pbs scripts to run on HPC
  	|- test/          		# self-contained testing repo
  	| |- code/  			# any programmatic code to prepare test load_datasets
  	| |- data/				# generated test data to run the model on
	|- Makefile	 # Reproduce the pipeline


### How to regenerate this repository in R

1. Clone the Github Repository and change directory to the project directory.

```
git clone https://github.com/SchlossLab/ML_pipeline_microbiome.git
cd ML_pipeline_microbiome
```

2. Everything needs to be run from project directory.

3. Generate your own input data by looking at the `test/data/small_input_data.csv` example.
	- First column should be the outcome.
	- Rest of the columns will be the features. 

3. This ML pipeline is to predict a binary outcome.

4. Go to `test/README.md` to see how you can use this pipeline with a pre-prepared test dataset.

5. The scripts that are part of the pipeline:

	* To choose the model and model hyperparemeters:`code/R/tuning_grid.R`
	
	* To preprocess and split the dataset 80-20 and to train the model: `code/R/model_pipeline.R`

	* To interpret the models: `code/R/permutation_importance.R`

6. We want to run the pipeline 100 times with different seeds so that we can evaluate variability in modeling results. We can do this in many different ways.

	- Run the scripts one by one with different seeds:

	`Rscript code/R/main.R --seed 1 --permutation --model L2_Logistic_Regression --data test/data/small_input_data.csv --hyperparams test/data/hyperparams.csv --outcome dx`


	`Rscript code/R/main.R --seed 2 --permutation --model L2_Logistic_Regression --data test/data/small_input_data.csv --hyperparams test/data/hyperparams.csv --outcome dx`

						`...`

	`Rscript code/R/main.R --seed 100 --permutation --model L2_Logistic_Regression --data test/data/small_input_data.csv --hyperparams test/data/hyperparams.csv --outcome dx`



	- However, this is time-consuming and not DRY. We can run it paralellized for each datasplit (seed). We do this in our High Performing Computer (Great Lakes) by submitting an array job where the seed is automatically assigned [0-100] and each script is submitted at the same time - an example is present in the `code/slurm/L2_Logistic_Regression.sh` script. 

7. After we run the pipeline 100 times, we will have saved 100 files for AUROC values, 100 files for training times, 100 files for AUROC values for each tuned hyperparameter, 100 files for feature importances of perfectly correlated features, 100 files for feature importances of non-perfectly correlated features. These individual files will all be saved to `data/temp`. We can merge these files and save them to `data/process`. 

		`bash cat_csv_files.sh`

