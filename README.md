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

### Command Line Interface

```
ML Pipeline Microbiome

Usage:
  main.R --seed=<num> --model=<name> --metadata=<csv> --hyperparams=<csv> --outcome=<colname> [--permutation]
  main.R --help

Options
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

2. Our dependencies:

	* R version 3.5.0

	* The R packages which needs to be installed in our environment: `caret` ,`rpart`, `xgboost`, `randomForest`, `kernlab`,`LiblineaR`, `pROC`, `tidyverse`, `cowplot`, `ggplot2`, `vegan`,`gtools`, `reshape2`.

	* Everything needs to be run from project directory.

	* Generate your own input data following the instructions on ... (KM - on what?)

3. This ML pipeline is to predict a binary outcome.

4. Go to `test/README.md` to see how you can use this pipeline with a pre-prepared test dataset.

5. Examples of how to run ML pipeline: (KM - Maybe split up these as seperate sections? i.e. Section 1 - Prep, Sec 2 - Running the Pipeline)

	1. Run the ML pipeline once (using seed=1) using L2-regularized logistic regression: (Using a different seed will result in the dataset to be split to 80 training set - 20 testing set differently. Different seeds will give slightly different results.)

		```
		Rscript code/R/main.R test/data/test_data.csv 1 "L2_Logistic_Regression" 0 "dx"
		```

	The `main.R` function accepts 7 different models that needs to call models as:

	    	* "L2_Logistic_Regression"
	     	* "L1_Linear_SVM"
	     	* "RBF_SVM"
	     	* "Decision_Tree"
	     	* "Random_Forest"
	     	* "XGBoost"

	For example- if you want to use a random forest model you'll run:


	`Rscript code/R/main.R test/data/test_data.csv 1 "Random_Forest" 0 "dx"` (KM - maybe explain what the 0 and "dx" are for as well)

	 2. `Rscript code/R/main.R` sources 4 other scripts that are part of the pipeline.

	 	* To choose the model and model hyperparemeters:`source('code/R/tuning_grid.R')`
		* This function (and `get_results`) also takes an optional argument to specify your own hyperparameters to be used for cross-validation (`hyperparameters`). This argument should be a list where the names of the list are the hyperparameters and the values are the values to be tested.

		Depending on your ML task, the model hyperparameter range to tune (KM - I'm not sure what tune means) will be different. This is hard-coded for our test study but will be updated to integrate user-defined range in the future (Issue # 10) (KM - will there be a limit to number of hyperparameters?)

	 	* To preprocess and split the dataset 80-20 and to train the model: `source('code/R/model_pipeline.R')`

	 	* To save the results of each model for each datasplit: `source('code/lR/generateAUCs.R')`

	 	* To interpret the models: `source('code/R/permutation_importance.R')` (KM - are there common signs to look for to know if it was completed successfully?)

	 3. We want to run the pipeline 100 times with different seeds so that we can evaluate variability in modeling results. We can do this in many different ways.


		- Run the scripts one by one with different seeds:

	`Rscript code/R/main.R test/data/test_data.csv 1 "Random_Forest" 0 "dx"`

	`Rscript code/R/main.R test/data/test_data.csv 2 "Random_Forest" 0 "dx"`

	`Rscript code/R/main.R test/data/test_data.csv 3 "Random_Forest" 0 "dx"`

						`...`

	`Rscript code/R/main.R test/data/test_data.csv 100 "Random_Forest" 0 "dx"`


		- However, this is time-consuming and not DRY. We can run it paralellized for each datasplit (seed). We do this in our HPC (KM - I'm not sure what HPC stands for) by submitting an array job where the seed is automatically assigned [0-100] and each script is submitted at the same time - an example is present in the `code/pbs/L2_Logistic_Regression.pbs` script. You can also follow how this is done in our `Makefile`.

	4. After we run the pipeline 100 times, we will have saved 100 files for AUROC values, 100 files for training times, 100 files for AUROC values for each tuned hyperparameter, 100 files for feature importances of perfectly correlated features, 100 files for feature importances of non-perfectly correlated features. These files will all be saved to `data/temp`. We need to merge these files. (KM - where will the individual files be saved?)

		`bash cat_csv_files.sh`


		This script will save combined files to `data/process`.
