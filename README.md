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
- [Zena Lapp](https://github.com/zenalapp)

## Usage

### Dependencies

This pipeline depends on [R version >=3.5.3](https://www.r-project.org/) and the following R packages:


- MLmetrics
- "docopt"
- "dplyr"
- "tictoc"
- "caret"
- "rpart"
- "xgboost"
- "randomForest"
- "kernlab"
- "LiblineaR"
- "pROC"
- "tidyverse"
- "yaml"
- "data.table"
- "e1071"

You can install them with [`install.packages`](https://cran.r-project.org/doc/manuals/r-release/R-admin.html#Installing-packages) or your preferred package manager.

If you'd like to use [conda](https://conda.io/projects/conda/en/latest/), you can use the provided environment file:
```
conda env create -f config/environment.yml

conda activate ml
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
                                RBF_SVM
                                Decision_Tree
                                Random_Forest
                                XGBoost
  --data=<csv>               Metadata filename in csv format.
  --hyperparams=<csv>        Hyperparameters filename in csv format.
  --outcome=<colname>        Outcome column name from the metadata file.
  --permutation              Whether to perform permutation.
  --level                    The name of the modeling experiment (this will create a sperate folder)

```

### Example

```
Rscript code/R/main.R --seed 1 --model L2_Logistic_Regression --data test/data/small_input_data.csv --hyperparams test/data/hyperparams.csv --outcome dx --level otu
```

### Overview

	project
	|- README.md       	# the top level description of content (this doc)
	|- CONTRIBUTING.md	# instructions for how to contribute to your project
	|- LICENSE.md      	# the license for this project
  	|- ml-pipeline-microbiome.Rproj	# Rstudio project file  
  	|  
	|- code/          	# any programmatic code
	| |- R/    		# R code to build model
	| |- bash/     		# bash scripts to prepare repo
  	|  
	|- data/           	# raw and primary data, are not changed once created
	| |- caret_models	# code for running caret (should probably in code/)
	| |- process/     	# final combined results as .tsv and .csv files
	| +- temp/     		# array jobs will dump all the files here.
  	|  
  	|- test/          	# self-contained testing repo
  	| |- code/  		# any programmatic code to prepare test load_datasets
  	| |- data/		# generated test data to run the model on
  	|  
	|- config/		# conda configuration file


### How to regenerate this repository in R

Clone the Github Repository and change directory to the project directory.

```
git clone https://github.com/SchlossLab/ML_pipeline_microbiome.git
cd ML_pipeline_microbiome
```
### Quick-start Tutorial

This ML pipline is intended to predict a binary outcome.
NOTE: Everything needs to be run from the project directory.

To test the pipeline with a pre-prepared test dataset, go to `test/README.md`

1. Generate your own input data and match the formatting to the `test/data/small_input_data.csv` example.
Specifically:
	- First column should be the outcome of interest.
	- Remaining columns should be the features, one feature per column.

2. This pipeline consists of the following scripts:

	* Model and Hyperparameter Selection:`code/R/tuning_grid.R`: This function takes an optional argument to specify your own hyperparameters to be used for cross-validation (`data/default_hyperparameters.csv`). This argument should be the name of a .csv file. This file must contain three colums. The first column "param" should contain the name of the parameter, the second column should "val" contain the parameter values to be tested and the third column "model" should contain the model name. If `data/default_hyperparameters.csv` file is `NULL`, then default values will be used.

	* Preprocessing and splitting the dataset 80-20 to train the model: `code/R/model_pipeline.R`

	* Model Interpretation: `code/R/permutation_importance.R`. Using the `--permutation` flag turns on Permutation Importance calculation, which identifies the features (i.e. OTUs) most important in prediction by the model. In order for this option to work, `code/R/permutation_importance.R` requires a matrix containing the correlation of each feature to every other feature in the dataset. If your data is formatted as specified above, you can use the `code/R/generate_corr_matrix` script to generate your own correlation matrix for permutation importance like this:
      - This pipeline consists of the following scripts:		`Rscript code/R/generate_corr_matrix.R "path/to/inputfile" "outcome"`

      - This script currently takes two arguments:
	         - `"path/to/inputfile"` is the path to your formatted dataset, in quotes.
	          - `"outcome"` is the outcome state to be predicted by the model, in quotes.

      -  **NOTE**: in the current iteration of this pipeline, running`generate_corr_matrix.R` on your own dataset will overwrite the correlation matrix used in the test data, which will cause errors if you try to run the test model afterwards. The test correlation matrix can be restored using `git checkout data/process/sig_flat_corr_matrix.csv`. Running this command will in turn overwrite the correlation matrix generated from your own dataset.

3. We want to run the pipeline 100 times with different seeds so that we can evaluate variability in modeling results. We can do this in different ways.

	A) Run the scripts one by one with different seeds:

	`Rscript code/R/main.R --seed 1 --permutation --model L2_Logistic_Regression --data test/data/small_input_data.csv --hyperparams test/data/hyperparams.csv --outcome dx`

	`Rscript code/R/main.R --seed 2 --permutation --model L2_Logistic_Regression --data test/data/small_input_data.csv --hyperparams test/data/hyperparams.csv --outcome dx`


						`...`

	`Rscript code/R/main.R --seed 100 --permutation --model L2_Logistic_Regression --data test/data/small_input_data.csv --hyperparams test/data/hyperparams.csv --outcome dx`

  B) Run it parallelized for each datasplit (seed). We do this in our High Performing Computer (Great Lakes) by submitting an array job where the seed is automatically assigned [0-100] and each script is submitted at the same time - an example is present in the `code/slurm/L2_Logistic_Regression.sh` script.


4. After we run the pipeline 100 times, we will have saved 100 files for AUROC values, 100 files for training times, 100 files for AUROC values for each tuned hyperparameter, 100 files for feature importances of perfectly correlated features, 100 files for feature importances of non-perfectly correlated features. These individual files will all be saved to `data/temp`. We can then merge these files and save them to `data/process`.

		`bash cat_csv_files.sh`
