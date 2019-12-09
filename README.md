### Overview

	project
	|- README         		# the top level description of content (this doc)
	|- CONTRIBUTING    		# instructions for how to contribute to your project
	|- LICENSE         		# the license for this project
	|
	|- data/           		# raw and primary data, are not changed once created
	| |- process/     		# .tsv and .csv files generated with main.R that runs the models
	| |- baxter.0.03.subsample.shared      	# subsampled mothur generated file with OTUs from Marc Sze's analysis
	| |- metadata.tsv     		        # metadata with clinical information from Marc Sze's analysis 		
	|- code/          			# any programmatic code
	| |- learning/    			# generalization performance of model
	| |- testing/     			# building final model
	|
	|- results/        			# all output from workflows and analyses
	| |- tables/      			# tables and .Rmd code of the tables to be rendered with kable in R
	| |- figures/     			# graphs, likely designated for manuscript figures
	|
	|- submission/
	| |- manuscript.Rmd 			# executable Rmarkdown for this study, if applicable
	| |- manuscript.md 			# Markdown (GitHub) version of the *.Rmd file 
	| |- manuscript.tex 			# TeX version of *.Rmd file 
	| |- manuscript.pdf 			# PDF version of *.Rmd file 
	| |- header.tex 			# LaTeX header file to format pdf version of manuscript 
	| |- references.bib 			# BibTeX formatted references 
	|
	|- Makefile	 # Reproduce the manuscript, figures and tables



### How to regenerate this repository in R

Before you start, please take a look at the `Makefile` for more information about the workflow. Please also read the `submission/manuscript.pdf` to get a more detailed look on what we achieve with this ML pipeline.

1. Clone the Github Repository and change directory to the project directory. 

```
git clone https://github.com/SchlossLab/Topcuoglu_ML_XXX_2019.git
cd DeepLearning
```

2. Our dependencies:

	* R version 3.5.0 
	
	* The R packages which needs to be installed in our environment: `caret` ,`rpart`, `xgboost`, `randomForest`, `kernlab`,`LiblineaR`, `pROC`, `tidyverse`, `cowplot`, `ggplot2`, `vegan`,`gtools`, `reshape2`. 
	
	* Everything needs to be run from project directory.
	
	* We need to download 2 datasets (OTU abundances and colonoscopy diagnosis of 490 patients) from *Sze MA, Schloss PD. 2018. Leveraging existing 16S rRNA gene surveys to identify reproducible biomarkers in individuals with colorectal tumors. mBio 9:e00630–18. doi:10.1128/mBio.00630-18* by running:
	
		```bash code/learning/load_datasets.batch``` 
	
	* We update the `caret` package with my modifications by running (Take a look at this script to change the R packages directory where `caret` is installed.):
	
		```Rscript code/learning/load_caret_models.R``` 
		
	These modifications are in `data/caret_models/svmLinear3.R` and `data/caret_models/svm_Linear4.R`
	
3. This ML pipeline is to predict a binary outcome. It is also hard-coded for predicting cancer vs healthy individuals. This feature will be updated to incorporate user-defined outcomes in the future. (Issue #6)
	
4. Examples of how to run ML pipeline:

	1. Run the ML pipeline once (using seed=1) using L2-regularized logistic regression: (Using a different seed will result in the dataset to be split to 80 training set - 20 testing set differently. Different seeds will give slightly different results.)
	
		```
		Rscript code/learning/main.R 1 "L2_Logistic_Regression"
		```
	
	The `main.R` function accepts 7 different models that needs to call models as:
	
	    	* "L2_Logistic_Regression"
	     	* "L1_Linear_SVM"
	     	* "RBF_SVM"
	     	* "Decision_Tree"
	     	* "Random_Forest" 
	     	* "XGBoost" 
		
	So if you want to use a random forest model you'll run:
	
		
	`Rscript code/learning/main.R 1 "Random_Forest"`
	
	`code/learning/main.R` is an R script that (i) prepares the data to plug into the ML pipeline, (ii) uses the 1st argument to set a seed,(iii) uses the 2nd argument to start running the pipeline with the model type (`get_results` function is called for this) and (iv) keep track of walltime.
	     
	 2. `Rscript code/learning/main.R` sources 4 other scripts that are part of the pipeline. 
	 
	 	* To choose the model and model hyperparemeters:`source('code/learning/model_selection.R')`
		* This function (and `get_results`) also takes an optional argument to specify your own hyperparameters to be used for cross-validation (`hyperparameters`). This argument should be a list where the names of the list are the hyperparameters and the values are the values to be tested
		
		Depending on your ML task, the model hyperparameter range to tune will be different. This is hard-coded for our study but will be updated to integrate user-defined range in the future (Issue # 10)
	 
	 	* To preprocess and split the dataset 80-20 and to train the model: `source('code/learning/model_pipeline.R')`
	 
	 	* To save the results of each model for each datasplit: `source('code/learning/generateAUCs.R')`
	 
	 	* To interpret the models: `source('code/learning/permutation_importance.R')`
	
	 3. We want to run the pipeline 100 times with different seeds so that we can evaluate variability in modeling results. We can do this in many different ways. 
	
		1. Run the scripts one by one with different seeds:
	
			```Rscript code/learning/main.R 1 "L2_Logistic_Regression"```
	
			```Rscript code/learning/main.R 2 "L2_Logistic_Regression"```
	
			```Rscript code/learning/main.R 3 "L2_Logistic_Regression"```
			
						`...`
						
			```Rscript code/learning/main.R 100 "L2_Logistic_Regression"```
	
				However, this is time-consuming and not DRY.
	
		2. We can run it paralellized for each datasplit (seed). We do this in our HPC by submitting an array job where the seed is automatically assigned [0-100] and each script is submitted at the same time - an example is present in the `L2_Logistic_Regression.pbs` script. You can also follow how this is done in our `Makefile`.
	
	4. After we run the pipeline 100 times, we will have saved 100 files for AUROC values, 100 files for training times, 100 files for AUROC values for each tuned hyperparameter, 100 files for feature importances of perfectly correlated features, 100 files for feature importances of non-perfectly correlated features. These files will all be saved to `data/temp`. We need to merge these files.
	
		`bash cat_csv_files.sh`
			
	
		This script will save combined files to `data/process`. 
	
	

### How to regenerate this repository in python (in progress)


#### To run L2 Logistic Regression, L1 and L2 Linear SVM, RBF SVM, Decision Tree, Random Forest and XGBoost in Python
1. Generate tab-delimited files: Cross-validation and testing AUC scores of each model.
2. Generate tab-delimited files: The AUC scores of each hyper-parameter tested for each model.
3. Generate a comma-seperated file: The hyper-parameters tuned for each model in one file.
4. Generate ROC curve figures: The cross-validation and testing ROC curves for each model. 

```
python code/learning/main.py
```


