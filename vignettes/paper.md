---
title: "mikRopML: A User-Friendly Machine Learning R Package for Binary Classification Problems"
output: 
  rmarkdown::html_vignette:
    keep_md: true
tags:
  - R
  - machine learning
  - logistic regression
  - decision tree
  - random forest
  - xgBoost
  - microbiology
authors:
  - name: Begüm D. Topçuoğlu
    orcid: 0000-0003-3140-537X
    affiliation: 1;4
  - name: Zena Lapp
    orcid: 0000-0003-4674-2176
    affiliation: 1
  - name: Kelly L. Sovacool
    orcid: 0000-0003-3283-829X
    affiliation: 1
  - name: Evan Snitkin
    orcid: 0000-0001-8409-278X
    affiliation: 3;5
  - name: Jenna Wiens
    orcid: 0000-0002-1057-7722
    affiliation: 2
  - name: Patrick D. Schloss
    orcid: 0000-0002-6935-4275
    affiliation: 3
affiliations:
  - name: Department of Computational Medicine & Bioinformatics, University of Michigan
    index: 1
  - name: Department of Electrical Engineering & Computer Science, University of Michigan
    index: 2
  - name: Department of Microbiology & Immunology, University of Michigan
    index: 3
  - name: Exploratory Science Center, Merck & Co., Inc., Cambridge, Massachusetts, USA.
    index: 4
  - name: Department of Internal Medicine/Division of Infectious Diseases, University of Michigan
    index: 5
date: 2020
bibliography: paper.bib
vignette: >
  %\VignetteIndexEntry{mikRopML paper}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---




# Summary

Machine learning (ML) for classification of data into groups based on a set of features is being used to make decisions in healthcare, economics, criminal justice and more. 
However, implementing a robust ML classification pipeline can be time-consuming, confusing, and difficult. 
Here, we present mikRopML (prononced "meek-ROPE em el"), an easy-to-use R package that generates robust ML pipelines for binary classification problems using logistic regression, a decision tree, random forest, xgBoost, or support vector machines.
It is available on [GitHub](https://github.com/SchlossLab/mikRopML/) and CRAN [**link to CRAN**]. 

# Statement of need

Supervised machine learning (ML) is widely used to recognize patterns in large datasets and make predictions to categorize the data. 
Several packages including `caret` [@kuhn_building_2008] and `tidymodels` [@kuhn_tidymodels_2020] in R and `scikitlearn` [@pedregosa_scikit-learn_2011] in Python allow you to implement your own ML algorithms; however, creating a robust machine learning pipeline can be overwhelming, particularly for beginners. 

ML requires data pre-processing, cross-validation, testing, model evaluation, and often interpretation of why the model makes a certain prediction. 
Performing these steps using the correct methodology is extremely important as failure to implement a robust ML pipeline can result in incorrect and misleading results [@teschendorff_avoiding_2019; @wiens_no_2019]. 

To allow a broader range of researchers to perform robust ML analyses, we created [mikRopML](https://github.com/SchlossLab/mikRopML/), an easy-to-use package in R [@r_core_team_r_2020] that implements the Topçuoğlu _et al._ [@topcuoglu_framework_2020] ML framework for binary classification problems. `mikRopML` acts as a wrapper around the R `caret` package to use five different ML algorithms; logistic regression [@paul_liblinear_2017], a decision tree, random forest [@liaw_classication_2002], xgBoost [@chen_xgboost_2020], and support vector machine with a radial basis kernel [@karatzoglou_kernlab_2004].  It incorporates best practices in the training, testing and model evaluation steps of ML pipelines.  Furthermore, it provides a data pre-processing function based on the FIDDLE framework outlined in Tang et al. [@tang_fiddle_2020] and permutation importance function to measure the importance of each feature in the model [@breiman_random_2001; @fisher2018models].

The framework implemented in `mikRopML` is generalizable to perform ML on many different data types as it has already been applied to microbiome data  [@topcuoglu_framework_2020] to categorize patients with cancer, to identify differences in genomic and clinical features associated with bacterial infections [@lapp_machine_2020] and to predict gender-based biases in academic publishing [**cite Ada’s paper**]. 


# mikRopML package

The mikRopML package has functions to preprocess the data, run ML and quantify feature importance. We also provide vignettes and a snakemake workflow [@koster_snakemakescalable_2012] to showcase how to run an ideal ML pipeline with multiple different train/test data splits.
The output of these runs can be plotted using functions in the package.

## Preprocessing data

We provide a function (`preprocess_data`) that preprocesses features using several different functions from the `caret` package. 
The `preprocess_data` function takes continuous and categorical data, re-factors categorical data into binary features, and provides options to normalize continuous data, remove features with near-zero variance, and remove correlated features. 
We chose the default options based on best practices implemented in FIDDLE [@tang_fiddle_2020], and we direct users to this paper for more information on data preprocessing prior to ML. 
More details on how to use the mikRopML `preprocess_data` function can be found in the vignette [**link to preprocessing data vignette**].

## Running ML

The main function in mikRopML (`run_ml`) minimally takes in a data frame including a binary outcome and features, and model choice. mikRopML currently supports logistic regression [@paul_liblinear_2017], support vector machine with a radial basis kernel [@karatzoglou_kernlab_2004], decision tree [@therneau_rpart_2019], random forest [@liaw_classication_2002], and xgBoost [@chen_xgboost_2020]. It randomly splits the data to train and test sets while also maintaining the distribution of the two outcomes found in the full dataset.

It trains and tests the data using the `caret` R package [@kuhn_building_2008], evaluates the model using the `PRROC` R package [@grau_prroc_2015], and optionally quantifies feature importance.

The output includes the best model upon tuning hyperparameters in an internal and repeated cross-validation step, two model evaluation metrics (area under the receiver operating characteristics curve - AUROC, and area under the precision recall curve - AUPRC), and optional feature importances (Figure 1). 

The quantification of feature importance using permutation allows calculating the decrease in the model's prediction performance after breaking the relationship between the feature and the true outcome, and is thus particularly useful for model interpretation [@topcuoglu_framework_2020]. 

Our vignette [**link to vignette**] contains a comprehensive tutorial on how to use the `run_ml` function.

![Figure 1. MikRopML pipeline](mikRopML-pipeline.png){width=100%}

## Ideal workflow for running mikRopML with many different train/test splits

In addition to simply performing ML on a single train/test split, it is crucial to investigate the variation in model performance depending on the train and test set used [@topcuoglu_framework_2020; @lapp_machine_2020]. Therefore, we provide examples of how to run `mikRopML` pipeline many times with different train/test splits and get summary information about model performance in your local computer  [**link to  foreach vignette**] or on a high-computing cluster using a snakemake workflow [**link to snakemake workflow example**].

## Plotting ML results 

One particularly important aspect of ML is hyperparameter tuning. We have to explore a range of possibilities for hyperparameters to pick the ideal one for our model. 
Therefore, we provide a function (**insert name of function**) to plot the cross-validation AUROC of models built using different train/test splits to evaluate if we are exhausing our hyperparameter search range to pick the ideal one. 
We also provide summary plots of test AUROC and AUPRC values for the many train/test splits using (**insert name of function**).

## Dependencies

mikRopML is written in R [@r_core_team_r_2020] and depends on several packages: `PRROC` [@grau_prroc_2015], `dplyr` [@wickham_dplyr_2020], `rlan`g [@henry_rlang_2020] and `caret` [@kuhn_building_2008]. The ML algorithms we support require additional packages:
`kernlab` [@karatzoglou_kernlab_2004] for support vector machines, `rpart2` [@therneau_rpart_2019] for decision trees, `randomForest` [@liaw_classication_2002] for random forest, and `xgboost` [@chen_xgboost_2020] for xgBoost. 
We also allow for parallelization of model training using `doParallel` [@ooi_doparallel_2019] and `foreac`h [@ooi_foreach_2020], as well as parallelization of the base *apply family of functions using `future.apply` [@bengtsson_futureapply_2020].

# Acknowledgements

We'd like to thank members of the Schloss Lab who participated in code clubs related to the initial development of the pipeline. 

# Funding
<!--TODO Others add funding if needed -->
KLS received support from the NIH Training Program in Bioinformatics (T32 GM070449).
ZL received support from the National Science Foundation Graduate Research Fellowship Program under Grant No. DGE 1256260. 
Any opinions, findings, and conclusions or recommendations expressed in this material are those of the authors and do not necessarily reflect the views of the National Science Foundation.

# Author contributions

BT, ZL, and KLS conceptualized the study and created the package. 
BT, ZL, JW, and PDS developed methodology. 
PDS and JW supervised the project. 
BT, ZL, and KLS wrote the original draft. 
All authors reviewed and edited the manuscript.

# Conflicts of interest
<!--TODO Add conflicts of interest here -->

# References
