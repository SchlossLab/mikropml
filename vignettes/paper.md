---
title: "mikRopML: A User-Friendly Machine Learning R Package for Binary Classification Problems"
output: 
  rmarkdown::html_vignette:
    keep_md: true
tags:
  - R
  - machine learning
  - logistic regression
  - random forest
  - xgBoost
  - microbiology
authors:
  - name: Begüm D. Topçuoğlu
    affiliation: 4
  - name: Zena Lapp
    orcid: 0000-0003-4674-2176
    affiliation: 1
  - name: Kelly L. Sovacool
    orcid: 0000-0003-3283-829X
    affiliation: 1
  - name: Jenna Wiens
    orcid: 0000-0002-1057-7722
    affiliation: 2
  - name: Patrick D. Schloss
    orcid: 0000-0002-6935-4275
    affiliation: 3
affiliations:
  - name: Department of Computational Medicine & Bioinformatics, University of Michigan
    index: 1q
  - name: Department of Electrical Engineering & Computer Science, University of Michigan
    index: 2
  - name: Department of Microbiology & Immunology, University of Michigan
    index: 3
  - name: Merck & Co., Inc.
    index: 4
date: 2020
bibliography: paper.bib
vignette: >
  %\VignetteIndexEntry{mikRopML paper}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---




# Summary

Machine learning (ML) for classification of data into groups based on pre-determined criteria is being used to make decisions in healthcare, economics, criminal justice and more. 
However, implementing a robust ML classification pipeline can be time-consuming, confusing, and difficult. 
Here, we present mikRopML (prononced "meek-ROPE em el"), an easy-to-use R package that acts as a wrapper around the R caret package and can be used for binary classification problems using logistic regression, random forest, xgBoost, and support vector machines. mikRopML is available on [GitHub/CRAN] (link). 

# Statement of need

Machine learning (ML) is widely used to analyze large datasets from fields ranging from biology to gender studies. 
Several packages including `caret` and `tidymodels` in R [@kuhn_building_2008] and `scikitlearn` in Python [@pedregosa_scikit-learn_2011] allow you to implement your own ML algorithms; however, creating a robust machine learning pipeline can be overwhelming, particularly for beginners. 
ML requires data pre-processing, cross-validation, testing, model evaluation, and often interpretation of why the model makes a certain prediction. 
Performing these steps using the correct methodology is extremely important as failure to implement a robust ML pipeline can result in incorrect and misleading results [@teschendorff_avoiding_2019]. 
To allow robust ML analyses and an easily accessible ML pipeline to a broader range of researchers, we created mikRopML (**[insert path to GitHub or CRAN]**), an easy-to-use package in R [@r_core_team_r_2020] that implements the Topçuoğlu _et al._ [@topcuoglu_framework_2020] ML framework for binary classification problems using logistic regression [@paul_liblinear_2017], random forest [@liaw_classication_2002], xgBoost [@chen_xgboost_2020], or support vector machines [**cite that pkg**]. 
Furthermore, we provide a data pre-processing function based on the FIDDLE framework outlined in Tang et al. [@tang_fiddle_2020].

While Topçuoğlu _et al._ [@topcuoglu_framework_2020] focused specifically on ML for microbiome data, the framework is generalizable to performing ML on other types of data as well. 
For instance, we have used the mikRopML framework to identify differences in genomic and clinical features associated with infection and colonization of a bacterial pathogen [@lapp_machine_2020]. 
Furthermore, we used the same framework to study gender representation and bias in journal articles [**cite Ada’s paper**]. 
Thus, this package can be used to study questions as diverse as what gut bacteria are associated with cancer to whether there are differences authorship gender representation in journal articles.

# mikRopML package

mikRopML has one main function called `run_ml` that minimally takes in a data frame including a binary outcome and features, and model choice (logistic regression [@paul_liblinear_2017], random forest [@liaw_classication_2002], or xgBoost [@chen_xgboost_2020]). 
It trains and tests the data using the `caret` R package [@kuhn_building_2008], evaluates the model using the `PRROC` R package [@grau_prroc_2015], and optionally quantifies feature importance. 
The output includes the best model, two model evaluation metrics (AUROC and AUPRC), and optional feature importances (Figure 1). 
Feature importance allows the users to more easily identify features that have a large impact on model performance, and is thus particularly useful for model interpretation [@topcuoglu_framework_2020]. 
Our vignette [link to vignette] contains a comprehensive tutorial on how to use mikRopML.

![Figure 1. MikRopML pipeline](mikRopML-pipeline.png)

We also provide a function that pre-processes features (`preprocess_data`) using several different functions from the `caret` package. 
The `preprocess_data` function takes continuous and categorical data, re-factors categorical data into binary features, and provides options to normalize continuous data, remove features with near-zero variance, and remove correlated features. 
We chose the default options based on those implemented in FIDDLE [@tang_fiddle_2020], and we direct users to this paper for more information on data pre-processing prior to ML.

mikRopML is written in R [@r_core_team_r_2020] and depends on several packages: PRROC [@grau_prroc_2015], dplyr [@wickham_dplyr_2020], caret [@kuhn_building_2008], LiblineaR [@paul_liblinear_2017], rlang [@henry_rlang_2020]. 
<!-- do we need to list all these? maybe should mention some suggests are needed depending on the ml method.  -->
<!-- say something about optional parallelization with doParallel & foreach in caret training -->
Packages that our package suggests are: testthat [@wickham_testthat_2011], knitr [@xie_dynamic_2015; @xie__aut_knitr_2020], rmarkdown [@allaire_rmarkdown_2020; @xie_r_2018], future.apply [@bengtsson_futureapply_2020], randomForest [@liaw_classication_2002], kernlab [@karatzoglou_kernlab_2004], rpart [@therneau_rpart_2019], xgboost [@chen_xgboost_2020], doParallel [@ooi_doparallel_2019], foreach [@ooi_foreach_2020].

In addition to simply performing ML on a single train/test split, we have found that it is important to investigate how model performance differs depending on the train and test set used [@topcuoglu_framework_2020; @lapp_machine_2020]. 
Therefore, we provide an example of how to integrate mikRopML into a snakemake workflow [link to snakemake workflow example] that runs machine learning 100 times and outputs summary information about model performance and feature importance.

# Acknowledgements

We'd like to thank members of the Schloss Lab who participated in code clubs related to the initial development of the pipeline. 

# Funding
<!--TODO Kelly and others add funding -->
ZL received support from the National Science Foundation Graduate Research Fellowship Program under Grant No. DGE 1256260. 
Any opinions, findings, and conclusions or recommendations expressed in this material are those of the authors and do not necessarily reflect the views of the National Science Foundation.

KLS received support from the NIH Training Program in Bioinformatics (T32 GM070449).

# Author contributions

BT, ZL, and KLS conceptualized the study and created the package. 
BT, ZL, JW, and PDS developed methodology. 
PDS and JW supervised the project. 
BT, ZL, and KLS wrote the original draft. 
All authors reviewed and edited the manuscript.

# Conflicts of interest
<!--TODO Add conflicts of interest here -->

# References
