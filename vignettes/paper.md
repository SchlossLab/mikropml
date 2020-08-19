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
Machine learning (ML) is a useful tool that researchers from diverse fields can use to investigate various probelms and questions. However, implementing a robust ML pipeline can be time-consuming, confusing, and difficult. Here, we presente mikRopML, an easy-to-use R package that can be used for binary classification problems using logistic regression, random forest, and xgBoost. mikRopML is available on [GitHub/CRAN] (link). 

# Statement of need

example citation [@topcuoglu_framework_2020]

Machine learning (ML) is widely used to analyze large datasets from fields ranging from biology to gender studies. Several packages including `caret` in R and `scikitlearn` in Python allow you to implement your own ML algorithms; however, creating a robust machine learning pipeline can be overwhelming for beginners. ML requires cross-validation, testing, model evaluation, and often interpretation of important features. Performing these steps using the correct methodology is extremely important as failure to implement a robust ML pipeline can result in incorrect and misleading results [cite some paper(s) by Jenna]. To make performing robust ML analyses easily accessible to a broader set of researchers, we created mikRopML ([insert path to GitHub or CRAN]), an easy-to-use R package that implements [Topcuoglu et al's ] ML framework for binary classification problems using logistic regression, random forest, and xgBoost. Furthermore, while [Topcuoglu et al] focused specifically on ML for microbiome data, the framework is generalizable to performing ML on other types of data as well. For instance, we have used the mikRopML framework to study differences in infection and colonization of a bacterial pathogen [cite Zena’s paper] and gender representation and bias in journal articles [cite Ada’s paper]. 

# mikRopML package

mikRopML has one main wrapper function called `run_ml` that minimally takes in a data frame including a binary outcome and features, and model choice (logistic regression, random forest, or xgBoost). It pre-processes the features using methods similar to those implemented in FIDDLE [cite FIDDLE], trains and tests the data using the `caret` R package, evaluates the model using the `PRROC` R package, and optionally quantifies feature importance. The output includes the best model, two model evaluation metrics (AUROC and AUPRC), and optional feature importances (Figure 1). Feature importance allows the users to more easily identify features that have a large impact on model performance, and is thus particularly useful for model interpretation [cite Begum's paper]. Our vignette [link to vignette] contains a comprehensive tutorial on how to use mikRopML.

<!-- ![Figure 1. MikRopML pipeline](./mikRopML-pipeline.pdf){width=100%} -->

<div class="figure" style="text-align: center">
<img src="./mikRopML-pipeline.png" alt="Figure 1. MikRopML pipeline" width="0.75\linewidth" />
<p class="caption">Figure 1. MikRopML pipeline</p>
</div>

The mikRopML package has the following dependencies: [list and cite dependencies]. 

In addition to simply performing ML on a single train/test split, we have found that it is important to investigate how model performance differs depending on the train and test set used [cite B&Z papers]. Therefore, we provide an example of how to integrate mikRopML into a snakemake workflow [link to snakemake workflow example] that runs machine learning 100 times and outputs summary information about model and feature importance.

# Acknowledgements

We'd like to thank member of the Schloss Lab who participated in code clubs related to the initial development of the pipeline. 

# References
