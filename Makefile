REFS = data/references
FIGS = results/figures
TABLES = results/tables
TEMP = data/temp
PROC = data/process
FINAL = submission/
CODE = code/learning

print-%:
	@echo '$*=$($*)'
################################################################################
#
# Part 1: Retrieve the subsampled shared file, taxonomy and metadata files that Marc Sze
# published in https://github.com/SchlossLab/Sze_CRCMetaAnalysis_mBio_2018
#
#	Copy from Github
#
################################################################################

data/baxter.0.03.subsample.shared\
data/metadata.tsv	:	code/learning/load_datasets.batch
	bash code/learning/load_datasets.batch

################################################################################
#
# Part 2: Model analysis in R
#
#	Run scripts to perform all the models on the dataset and generate AUC values
#	Each model has to be submitted seperately.
#	These will generate 100 datasplit results for 7 models
#	Submit each rule on the HPC parallelized.
#	First 7 rules should finish before we move on to combining step at rule 8
#
################################################################################
$(TEMP)/traintime_XGBoost_%.csv\
$(TEMP)/all_imp_features_cor_results_XGBoost_%.csv\
$(TEMP)/all_imp_features_non_cor_results_XGBoost_%.csv\
$(TEMP)/all_hp_results_XGBoost_%.csv\
$(TEMP)/best_hp_results_XGBoost_%.csv	:	data/baxter.0.03.subsample.shared\
														data/metadata.tsv\
														$(CODE)/generateAUCs.R\
														$(CODE)/model_pipeline.R\
														$(CODE)/model_interpret.R\
														$(CODE)/main.R\
														$(CODE)/model_selection.R
			Rscript code/learning/main.R $* "XGBoost"


$(TEMP)/traintime_Random_Forest_%.csv\
$(TEMP)/all_imp_features_cor_results_Random_Forest_%.csv\
$(TEMP)/all_imp_features_non_cor_results_Random_Forest_%.csv\
$(TEMP)/all_hp_results_Random_Forest_%.csv\
$(TEMP)/best_hp_results_Random_Forest_%.csv	:	data/baxter.0.03.subsample.shared\
														data/metadata.tsv\
														$(CODE)/generateAUCs.R\
														$(CODE)/model_pipeline.R\
														$(CODE)/model_interpret.R\
														$(CODE)/main.R\
														$(CODE)/model_selection.R
			Rscript code/learning/main.R $* "Random_Forest"

$(TEMP)/traintime_Decision_Tree_%.csv\
$(TEMP)/all_imp_features_cor_results_Decision_Tree_%.csv\
$(TEMP)/all_imp_features_non_cor_results_Decision_Tree_%.csv\
$(TEMP)/all_hp_results_Decision_Tree_%.csv\
$(TEMP)/best_hp_results_Decision_Tree_%.csv	:	data/baxter.0.03.subsample.shared\
														data/metadata.tsv\
														$(CODE)/generateAUCs.R\
														$(CODE)/model_pipeline.R\
														$(CODE)/model_interpret.R\
														$(CODE)/main.R\
														$(CODE)/model_selection.R
			Rscript code/learning/main.R $* "Decision_Tree"

$(TEMP)/traintime_RBF_SVM_%.csv\
$(TEMP)/all_imp_features_cor_results_RBF_SVM_%.csv\
$(TEMP)/all_imp_features_non_cor_results_RBF_SVM_%.csv\
$(TEMP)/all_hp_results_RBF_SVM_%.csv\
$(TEMP)/best_hp_results_RBF_SVM_%.csv	:	data/baxter.0.03.subsample.shared\
														data/metadata.tsv\
														$(CODE)/generateAUCs.R\
														$(CODE)/model_pipeline.R\
														$(CODE)/model_interpret.R\
														$(CODE)/main.R\
														$(CODE)/model_selection.R
			Rscript code/learning/main.R $* "RBF_SVM"

$(TEMP)/traintime_L1_Linear_SVM_%.csv\
$(TEMP)/all_imp_features_cor_results_L1_Linear_SVM_%.csv\
$(TEMP)/all_imp_features_non_cor_results_L1_Linear_SVM_%.csv\
$(TEMP)/all_hp_results_L1_Linear_SVM_%.csv\
$(TEMP)/best_hp_results_L1_Linear_SVM_%.csv	:	data/baxter.0.03.subsample.shared\
														data/metadata.tsv\
														$(CODE)/generateAUCs.R\
														$(CODE)/model_pipeline.R\
														$(CODE)/model_interpret.R\
														$(CODE)/main.R\
														$(CODE)/model_selection.R
			Rscript code/learning/main.R $* "L1_Linear_SVM"

$(TEMP)/traintime_L2_Linear_SVM_%.csv\
$(TEMP)/all_imp_features_cor_results_L2_Linear_SVM_%.csv\
$(TEMP)/all_imp_features_non_cor_results_L2_Linear_SVM_%.csv\
$(TEMP)/all_hp_results_L2_Linear_SVM_%.csv\
$(TEMP)/best_hp_results_L2_Linear_SVM_%.csv	:	data/baxter.0.03.subsample.shared\
														data/metadata.tsv\
														$(CODE)/generateAUCs.R\
														$(CODE)/model_pipeline.R\
														$(CODE)/model_interpret.R\
														$(CODE)/main.R\
														$(CODE)/model_selection.R
			Rscript code/learning/main.R $* "L2_Linear_SVM"

$(TEMP)/traintime_L2_Logistic_Regression_%.csv\
$(TEMP)/all_imp_features_cor_results_L2_Logistic_Regression_%.csv\
$(TEMP)/all_imp_features_non_cor_results_L2_Logistic_Regression_%.csv\
$(TEMP)/all_hp_results_L2_Logistic_Regression_%.csv\
$(TEMP)/best_hp_results_L2_Logistic_Regression_%.csv	:	data/baxter.0.03.subsample.shared\
														data/metadata.tsv\
														$(CODE)/generateAUCs.R\
														$(CODE)/model_pipeline.R\
														$(CODE)/model_interpret.R\
														$(CODE)/main.R\
														$(CODE)/model_selection.R
			Rscript code/learning/main.R $* "L2_Logistic_Regression"

# Create variable names with patterns to describe temporary files

SEEDS=$(shell seq 0 99)
OBJECTS=L1_Linear_SVM L2_Linear_SVM L2_Logistic_Regression RBF_SVM Decision_Tree Random_Forest XGBoost

BEST_REPS_FILES = $(foreach S,$(SEEDS),$(foreach O,$(OBJECTS),$(TEMP)/best_hp_results_$(O)_$(S).csv))
ALL_REPS_FILES = $(foreach S,$(SEEDS),$(foreach O,$(OBJECTS),$(TEMP)/all_hp_results_$(O)_$(S).csv))
COR_IMP_REPS_FILES = $(foreach S,$(SEEDS),$(foreach O,$(OBJECTS),$(TEMP)/all_imp_features_cor_results_$(O)_$(S).csv))
NON_COR_IMP_REPS_FILES = $(foreach S,$(SEEDS),$(foreach O,$(OBJECTS),$(TEMP)/all_imp_features_non_cor_results_$(O)_$(S).csv))
TIME_REPS_FILES = $(foreach S,$(SEEDS),$(foreach O,$(OBJECTS),$(TEMP)/traintime_$(O)_$(S).csv))

# Create variable names with patterns to describe processed files that are combined

BEST_COMB_FILES = $(foreach O,$(OBJECTS),$(PROC)/combined_best_hp_results_$(O).csv)
ALL_COMB_FILES = $(foreach O,$(OBJECTS),$(PROC)/combined_all_hp_results_$(O).csv)
COR_COMB_FILES = $(foreach O,$(OBJECTS),$(PROC)/combined_all_imp_features_cor_results_$(O).csv)
NON_COR_COMB_FILES = $(foreach O,$(OBJECTS),$(PROC)/combined_all_imp_features_non_cor_results_$(O).csv)
TIME_COMB_FILES = $(foreach O,$(OBJECTS),$(PROC)/traintime_$(O).csv)

# Combine all the files generated from each submitted job

$(BEST_COMB_FILES)\
$(ALL_COMB_FILES)\
$(COR_COMB_FILES)\
$(NON_COR_COMB_FILES)\
$(TIME_COMB_FILES)\	:	$(BEST_REPS_FILES)\
						$(ALL_REPS_FILES)\
						$(COR_IMP_REPS_FILES)\
						$(NON_COR_IMP_REPS_FILES)\
						$(TIME_REPS_FILES)\
						code/cat_csv_files.sh
	bash code/cat_csv_files.sh

# Take the individual correlated importance files of linear models which have weights of each feature for each datasplit and create feature rankings for each datasplit
# Then combine each feature ranking into 1 combined file
DATA=feature_ranking

$(PROC)/combined_L1_Linear_SVM_$(DATA).tsv\
$(PROC)/combined_L2_Linear_SVM_$(DATA).tsv\
$(PROC)/combined_L2_Logistic_Regression_$(DATA).tsv	:	$(L2_LOGISTIC_REGRESSION_COR_IMP_REPS)\
												$(L1_LINEAR_SVM_COR_IMP_REPS)\
												$(L2_LINEAR_SVM_COR_IMP_REPS)\
												code/learning/get_feature_rankings.R\
												code/merge_feature_ranks.sh
	Rscript code/learning/get_feature_rankings\
	bash code/merge_feature_ranks.sh


################################################################################
#
# Part 3: Figure and table generation
#
#	Run scripts to generate figures and tables
#
################################################################################

# Figure 2 shows the generalization performance of all the models tested.
$(FINAL)/Figure_2.png	:	$(CODE)/functions.R\
							$(CODE)/Figure2.R\
							$(BEST_COMB_FILES)
					Rscript $(CODE)/Figure2.R

# Figure 3 shows the linear model interpretation with weight rankings
$(FINAL)/Figure_3.png	:	$(CODE)/functions.R\
							$(CODE)/Figure3.R\
							data/baxter.taxonomy\
							$(PROC)/combined_L1_Linear_SVM_$(DATA).tsv\
							$(PROC)/combined_L2_Linear_SVM_$(DATA).tsv\
							$(PROC)/combined_L2_Logistic_Regression_$(DATA).tsv
					Rscript $(CODE)/Figure3.R

# Figure 4 shows non-linear model interpretation with permutation importance
$(FINAL)/Figure_4.png	:	$(CODE)/functions.R\
							$(CODE)/Figure4.R\
							$(BEST_COMB_FILES)\
							$(COR_COMB_FILES)\
							$(NON_COR_COMB_FILES)
					Rscript $(CODE)/Figure4.R

# Figure 5 shows training times of each model

$(FINAL)/Figure_5.png	:	$(CODE)/functions.R\
							$(CODE)/Figure5.R\
							$(TIME_COMB_FILES)
					Rscript $(CODE)/Figure5.R

# Figure S1 shows the hyper-parameter tuning AUC values of linear models
$(FINAL)/Figure_S1.png	:	$(CODE)/functions.R\
							$(CODE)/FigureS1.R\
							$(ALL_COMB_FILES)
					Rscript $(CODE)/FigureS1.R

# Figure S2 shows the hyper-parameter tuning AUC values of non-linear models

$(FINAL)/Figure_S2.png	:	$(CODE)/functions.R\
							$(CODE)/FigureS1.R\
							$(ALL_COMB_FILES)
					Rscript $(CODE)/FigureS2.R


# Table 1 is a summary of the compelxity properties of all the models tested.
$(FINAL)/TableS1.pdf :	$(FINAL)/Table1.Rmd\
						$(FINAL)/header.tex
	R -e "rmarkdown::render('$(FINAL)/Table1.Rmd', clean=TRUE)"


################################################################################
#
# Part 4: Pull it all together
#
# Render the manuscript
#
################################################################################


$(FINAL)/manuscript.%	:	$(FINAL)/mbio.csl\
							$(FINAL)/references.bib\
							$(FINAL)/manuscript.Rmd
	R -e 'rmarkdown::render("$(FINAL)/manuscript.Rmd", clean=FALSE)'
	mv $(FINAL)/manuscript.knit.md submission/manuscript.md
	rm $(FINAL)/manuscript.utf8.md


write.paper :	$(FINAL)/Figure_1.pdf\
				$(FINAL)/Figure_2.png\
				$(FINAL)/Figure_3.png\
				$(FINAL)/Figure_4.png\
				$(FINAL)/Figure_5.png\
				$(FINAL)/Figure_S1.png\
				$(FINAL)/Figure_S1.png\
				$(FINAL)/manuscript.Rmd\
				$(FINAL)/manuscript.md\
				$(FINAL)/manuscript.tex\
				$(FINAL)/manuscript.pdf
