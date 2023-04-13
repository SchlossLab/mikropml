# mikropml development version

- New function `bootstrap_performance()` allows you to calculate confidence 
  intervals for the model performance from a single train/test split by
  bootstrapping the test set (#329, @kelly-sovacool).
- New function `calc_balanced_precision()` allows you to calculate balanced
  precision and balanced area under the precision-recall curve (#333, @kelly-sovacool).
- Improved output from `find_feature_importance()` (#326, @kelly-sovacool).
    - Renamed the column `names` to `feat` to represent each feature or group of correlated features.
    - New column `lower` and `upper` to report the bounds of the empirical 95% confidence interval from the permutation test.
      See `vignette('parallel')` for an example of plotting feature importance with confidence intervals.
- Minor documentation improvements (#323, @kelly-sovacool).
- Added option to impute missing data during training rather than preprocessing (#301, @megancoden and @shah-priyal).
  - Added impute_in_training option to `run_ml()`, which defaults to FALSE.
  - Added impute_in_preprocessing option to `preprocess()`, which defaults to TRUE.

# mikropml 1.5.0

- New example showing how to plot feature importances in the `parallel` vignette (#310, @kelly-sovacool).
- You can now use `parRF`, a parallel implementation of the `rf` method, with
  the same default hyperparameters as `rf` set automatically (#306, @kelly-sovacool).
- New functions to calculate and plot ROC and PRC curves: (#321, @kelly-sovacool)
  - `calc_model_sensspec()` - calculate sensitivity, specificity, and precision for a model.
  - `calc_mean_roc()` & `plot_mean_roc()` - calculate & plot specificity and mean sensitivity for multiple models.
  - `calc_mean_prc()` & `plot_mean_prc()` - calculate & plot recall and mean precision for multiple models.

# mikropml 1.4.0

- Extra arguments given to `run_ml()` are now forwarded to `caret::train()` (#304, @kelly-sovacool).
    - Users can now pass any model-specific arguments (e.g. `weights`) to `caret::train()`, allowing greater flexibility.
- Improved tests (#298, #300, #303 #kelly-sovacool)
- Minor documentation improvements.

# mikropml 1.3.0

- mikropml now requires R version 4.1.0 or greater due to an update in the randomForest package (#292). 
- New function `compare_models()` compares the performance of two models with a permutation test (#295, @courtneyarmour).
- Fixed a bug where `cv_times` did not affect the reported repeats for cross-validation (#291, @kelly-sovacool).
- Made minor documentation improvements (#293, @kelly-sovacool)

# mikropml 1.2.2

This minor patch fixes a test failure on platforms with no long doubles.
The actual package code remains unchanged.

# mikropml 1.2.1

- Allow `kfold >= length(groups)` (#285, @kelly-sovacool).
    - When using the groups parameter, groups are kept together in cross-validation partitions when `kfold` <= the number of groups in the training set. Previously, an error was thrown if this condition was not met. Now, if there are not enough groups in the training set for groups to be kept together during CV, groups are allowed to be split up across CV partitions. 
- Report p-values for permutation feature importance (#288, @kelly-sovacool).

# mikropml 1.2.0

- New parameter `cross_val` added to `run_ml()` allows users to define their own custom cross-validation scheme (#278, @kelly-sovacool).
    - Also added a new parameter `calculate_performance`, which controls whether performance metrics are calculated (default: `TRUE`). Users may wish to skip performance calculations when training models with no cross-validation.
- New parameter `group_partitions` added to `run_ml()` allows users to control which groups should go to which partition of the train/test split (#281, @kelly-sovacool).
- Modified the `training_frac` parameter in `run_ml()` (#281, @kelly-sovacool).
    - By default, `training_frac` is a fraction between 0 and 1 that specifies how much of the dataset should be used in the training fraction of the train/test split.
    - Users can instead give `training_frac` a vector of indices that correspond to which rows of the dataset should go in the training fraction of the train/test split. This gives users direct control over exactly which observations are in the training fraction if desired.

# mikropml 1.1.1

- Fixed bugs related to grouping correlated features (#276, @kelly-sovacool).
    - Also, `group_correlated_features()` is now a user-facing function.

# mikropml 1.1.0

- New correlation method option for feature importance (#267, @courtneyarmour).
    - The default is still "spearman", and now you can use other methods supported by `stats::cor` with the `corr_method` parameter: `get_feature_importance(corr_method = "pearson")`
- There are now [video tutorials](https://www.youtube.com/playlist?list=PLmNrK_nkqBpKpzb9-vI4V7SdXC-jXEcmg) covering mikropml and other skills related to machine learning, created by @pschloss (#270).
- Fixed a bug where `preprocess_data()` converted the outcome column to a character vector (#273, @kelly-sovacool, @ecmaggioncalda).

# mikropml 1.0.0

- mikropml now has a logo created by @NLesniak!
- Made documentation improvements (#238, #231 @kelly-sovacool; #256 @BTopcuoglu).
- New option in `preprocess_data()`: `prefilter_threshold` (#240, @kelly-sovacool, @courtneyarmour).
    - Remove any features that appear in N=`prefilter_threshold` or fewer rows in the data.
    - Created function `remove_singleton_columns()` called by `preprocess_data()` to carry this out.
- New option in `get_feature_importance()`: `groups` (#246, @kelly-sovacool).
    - Provide custom groups of features to permute together during permutation importance.
    - `groups` is `NULL` by default; in this case, correlated features above `corr_thresh` are grouped together.
- `preprocess_data()` now replaces spaces in the outcome column with underscores (#247, @kelly-sovacool, @JonnyTran).
- Clarify in the intro vignette that we do not support multi-label outcomes. (#254, @zenalapp)
- Optional progress bar for `preprocess_data()` and `get_feature_importance()` using [the progressr package](https://github.com/HenrikBengtsson/progressr) (#257, @kelly-sovacool, @JonnyTran, @FedericoComoglio).
- The mikropml paper is soon to be published in [JOSS](https://joss.theoj.org/papers/10.21105/joss.03073)!

# mikropml 0.0.2

- Fixed a test failure on Solaris.
- Fixed multiple test failures with R 3.6.2 due to `stringsAsFactors` behavior.
- Made minor documentation improvements.
- Moved `rpart` from Suggests to Imports for consistency with other packages used during model training.

# mikropml 0.0.1

This is the first release version of mikropml! 🎉

- Added a `NEWS.md` file to track changes to the package.
- Major new functions:
    - `run_ml()`
    - `preprocess_data()`
    - `plot_model_performance()`
    - `plot_hp_performance()`
- Support for ML methods in `run_ml()`:
    - `glmnet`: logistic and linear regression
    - `rf`: random forest
    - `rpart2`: decision trees
    - `svmRadial`: support vector machines
    - `xgbTree`: gradient-boosted trees
- New vignettes:
    - [Introduction](http://www.schlosslab.org/mikropml/articles/introduction.html)
    - [Preprocess data](http://www.schlosslab.org/mikropml/articles/preprocess.html)
    - [Hyperparameter tuning](http://www.schlosslab.org/mikropml/articles/tuning.html)
    - [Parallel processing](http://www.schlosslab.org/mikropml/articles/parallel.html)
    - [The mikropml paper](http://www.schlosslab.org/mikropml/articles/paper.html)
