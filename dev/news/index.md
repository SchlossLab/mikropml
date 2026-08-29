# Changelog

## mikropml 1.7.1

- Allow `training_frac` equal to 1… really this time!
  ([\#360](https://github.com/SchlossLab/mikropml/issues/360),
  [@pschloss](https://github.com/pschloss))
- Note: the maximum `xgboost` version supported is `1.7` (see
  [\#362](https://github.com/SchlossLab/mikropml/issues/362)).
- Fix SummarizedExperiment export
  ([\#368](https://github.com/SchlossLab/mikropml/issues/368),
  [\#370](https://github.com/SchlossLab/mikropml/issues/370),
  [@kelly-sovacool](https://github.com/kelly-sovacool)).
- A container is now available in ghcr
  ([\#364](https://github.com/SchlossLab/mikropml/issues/364),
  [\#366](https://github.com/SchlossLab/mikropml/issues/366),
  [@kelly-sovacool](https://github.com/kelly-sovacool)).
  - see
    <https://github.com/SchlossLab/mikropml/pkgs/container/mikropml>.

## mikropml 1.7.0

CRAN release: 2025-10-29

- mikropml now supports datasets in `TreeSummarizedExperiment` format
  ([\#349](https://github.com/SchlossLab/mikropml/issues/349),
  [@TuomasBorman](https://github.com/TuomasBorman)).
- The correlation method and threshold used to collapse correlated
  features can now be set in
  [`preprocess_data()`](http://www.schlosslab.org/mikropml/dev/reference/preprocess_data.md)
  ([\#355](https://github.com/SchlossLab/mikropml/issues/355),
  [@Benjamin-Valderrama](https://github.com/Benjamin-Valderrama)).

## mikropml 1.6.2

CRAN release: 2025-08-22

- Allow `training_frac` equal to 1
  ([\#347](https://github.com/SchlossLab/mikropml/issues/347),
  [@kelly-sovacool](https://github.com/kelly-sovacool)).
- Fix for upcoming ggplot2 release
  ([\#352](https://github.com/SchlossLab/mikropml/issues/352),
  [@teunbrand](https://github.com/teunbrand)).

## mikropml 1.6.1

CRAN release: 2023-08-21

- Fix roxygen package doc syntax
  (<https://github.com/r-lib/roxygen2/issues/1491>,
  [@kelly-sovacool](https://github.com/kelly-sovacool)).

## mikropml 1.6.0

CRAN release: 2023-04-14

- New functions:
  - [`bootstrap_performance()`](http://www.schlosslab.org/mikropml/dev/reference/bootstrap_performance.md)
    allows you to calculate confidence intervals for the model
    performance from a single train/test split by bootstrapping the test
    set ([\#329](https://github.com/SchlossLab/mikropml/issues/329),
    [@kelly-sovacool](https://github.com/kelly-sovacool)).
  - [`calc_balanced_precision()`](http://www.schlosslab.org/mikropml/dev/reference/calc_balanced_precision.md)
    allows you to calculate balanced precision and balanced area under
    the precision-recall curve
    ([\#333](https://github.com/SchlossLab/mikropml/issues/333),
    [@kelly-sovacool](https://github.com/kelly-sovacool)).
- Improved output from `find_feature_importance()`
  ([\#326](https://github.com/SchlossLab/mikropml/issues/326),
  [@kelly-sovacool](https://github.com/kelly-sovacool)).
  - Renamed the column `names` to `feat` to represent each feature or
    group of correlated features.
  - New column `lower` and `upper` to report the bounds of the empirical
    95% confidence interval from the permutation test. See
    [`vignette('parallel')`](http://www.schlosslab.org/mikropml/dev/articles/parallel.md)
    for an example of plotting feature importance with confidence
    intervals.
- Minor documentation improvements
  ([\#323](https://github.com/SchlossLab/mikropml/issues/323),
  [\#332](https://github.com/SchlossLab/mikropml/issues/332),
  [@kelly-sovacool](https://github.com/kelly-sovacool)).

## mikropml 1.5.0

CRAN release: 2023-01-16

- New example showing how to plot feature importances in the `parallel`
  vignette ([\#310](https://github.com/SchlossLab/mikropml/issues/310),
  [@kelly-sovacool](https://github.com/kelly-sovacool)).
- You can now use `parRF`, a parallel implementation of the `rf` method,
  with the same default hyperparameters as `rf` set automatically
  ([\#306](https://github.com/SchlossLab/mikropml/issues/306),
  [@kelly-sovacool](https://github.com/kelly-sovacool)).
- New functions to calculate and plot ROC and PRC curves:
  ([\#321](https://github.com/SchlossLab/mikropml/issues/321),
  [@kelly-sovacool](https://github.com/kelly-sovacool))
  - [`calc_model_sensspec()`](http://www.schlosslab.org/mikropml/dev/reference/sensspec.md) -
    calculate sensitivity, specificity, and precision for a model.
  - [`calc_mean_roc()`](http://www.schlosslab.org/mikropml/dev/reference/sensspec.md)
    &
    [`plot_mean_roc()`](http://www.schlosslab.org/mikropml/dev/reference/plot_curves.md) -
    calculate & plot specificity and mean sensitivity for multiple
    models.
  - [`calc_mean_prc()`](http://www.schlosslab.org/mikropml/dev/reference/sensspec.md)
    &
    [`plot_mean_prc()`](http://www.schlosslab.org/mikropml/dev/reference/plot_curves.md) -
    calculate & plot recall and mean precision for multiple models.

## mikropml 1.4.0

CRAN release: 2022-10-16

- Extra arguments given to
  [`run_ml()`](http://www.schlosslab.org/mikropml/dev/reference/run_ml.md)
  are now forwarded to
  [`caret::train()`](https://rdrr.io/pkg/caret/man/train.html)
  ([\#304](https://github.com/SchlossLab/mikropml/issues/304),
  [@kelly-sovacool](https://github.com/kelly-sovacool)).
  - Users can now pass any model-specific arguments (e.g. `weights`) to
    [`caret::train()`](https://rdrr.io/pkg/caret/man/train.html),
    allowing greater flexibility.
- Improved tests
  ([\#298](https://github.com/SchlossLab/mikropml/issues/298),
  [\#300](https://github.com/SchlossLab/mikropml/issues/300),
  [\#303](https://github.com/SchlossLab/mikropml/issues/303)
  \#kelly-sovacool)
- Minor documentation improvements.

## mikropml 1.3.0

CRAN release: 2022-05-20

- mikropml now requires R version 4.1.0 or greater due to an update in
  the randomForest package
  ([\#292](https://github.com/SchlossLab/mikropml/issues/292)).
- New function
  [`compare_models()`](http://www.schlosslab.org/mikropml/dev/reference/compare_models.md)
  compares the performance of two models with a permutation test
  ([\#295](https://github.com/SchlossLab/mikropml/issues/295),
  [@courtneyarmour](https://github.com/courtneyarmour)).
- Fixed a bug where `cv_times` did not affect the reported repeats for
  cross-validation
  ([\#291](https://github.com/SchlossLab/mikropml/issues/291),
  [@kelly-sovacool](https://github.com/kelly-sovacool)).
- Made minor documentation improvements
  ([\#293](https://github.com/SchlossLab/mikropml/issues/293),
  [@kelly-sovacool](https://github.com/kelly-sovacool))

## mikropml 1.2.2

CRAN release: 2022-02-03

This minor patch fixes a test failure on platforms with no long doubles.
The actual package code remains unchanged.

## mikropml 1.2.1

CRAN release: 2022-01-30

- Allow `kfold >= length(groups)`
  ([\#285](https://github.com/SchlossLab/mikropml/issues/285),
  [@kelly-sovacool](https://github.com/kelly-sovacool)).
  - When using the groups parameter, groups are kept together in
    cross-validation partitions when `kfold` \<= the number of groups in
    the training set. Previously, an error was thrown if this condition
    was not met. Now, if there are not enough groups in the training set
    for groups to be kept together during CV, groups are allowed to be
    split up across CV partitions.
- Report p-values for permutation feature importance
  ([\#288](https://github.com/SchlossLab/mikropml/issues/288),
  [@kelly-sovacool](https://github.com/kelly-sovacool)).

## mikropml 1.2.0

CRAN release: 2021-11-10

- New parameter `cross_val` added to
  [`run_ml()`](http://www.schlosslab.org/mikropml/dev/reference/run_ml.md)
  allows users to define their own custom cross-validation scheme
  ([\#278](https://github.com/SchlossLab/mikropml/issues/278),
  [@kelly-sovacool](https://github.com/kelly-sovacool)).
  - Also added a new parameter `calculate_performance`, which controls
    whether performance metrics are calculated (default: `TRUE`). Users
    may wish to skip performance calculations when training models with
    no cross-validation.
- New parameter `group_partitions` added to
  [`run_ml()`](http://www.schlosslab.org/mikropml/dev/reference/run_ml.md)
  allows users to control which groups should go to which partition of
  the train/test split
  ([\#281](https://github.com/SchlossLab/mikropml/issues/281),
  [@kelly-sovacool](https://github.com/kelly-sovacool)).
- Modified the `training_frac` parameter in
  [`run_ml()`](http://www.schlosslab.org/mikropml/dev/reference/run_ml.md)
  ([\#281](https://github.com/SchlossLab/mikropml/issues/281),
  [@kelly-sovacool](https://github.com/kelly-sovacool)).
  - By default, `training_frac` is a fraction between 0 and 1 that
    specifies how much of the dataset should be used in the training
    fraction of the train/test split.
  - Users can instead give `training_frac` a vector of indices that
    correspond to which rows of the dataset should go in the training
    fraction of the train/test split. This gives users direct control
    over exactly which observations are in the training fraction if
    desired.

## mikropml 1.1.1

CRAN release: 2021-09-14

- Fixed bugs related to grouping correlated features
  ([\#276](https://github.com/SchlossLab/mikropml/issues/276),
  [@kelly-sovacool](https://github.com/kelly-sovacool)).
  - Also,
    [`group_correlated_features()`](http://www.schlosslab.org/mikropml/dev/reference/group_correlated_features.md)
    is now a user-facing function.

## mikropml 1.1.0

CRAN release: 2021-08-10

- New correlation method option for feature importance
  ([\#267](https://github.com/SchlossLab/mikropml/issues/267),
  [@courtneyarmour](https://github.com/courtneyarmour)).
  - The default is still “spearman”, and now you can use other methods
    supported by [`stats::cor`](https://rdrr.io/r/stats/cor.html) with
    the `corr_method` parameter:
    `get_feature_importance(corr_method = "pearson")`
- There are now [video
  tutorials](https://www.youtube.com/playlist?list=PLmNrK_nkqBpKpzb9-vI4V7SdXC-jXEcmg)
  covering mikropml and other skills related to machine learning,
  created by [@pschloss](https://github.com/pschloss)
  ([\#270](https://github.com/SchlossLab/mikropml/issues/270)).
- Fixed a bug where
  [`preprocess_data()`](http://www.schlosslab.org/mikropml/dev/reference/preprocess_data.md)
  converted the outcome column to a character vector
  ([\#273](https://github.com/SchlossLab/mikropml/issues/273),
  [@kelly-sovacool](https://github.com/kelly-sovacool),
  [@ecmaggioncalda](https://github.com/ecmaggioncalda)).

## mikropml 1.0.0

CRAN release: 2021-05-13

- mikropml now has a logo created by
  [@NLesniak](https://github.com/NLesniak)!
- Made documentation improvements
  ([\#238](https://github.com/SchlossLab/mikropml/issues/238),
  [\#231](https://github.com/SchlossLab/mikropml/issues/231)
  [@kelly-sovacool](https://github.com/kelly-sovacool);
  [\#256](https://github.com/SchlossLab/mikropml/issues/256)
  [@BTopcuoglu](https://github.com/BTopcuoglu)).
- New option in
  [`preprocess_data()`](http://www.schlosslab.org/mikropml/dev/reference/preprocess_data.md):
  `prefilter_threshold`
  ([\#240](https://github.com/SchlossLab/mikropml/issues/240),
  [@kelly-sovacool](https://github.com/kelly-sovacool),
  [@courtneyarmour](https://github.com/courtneyarmour)).
  - Remove any features that appear in N=`prefilter_threshold` or fewer
    rows in the data.
  - Created function
    [`remove_singleton_columns()`](http://www.schlosslab.org/mikropml/dev/reference/remove_singleton_columns.md)
    called by
    [`preprocess_data()`](http://www.schlosslab.org/mikropml/dev/reference/preprocess_data.md)
    to carry this out.
- New option in
  [`get_feature_importance()`](http://www.schlosslab.org/mikropml/dev/reference/get_feature_importance.md):
  `groups` ([\#246](https://github.com/SchlossLab/mikropml/issues/246),
  [@kelly-sovacool](https://github.com/kelly-sovacool)).
  - Provide custom groups of features to permute together during
    permutation importance.
  - `groups` is `NULL` by default; in this case, correlated features
    above `corr_thresh` are grouped together.
- [`preprocess_data()`](http://www.schlosslab.org/mikropml/dev/reference/preprocess_data.md)
  now replaces spaces in the outcome column with underscores
  ([\#247](https://github.com/SchlossLab/mikropml/issues/247),
  [@kelly-sovacool](https://github.com/kelly-sovacool),
  [@JonnyTran](https://github.com/JonnyTran)).
- Clarify in the intro vignette that we do not support multi-label
  outcomes. ([\#254](https://github.com/SchlossLab/mikropml/issues/254),
  [@zenalapp](https://github.com/zenalapp))
- Optional progress bar for
  [`preprocess_data()`](http://www.schlosslab.org/mikropml/dev/reference/preprocess_data.md)
  and
  [`get_feature_importance()`](http://www.schlosslab.org/mikropml/dev/reference/get_feature_importance.md)
  using [the progressr
  package](https://github.com/futureverse/progressr)
  ([\#257](https://github.com/SchlossLab/mikropml/issues/257),
  [@kelly-sovacool](https://github.com/kelly-sovacool),
  [@JonnyTran](https://github.com/JonnyTran),
  [@FedericoComoglio](https://github.com/FedericoComoglio)).
- The mikropml paper is soon to be published in
  [JOSS](https://joss.theoj.org/papers/10.21105/joss.03073)!

## mikropml 0.0.2

CRAN release: 2020-12-03

- Fixed a test failure on Solaris.
- Fixed multiple test failures with R 3.6.2 due to `stringsAsFactors`
  behavior.
- Made minor documentation improvements.
- Moved `rpart` from Suggests to Imports for consistency with other
  packages used during model training.

## mikropml 0.0.1

CRAN release: 2020-11-23

This is the first release version of mikropml! 🎉

- Added a `NEWS.md` file to track changes to the package.
- Major new functions:
  - [`run_ml()`](http://www.schlosslab.org/mikropml/dev/reference/run_ml.md)
  - [`preprocess_data()`](http://www.schlosslab.org/mikropml/dev/reference/preprocess_data.md)
  - [`plot_model_performance()`](http://www.schlosslab.org/mikropml/dev/reference/plot_model_performance.md)
  - [`plot_hp_performance()`](http://www.schlosslab.org/mikropml/dev/reference/plot_hp_performance.md)
- Support for ML methods in
  [`run_ml()`](http://www.schlosslab.org/mikropml/dev/reference/run_ml.md):
  - `glmnet`: logistic and linear regression
  - `rf`: random forest
  - `rpart2`: decision trees
  - `svmRadial`: support vector machines
  - `xgbTree`: gradient-boosted trees
- New vignettes:
  - [Introduction](http://www.schlosslab.org/mikropml/articles/introduction.md)
  - [Preprocess
    data](http://www.schlosslab.org/mikropml/articles/preprocess.md)
  - [Hyperparameter
    tuning](http://www.schlosslab.org/mikropml/articles/tuning.md)
  - [Parallel
    processing](http://www.schlosslab.org/mikropml/articles/parallel.md)
  - [The mikropml
    paper](http://www.schlosslab.org/mikropml/articles/paper.md)
