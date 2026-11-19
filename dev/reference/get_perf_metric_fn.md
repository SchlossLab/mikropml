# Get default performance metric function

Get default performance metric function

## Usage

``` r
get_perf_metric_fn(outcome_type)
```

## Arguments

- outcome_type:

  Type of outcome (one of: `"continuous"`,`"binary"`,`"multiclass"`).

## Value

Performance metric function.

## Author

Zena Lapp, <zenalapp@umich.edu>

## Examples

``` r
get_perf_metric_fn("continuous")
#> function (data, lev = NULL, model = NULL) 
#> {
#>     if (is.character(data$obs)) {
#>         data$obs <- factor(data$obs, levels = lev)
#>     }
#>     postResample(data[, "pred"], data[, "obs"])
#> }
#> <bytecode: 0x60a397055b20>
#> <environment: namespace:caret>
get_perf_metric_fn("binary")
#> function (data, lev = NULL, model = NULL) 
#> {
#>     if (!all(levels(data[, "pred"]) == levels(data[, "obs"]))) {
#>         stop("levels of observed and predicted data do not match")
#>     }
#>     has_class_probs <- all(lev %in% colnames(data))
#>     obs <- data[, "obs"]
#>     pred <- data[, "pred"]
#>     classes <- levels(pred)
#>     is_binary <- length(classes) == 2
#>     if (has_class_probs) {
#>         lloss <- mnLogLoss(data = data, lev = lev, model = model)
#>         prob_stats <- lapply(classes, function(x) {
#>             is_x <- factor(obs == x, levels = c(TRUE, FALSE))
#>             suppressWarnings(res <- c(ROC = prob_metric(yardstick::roc_auc_vec, 
#>                 is_x, data[, x]), AUC = prob_metric(yardstick::pr_auc_vec, 
#>                 is_x, data[, x])))
#>             res
#>         })
#>         prob_stats <- do.call("rbind", prob_stats)
#>         prob_stats <- colMeans(prob_stats, na.rm = TRUE)
#>     }
#>     if (is_binary) {
#>         estimator <- "binary"
#>     }
#>     else {
#>         estimator <- "macro"
#>     }
#>     class_stat <- function(fn) {
#>         suppressWarnings(fn(obs, pred, estimator = estimator))
#>     }
#>     if (is_binary) {
#>         detection_rate <- mean(pred == classes[1] & obs == classes[1])
#>     }
#>     else {
#>         detection_rate <- mean(pred == obs)/length(classes)
#>     }
#>     stats <- c(Accuracy = yardstick::accuracy_vec(obs, pred), 
#>         Kappa = yardstick::kap_vec(obs, pred), Mean_F1 = class_stat(yardstick::f_meas_vec), 
#>         Mean_Sensitivity = class_stat(yardstick::sens_vec), Mean_Specificity = class_stat(yardstick::spec_vec), 
#>         Mean_Pos_Pred_Value = class_stat(yardstick::ppv_vec), 
#>         Mean_Neg_Pred_Value = class_stat(yardstick::npv_vec), 
#>         Mean_Precision = class_stat(yardstick::precision_vec), 
#>         Mean_Recall = class_stat(yardstick::recall_vec), Mean_Detection_Rate = detection_rate, 
#>         Mean_Balanced_Accuracy = class_stat(yardstick::bal_accuracy_vec))
#>     if (has_class_probs) {
#>         stats <- c(logLoss = as.numeric(lloss), AUC = unname(prob_stats["ROC"]), 
#>             prAUC = unname(prob_stats["AUC"]), stats)
#>     }
#>     if (is_binary) {
#>         names(stats) <- gsub("^Mean_", "", names(stats))
#>     }
#>     stats[is.nan(stats)] <- NA
#>     stats
#> }
#> <bytecode: 0x60a38e06c410>
#> <environment: namespace:caret>
get_perf_metric_fn("multiclass")
#> function (data, lev = NULL, model = NULL) 
#> {
#>     if (!all(levels(data[, "pred"]) == levels(data[, "obs"]))) {
#>         stop("levels of observed and predicted data do not match")
#>     }
#>     has_class_probs <- all(lev %in% colnames(data))
#>     obs <- data[, "obs"]
#>     pred <- data[, "pred"]
#>     classes <- levels(pred)
#>     is_binary <- length(classes) == 2
#>     if (has_class_probs) {
#>         lloss <- mnLogLoss(data = data, lev = lev, model = model)
#>         prob_stats <- lapply(classes, function(x) {
#>             is_x <- factor(obs == x, levels = c(TRUE, FALSE))
#>             suppressWarnings(res <- c(ROC = prob_metric(yardstick::roc_auc_vec, 
#>                 is_x, data[, x]), AUC = prob_metric(yardstick::pr_auc_vec, 
#>                 is_x, data[, x])))
#>             res
#>         })
#>         prob_stats <- do.call("rbind", prob_stats)
#>         prob_stats <- colMeans(prob_stats, na.rm = TRUE)
#>     }
#>     if (is_binary) {
#>         estimator <- "binary"
#>     }
#>     else {
#>         estimator <- "macro"
#>     }
#>     class_stat <- function(fn) {
#>         suppressWarnings(fn(obs, pred, estimator = estimator))
#>     }
#>     if (is_binary) {
#>         detection_rate <- mean(pred == classes[1] & obs == classes[1])
#>     }
#>     else {
#>         detection_rate <- mean(pred == obs)/length(classes)
#>     }
#>     stats <- c(Accuracy = yardstick::accuracy_vec(obs, pred), 
#>         Kappa = yardstick::kap_vec(obs, pred), Mean_F1 = class_stat(yardstick::f_meas_vec), 
#>         Mean_Sensitivity = class_stat(yardstick::sens_vec), Mean_Specificity = class_stat(yardstick::spec_vec), 
#>         Mean_Pos_Pred_Value = class_stat(yardstick::ppv_vec), 
#>         Mean_Neg_Pred_Value = class_stat(yardstick::npv_vec), 
#>         Mean_Precision = class_stat(yardstick::precision_vec), 
#>         Mean_Recall = class_stat(yardstick::recall_vec), Mean_Detection_Rate = detection_rate, 
#>         Mean_Balanced_Accuracy = class_stat(yardstick::bal_accuracy_vec))
#>     if (has_class_probs) {
#>         stats <- c(logLoss = as.numeric(lloss), AUC = unname(prob_stats["ROC"]), 
#>             prAUC = unname(prob_stats["AUC"]), stats)
#>     }
#>     if (is_binary) {
#>         names(stats) <- gsub("^Mean_", "", names(stats))
#>     }
#>     stats[is.nan(stats)] <- NA
#>     stats
#> }
#> <bytecode: 0x60a38e06c410>
#> <environment: namespace:caret>
```
