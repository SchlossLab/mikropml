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
#>     if (is.character(data$obs)) 
#>         data$obs <- factor(data$obs, levels = lev)
#>     postResample(data[, "pred"], data[, "obs"])
#> }
#> <bytecode: 0x55e993a80b40>
#> <environment: namespace:caret>
get_perf_metric_fn("binary")
#> function (data, lev = NULL, model = NULL) 
#> {
#>     if (!all(levels(data[, "pred"]) == levels(data[, "obs"]))) 
#>         stop("levels of observed and predicted data do not match")
#>     has_class_probs <- all(lev %in% colnames(data))
#>     if (has_class_probs) {
#>         lloss <- mnLogLoss(data = data, lev = lev, model = model)
#>         requireNamespaceQuietStop("pROC")
#>         requireNamespaceQuietStop("MLmetrics")
#>         prob_stats <- lapply(levels(data[, "pred"]), function(x) {
#>             obs <- ifelse(data[, "obs"] == x, 1, 0)
#>             prob <- data[, x]
#>             roc_auc <- try(pROC::roc(obs, data[, x], direction = "<", 
#>                 quiet = TRUE), silent = TRUE)
#>             roc_auc <- if (inherits(roc_auc, "try-error")) 
#>                 NA
#>             else roc_auc$auc
#>             pr_auc <- try(MLmetrics::PRAUC(y_pred = data[, x], 
#>                 y_true = obs), silent = TRUE)
#>             if (inherits(pr_auc, "try-error")) 
#>                 pr_auc <- NA
#>             res <- c(ROC = roc_auc, AUC = pr_auc)
#>             return(res)
#>         })
#>         prob_stats <- do.call("rbind", prob_stats)
#>         prob_stats <- colMeans(prob_stats, na.rm = TRUE)
#>     }
#>     CM <- confusionMatrix(data[, "pred"], data[, "obs"], mode = "everything")
#>     if (length(levels(data[, "pred"])) == 2) {
#>         class_stats <- CM$byClass
#>     }
#>     else {
#>         class_stats <- colMeans(CM$byClass)
#>         names(class_stats) <- paste("Mean", names(class_stats))
#>     }
#>     overall_stats <- if (has_class_probs) 
#>         c(CM$overall, logLoss = as.numeric(lloss), AUC = unname(prob_stats["ROC"]), 
#>             prAUC = unname(prob_stats["AUC"]))
#>     else CM$overall
#>     stats <- c(overall_stats, class_stats)
#>     stats <- stats[!names(stats) %in% c("AccuracyNull", "AccuracyLower", 
#>         "AccuracyUpper", "AccuracyPValue", "McnemarPValue", "Mean Prevalence", 
#>         "Mean Detection Prevalence")]
#>     names(stats) <- gsub("[[:blank:]]+", "_", names(stats))
#>     stat_list <- c("Accuracy", "Kappa", "Mean_F1", "Mean_Sensitivity", 
#>         "Mean_Specificity", "Mean_Pos_Pred_Value", "Mean_Neg_Pred_Value", 
#>         "Mean_Precision", "Mean_Recall", "Mean_Detection_Rate", 
#>         "Mean_Balanced_Accuracy")
#>     if (has_class_probs) 
#>         stat_list <- c("logLoss", "AUC", "prAUC", stat_list)
#>     if (length(levels(data[, "pred"])) == 2) 
#>         stat_list <- gsub("^Mean_", "", stat_list)
#>     stats <- stats[c(stat_list)]
#>     return(stats)
#> }
#> <bytecode: 0x55e998fc51b8>
#> <environment: namespace:caret>
get_perf_metric_fn("multiclass")
#> function (data, lev = NULL, model = NULL) 
#> {
#>     if (!all(levels(data[, "pred"]) == levels(data[, "obs"]))) 
#>         stop("levels of observed and predicted data do not match")
#>     has_class_probs <- all(lev %in% colnames(data))
#>     if (has_class_probs) {
#>         lloss <- mnLogLoss(data = data, lev = lev, model = model)
#>         requireNamespaceQuietStop("pROC")
#>         requireNamespaceQuietStop("MLmetrics")
#>         prob_stats <- lapply(levels(data[, "pred"]), function(x) {
#>             obs <- ifelse(data[, "obs"] == x, 1, 0)
#>             prob <- data[, x]
#>             roc_auc <- try(pROC::roc(obs, data[, x], direction = "<", 
#>                 quiet = TRUE), silent = TRUE)
#>             roc_auc <- if (inherits(roc_auc, "try-error")) 
#>                 NA
#>             else roc_auc$auc
#>             pr_auc <- try(MLmetrics::PRAUC(y_pred = data[, x], 
#>                 y_true = obs), silent = TRUE)
#>             if (inherits(pr_auc, "try-error")) 
#>                 pr_auc <- NA
#>             res <- c(ROC = roc_auc, AUC = pr_auc)
#>             return(res)
#>         })
#>         prob_stats <- do.call("rbind", prob_stats)
#>         prob_stats <- colMeans(prob_stats, na.rm = TRUE)
#>     }
#>     CM <- confusionMatrix(data[, "pred"], data[, "obs"], mode = "everything")
#>     if (length(levels(data[, "pred"])) == 2) {
#>         class_stats <- CM$byClass
#>     }
#>     else {
#>         class_stats <- colMeans(CM$byClass)
#>         names(class_stats) <- paste("Mean", names(class_stats))
#>     }
#>     overall_stats <- if (has_class_probs) 
#>         c(CM$overall, logLoss = as.numeric(lloss), AUC = unname(prob_stats["ROC"]), 
#>             prAUC = unname(prob_stats["AUC"]))
#>     else CM$overall
#>     stats <- c(overall_stats, class_stats)
#>     stats <- stats[!names(stats) %in% c("AccuracyNull", "AccuracyLower", 
#>         "AccuracyUpper", "AccuracyPValue", "McnemarPValue", "Mean Prevalence", 
#>         "Mean Detection Prevalence")]
#>     names(stats) <- gsub("[[:blank:]]+", "_", names(stats))
#>     stat_list <- c("Accuracy", "Kappa", "Mean_F1", "Mean_Sensitivity", 
#>         "Mean_Specificity", "Mean_Pos_Pred_Value", "Mean_Neg_Pred_Value", 
#>         "Mean_Precision", "Mean_Recall", "Mean_Detection_Rate", 
#>         "Mean_Balanced_Accuracy")
#>     if (has_class_probs) 
#>         stat_list <- c("logLoss", "AUC", "prAUC", stat_list)
#>     if (length(levels(data[, "pred"])) == 2) 
#>         stat_list <- gsub("^Mean_", "", stat_list)
#>     stats <- stats[c(stat_list)]
#>     return(stats)
#> }
#> <bytecode: 0x55e998fc51b8>
#> <environment: namespace:caret>
```
