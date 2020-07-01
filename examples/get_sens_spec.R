# NOTE: This code is just pasted here so we have it in case we want to use it later.
# It can't be run as is and it might be buggy!!

rpartProbs <- predict(trained_model, test_data, type="prob")
bin_outcome <- ifelse(test_data[,outcome] == names(rpartProbs)[1], 1, 0)
test_roc <- roc(bin_outcome,rpartProbs[[1]],direction='<')

# Calculate sensitivity and specificity for 0.5 decision threshold.
p_class <- ifelse(rpartProbs[[1]] > 0.5, second_outcome, first_outcome)
r <- confusionMatrix(as.factor(p_class), test_data[,outcome])
sensitivity <- r$byClass[[1]]
specificity <- r$byClass[[2]]
# best decision threshold (youden method)
thr <- coords(test_roc, "best", ret = "threshold")


