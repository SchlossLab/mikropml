
# Download all models file from caret package
# We will parse, create and save models.RData here:
# ls Library/Frameworks/R.framework/Versions/3.5/Resources/library/caret/models/
# This directory will be different for you. Make sure change the path to where caret is saved. 

setwd("data/caret_models")

modelFiles <- list.files(pattern = "\\.R$")

models <- vector(mode = "list", length = length(modelFiles))
names(models) <- gsub("\\.R$", "", modelFiles)

for(i in seq(along = modelFiles)) {
  source(modelFiles[i])
  models[[i]] <- modelInfo
  rm(modelInfo)
}
# Save to your caret package directory, into the models/ subdirectory
save(models, file = "/Library/Frameworks/R.framework/Versions/3.5/Resources/library/caret/models/models.RData")
