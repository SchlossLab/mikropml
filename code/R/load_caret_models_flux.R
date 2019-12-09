
# Download all models file from caret package
# We will parse, create and save models.RData here:
# ls Library/Frameworks/R.framework/Versions/3.5/Resources/library/caret/models/

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
save(models, file = "~/R/x86_64-pc-linux-gnu-library/3.5/caret/models/models.RData")

