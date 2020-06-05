rm(list = ls())

library(caret,quietly = TRUE)
library(e1071,quietly = TRUE)

source("local_classification.R")

data = read.csv("mushrooms.csv")

set.seed(12345) # for reproducibility

train <- sample(1:nrow(data),size = ceiling(0.85*nrow(data)),replace = FALSE)
data_train <- data[train,]
data_test <- data[-train,]

start.time <- Sys.time()


predLocal <- matrix(ncol = 2, nrow=0)

for(i in 1:nrow(data_test)) {
  row <- data_test[i,]
  local <- localClassification(row, data_train, 4000, algorithm = "NaiveBayes")
  
  predLocal = rbind(predLocal, local)
  
  cat(sprintf("%s z %s\n", i, nrow(data_test)))
}

predLocal <- ifelse ( predLocal[, 2] > 0.5, 'p', 'e' )


nb.Model         <- naiveBayes( class~., data = data_train )
predGlobal    <- predict( nb.Model, newdata = data_test, type = "raw" )
predGlobal    <- ifelse ( predGlobal[, 2] > 0.5, 'p', 'e' )


end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

tLocal <- table(data_test$class,predLocal)
tGlobal <- table(data_test$class,predGlobal)
confusionMatrix(tLocal)
confusionMatrix(tGlobal)