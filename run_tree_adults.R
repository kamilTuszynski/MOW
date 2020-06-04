rm(list = ls())

library(rpart,quietly = TRUE)
library(caret,quietly = TRUE)
library(rpart.plot,quietly = TRUE)

source("local_classification.R")

data = read.csv("adult_fixed.csv")

set.seed(12345) # for reproducibility

train <- sample(1:nrow(data),size = ceiling(0.999*nrow(data)),replace = FALSE)
data_train <- data[train,]
data_test <- data[-train,]

start.time <- Sys.time()


predLocal <- factor()

# for test minsplit and maxdepth
control = rpart.control()

for(i in 1:nrow(data_test)) {
  row <- data_test[i,]
  local <- localClassification(row, data_train, 8000, algorithm = "DecisionTree", controlTree = control)
  
  predLocal = unlist(list(predLocal,local))
  
  cat(sprintf("%s z %s\n", i, nrow(data_test)))
}

tree <- rpart(class~.,
              data=data_train,
              control = control,
              method = "class")
predGlobal <- predict(object=tree,data_test,type="class")


end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

tLocal <- table(data_test$class,predLocal)
tGlobal <- table(data_test$class,predGlobal)
confusionMatrix(tLocal)
confusionMatrix(tGlobal)