rm(list = ls())

library(rpart,quietly = TRUE)
library(caret,quietly = TRUE)
library(rpart.plot,quietly = TRUE)
library(e1071,quietly = TRUE)

source("find_nearest_neighbours.R")

data = read.csv("adult.csv")
example = data[1:1,]


set.seed(12345) # for reproducibility
train <- sample(1:nrow(data),size = ceiling(0.999*nrow(data)),replace = FALSE)
mushrooms_train <- data[train,]
mushrooms_test <- data[-train,]
train_data = data[1:5997,]
test_data = data[5998:5999,]

start.time <- Sys.time()

alg = "NaiveBayes"

if(alg == "DecisionTree")
{
  pred <- factor()
}else if(alg == "NaiveBayes")
{
  pred <- matrix(ncol = 2, nrow=0)
}

for(i in 1:nrow(mushrooms_test)) {
  row <- mushrooms_test[i,]
  local <- localClassification(row, mushrooms_train, 2000, algorithm = alg, classIndex = ncol(data))
  if(alg == "DecisionTree")
  {
    pred = unlist(list(pred,local))
  }  else if(alg == "NaiveBayes")
  {
    pred = rbind(pred, local)
  }
  
  cat(sprintf("%s z %s\n", i, nrow(mushrooms_test)))
}
if(alg == "NaiveBayes"){
  #pred    <- ifelse ( pred[, 2] > 0.5, 'p', 'e' )
  pred    <- ifelse ( pred[, 2] > 0.5, '>50K', '<=50K' )
}


# tree <- rpart(class~.,
#               data=mushrooms_train,
#               method = "class")
# pred2 <- predict(object=tree,mushrooms_test,type="class")

nb.Model         <- naiveBayes( yearlyincome~., data = mushrooms_train )
pred2    <- predict( nb.Model, newdata = mushrooms_test, type = "raw" )
#pred2    <- ifelse ( pred2[, 2] > 0.5, 'p', 'e' )
pred2    <- ifelse ( pred2[, 2] > 0.5, '>50K', '<=50K' )


end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

t1 <- table(mushrooms_test$yearlyincome,pred)
t2 <- table(mushrooms_test$yearlyincome,pred2)
confusionMatrix(t1)
confusionMatrix(t2)