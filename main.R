rm(list = ls())

library(rpart,quietly = TRUE)
library(caret,quietly = TRUE)
library(rpart.plot,quietly = TRUE)

source("find_nearest_neighbours.R")

data = getData()
example = data[1:1,]


set.seed(12345) # for reproducibility
train <- sample(1:nrow(data),size = ceiling(0.99*nrow(data)),replace = FALSE)
mushrooms_train <- data[train,]
mushrooms_test <- data[-train,]
train_data = data[1:5997,]
test_data = data[5998:5999,]

start.time <- Sys.time()

pred <- factor()
for(i in 1:nrow(mushrooms_test)) {
  row <- mushrooms_test[i,]
  local <- localClassification(row, mushrooms_train, 2000)
  pred = unlist(list(pred,local))
  cat(sprintf("%s z %s\n", i, nrow(mushrooms_test)))
}

tree <- rpart(class~.,
              data=mushrooms_train,
              method = "class")
pred2 <- predict(object=tree,mushrooms_test,type="class")


end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

t1 <- table(mushrooms_test$class,pred)
t2 <- table(mushrooms_test$class,pred2)
confusionMatrix(t1)
confusionMatrix(t2)