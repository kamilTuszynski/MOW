rm(list = ls())

library(rpart,quietly = TRUE)
library(caret,quietly = TRUE)
library(rpart.plot,quietly = TRUE)


# reading the dataset as a dataframe
mushrooms <- read.csv("adult.csv");
# structure of the data
str(mushrooms)
# number of rows with missing values
nrow(mushrooms) - sum(complete.cases(mushrooms))


# deleting useless variable `veil.type`
mushrooms$veil.type <- NULL

### Simple algorithm to gauge variable importance

table(mushrooms$class,mushrooms$odor)

  

number.perfect.splits <- apply(X=mushrooms[-1], MARGIN = 2, FUN = function(col){
    t <- table(mushrooms$class,col)
    sum(t == 0)
})

# order by number of prefect splits decreasing
order <- order(number.perfect.splits,decreasing = TRUE)
number.perfect.splits <- number.perfect.splits[order]
# plot results
par(mar=c(10,2,2,2))
barplot(number.perfect.splits,
        main="Number of perfect splits vs feature",
        xlab="",ylab="Feature",las=2,col="wheat")


## Splitting the data into training and testing sets

set.seed(12345) # for reproducibility
train <- sample(1:nrow(mushrooms),size = ceiling(0.80*nrow(mushrooms)),replace = FALSE)
# training set
mushrooms_train <- mushrooms[train,]
# test set
mushrooms_test <- mushrooms[-train,]

## Classification Tree


# panalty matrix: will set a penalty which is 10 times bigger for classifying a
# poissoness mushroom as edible than classifying an edible mushroom as 
# poisonness
#penalty.matrix <- matrix(c(0,1,10,0), byrow=TRUE, nrow=2)
# building the classification tree with rpart
tree <- rpart(yearlyincome~.,
              data=mushrooms_train,
              #parms = list(loss = penalty.matrix),
              method = "class")

## Prunning the tree using the best complexity parameter

# choosing the best complexity parameter "cp" to prune the tree
#cp.optim <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
# tree prunning using the best complexity parameter. For more in
#tree <- prune(tree, cp=cp.optim)

## Visualizing the final tree

# plotting the tree with the rpart.plot package
prp(tree,faclen = 0, cex = 0.8, extra = 1)

## Predictions on test set

pred <- predict(object=tree,mushrooms_test,type="class")
t <- table(mushrooms_test$yearlyincome,pred)
confusionMatrix(t)
