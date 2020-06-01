library(e1071,quietly = TRUE)
library(caret,quietly = TRUE)


# reading the dataset as a dataframe
mushrooms <- read.csv("mushrooms.csv");
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

nb.Model         <- naiveBayes( class~., data = mushrooms_train )
nb.Prediction    <- predict( nb.Model, newdata = mushrooms_test[-1], type = "raw" )


## Predictions on test set
predictions    <- ifelse ( nb.Prediction[, 2] > 0.5, 'p', 'e' )
t <- table(mushrooms_test$class, predictions)
confusionMatrix(t)
