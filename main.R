library(rpart,quietly = TRUE)
library(caret,quietly = TRUE)
library(rpart.plot,quietly = TRUE)

source("find_nearest_neighbours.R")

data = getData()
example = data[1:1,]

train_data = data[1:6000,]
test_data = data[6001:6100,]

fac <- factor()
for(i in 1:nrow(test_data)) {
  row <- test_data[i,]
  local <- localClassification(example, train_data)
  fac = unlist(list(fac,local))
  print(i)
}

#local <- apply(test_data, 1, localClassification, trainData = train_data)


#local <- localClassification(example, train_data)

t <- table(test_data$class,fac)
confusionMatrix(t)