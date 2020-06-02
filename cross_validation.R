mushrooms <- read.csv("mushrooms.csv", stringsAsFactors = TRUE);


#Randomly shuffle the data
mushrooms<-mushrooms[sample(nrow(mushrooms)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(mushrooms)),breaks=10,labels=FALSE)

#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- mushrooms[testIndexes, ]
  write.csv(testData, paste("mushroom_test_", toString(i), ".csv"));
  trainData <- mushrooms[-testIndexes, ]
  write.csv(trainData, paste("mushroom_train_", toString(i), ".csv"));
    #Use the test and train data partitions however you desire...
}