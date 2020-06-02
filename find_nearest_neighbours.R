getData<-function(){
  mushrooms <- read.csv("mushrooms.csv", stringsAsFactors = TRUE);
}



getNearestNeighbours<-function(data, example, k){
  # first column is class
  dataWithoutClass = data[,-1]
  exampleWithoutClass = example[-1]
  
  distVector = apply(dataWithoutClass,1,calculateDistanceDiscrete, b=exampleWithoutClass)
  sorted = sort(distVector, decreasing = FALSE, index.return = TRUE)
  indexes = sorted$ix
  indexes = indexes[1:k]
  data[indexes,]
}

calculateDistanceDiscrete<-function(a, b){
  distVector = ifelse(a == b, 0, 1)
  sum(distVector)
}

localClassification<-function(example, trainData){
  
  nn = getNearestNeighbours(trainData, example, 2000)
  
  
  penalty.matrix <- matrix(c(0,1,10,0), byrow=TRUE, nrow=2)
  # building the classification tree with rpart
  tree <- rpart(class~.,
                data=nn,
                parms = list(loss = penalty.matrix),
                method = "class")
  
  cp.optim <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
  # tree prunning using the best complexity parameter. For more in
  tree <- prune(tree, cp=cp.optim)
  
  
  
  predict(object=tree,example,type="class")
}

data = getData()
example = data[1:1,-1]
data = data[-1,]
getNearestNeighbours(data, example, 2000)

