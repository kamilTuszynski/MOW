getData<-function(){
  mushrooms <- read.csv("mushrooms.csv", stringsAsFactors = TRUE);
}



getNearestNeighbours<-function(data, example, k){
  # first column is class
  dataWithoutClass = data[,-1]
  exampleWithoutClass = example[-1]
  getMaxValuesInColumns(dataWithoutClass)
  
  distVector = apply(dataWithoutClass,1,calculateDistanceDiscrete, b=exampleWithoutClass)
  sorted = sort(distVector, decreasing = FALSE, index.return = TRUE)
  indexes = sorted$ix
  indexes = indexes[1:k]
  data[indexes,]
}

maxVector<-NULL

calculateDistanceDiscrete<-function(a, b){
  distVector = ifelse(a == b, 0, 1)
  sum(distVector)
}

getMaxValuesInColumns<-function(dataFrame){
  maxVector <<- integer(length(head(dataFrame,1)))
  for(x in 1:length(maxVector)){
    maxVector[x] = 0;
  }
  for (row in dataFrame) {
    for(i in 1:length(row)){
      numericValue<- as.numeric(row[i]);
      if(!is.na(numericValue)){
        if(TRUE && !is.na(maxVector[i]<numericValue) && maxVector[i]<numericValue){
          maxVector[i] = numericValue
        }
      }
      else{
        maxVector[i]=1
      }
    }
  }
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

calculateDistance<-function(a, b){
  distMatrix  <- integer(length(maxVector));
  for(i in 1:length(maxVector)){
    numericValueA <- as.numeric(a[i]);
    numericValueB <- as.numeric(b[i]);
    if(TRUE && !is.na(!is.na(numericValueA) && !is.na(numericValueB)) && !is.na(numericValueA) && !is.na(numericValueB)){
      denominator<-1
      if(maxVector[i]!=0){
        denominator = maxVector[i]
      }
      distMatrix[i] = sqrt(numericValueA^2 +numericValueB^2)/denominator
    }
    else{
      if(!is.na(!is.null(a[i]) && !is.null(b[i]) && a[i]==b[i]) && !is.null(a[i]) && !is.null(b[i]) && a[i]==b[i]){
        distMatrix[i] = 0;
      }
      else{
        distMatrix[i] = 1;
      }
    }
  }
  sum(distMatrix)
}

data = getData()
example = data[1:1,-1]
data = data[-1,]
getNearestNeighbours(data, example, 2000)