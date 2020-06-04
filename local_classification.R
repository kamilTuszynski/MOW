getData<-function(){
  mushrooms <- read.csv("mushrooms.csv");
}


# function that returns k nearest neighbours for example among the data
getNearestNeighbours<-function(data, example, k, classIndex = 1){
  
  dataWithoutClass = data[,-classIndex]
  exampleWithoutClass = example[-classIndex]
  
  maxVector = colMax(dataWithoutClass)
  minVector = colMin(dataWithoutClass)
  
  
  distVector = apply(dataWithoutClass,1,calculateDistanceForAll, b=exampleWithoutClass, max = maxVector, min = minVector)
  
  sorted = sort(distVector, decreasing = FALSE, index.return = TRUE)
  indexes = sorted$ix
  indexes = indexes[1:k]
  data[indexes,]
}

# function that returns max value for each column
colMax <- function(data) sapply(data, getMaxIfNumeric)

# function that returns min value for each column
colMin <- function(data) sapply(data, getMinIfNumeric)

getMaxIfNumeric <- function(data){
  if(is.numeric(data)){
    max(data, na.rm = TRUE)
  }
  else{0}
}

getMinIfNumeric <- function(data){
  if(is.numeric(data)){
    min(data, na.rm = TRUE)
  }
  else{0}
}

calculateDistanceForAll<-function(a, b, max, min){
  distVector = ifelse(sapply(a,is.numeric), mapply(calculateDistanceNumeric, a, b, max, min), ifelse(a == b, 0, 1))
  
  sum(distVector)
}


calculateDistanceNumeric <- function(a, b, max, min){
  if(is.numeric(a)){
    # think about calculating this
    abs(a-b)/(max -min) * 3
  }
  else{
    0
  }
}


localClassification<-function(example, trainData, k, classIndex = 1, algorithm = "DecisionTree", controlTree = rpart.control()){
  
  nn = getNearestNeighbours(trainData, example, k, classIndex)
  
  if(algorithm == "DecisionTree")
  {
    # building the classification tree with rpart
    tree <- rpart(class~.,
                  data=nn,
                  control = controlTree,
                  #parms = list(loss = penalty.matrix),
                  method = "class")
    predict(object=tree,example,type="class")
  }
  else if(algorithm == "NaiveBayes")
  {
    nb.Model         <- naiveBayes( class~., data = nn )
    nb.Prediction    <- predict( nb.Model, newdata = example, type = "raw" )
  }
}

# TODO: usun przed wyslaniem projektu
# data = getData()
# example = data[1:1,]
# data = data[-1,]
# 
# start.time <- Sys.time()
# getNearestNeighbours(data, example, 2000)
# end.time <- Sys.time()
# 
# time.taken <- end.time - start.time
# time.taken