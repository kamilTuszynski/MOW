getData<-function(){
  mushrooms <- read.csv("adult.csv");
}



getNearestNeighbours<-function(data, example, k){
  # first column is class
  dataWithoutClass = data[,-1]
  exampleWithoutClass = example[-1]
  
  maxVector = colMax(dataWithoutClass)
  minVector = colMin(dataWithoutClass)
  
  
  distVector = apply(dataWithoutClass,1,calculateDistanceForAll, b=exampleWithoutClass, max = maxVector, min = minVector)
  
  sorted = sort(distVector, decreasing = FALSE, index.return = TRUE)
  indexes = sorted$ix
  indexes = indexes[1:k]
  data[indexes,]
}


colMax <- function(data) sapply(data, getMaxIfNumeric)

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
example = data[1:1,]
data = data[-1,]

start.time <- Sys.time()
getNearestNeighbours(data, example, 2000)
end.time <- Sys.time()

time.taken <- end.time - start.time
time.taken