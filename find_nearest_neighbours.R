getData<-function(){
  mushrooms <- read.csv("mushrooms.csv", stringsAsFactors = TRUE);
}



getNearestNeighbours<-function(data, example, k){
  # first column is class
  dataWithoutClass = data[,-1]
  
  distVector = apply(dataWithoutClass,1,calculateDistanceDiscrete, b=example)
  sorted = sort(distVector, decreasing = FALSE, index.return = TRUE)
  indexes = sorted$ix
  indexes = indexes[1:k]
  data[indexes,]
}

calculateDistanceDiscrete<-function(a, b){
  distMatrix = ifelse(a == b, 0, 1)
  sum(distMatrix)
}

data = getData()
example = data[1:1,-1]
data = data[-1,]
getNearestNeighbours(data, example, 2000)

