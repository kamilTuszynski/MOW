rm(list = ls())

getData<-function(){
  mushrooms <- read.csv("adult.csv");
  mushrooms
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
  
  distVector
  #sum(distVector)
}


calculateDistanceNumeric <- function(a, b, max, min){
  if(is.numeric(a)){
    abs(a-b)/(max -min) * 3
  }
  else{
    0
  }
}

data = getData()
row1 = data[1,]
row2 = data[2,]
max = colMax(data)
min = colMin(data)