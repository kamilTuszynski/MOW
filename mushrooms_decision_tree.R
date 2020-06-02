library(rpart, quietly = TRUE)

#mushrooms <- read.csv("mushrooms.csv");
data_to_classification = read.table("mushrooms.csv",sep=",",header=TRUE)

mytree <- rpart(
  as.factor(class) ~ .,
  data = data_to_classification
)

plot(mytree)