library(FNN, quietly = TRUE)

knn_1 <- function(mat, k){
  n <- nrow(mat)
  if (n <= k) stop("k can not be more than n-1")
  neigh <- matrix(0, nrow = n, ncol = k)
  for(i in 1:n) {
    euc.dist <- colSums((mat[i, ] - t(mat)) ^ 2)  
    neigh[i, ] <- order(euc.dist)[2:(k + 1)]
  }
  return(neigh)
}


train <- read.csv("C:/GIT/private/MOW/adult.csv", header=TRUE);
test <- read.csv("C:/GIT/private/MOW/adult_test.csv", header=TRUE);

labels <- train[,1];
train <- train[,-1];

test <- test[,-1];


res <- knn_1(train, 4)

print(res)
