numLlvs <- 2

data <- factor(sample(rep(letters[1:numLlvs], 200), 50))
reference <- factor(sample(rep(letters[1:numLlvs], 200), 50))

confusionMatrix(data, reference)