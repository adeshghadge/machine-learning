datapath <- "/Users/adeshghadge/Box Sync/MScA/Courses/Machine Learning & Predictive Analytics/Project/supervized_classification_data"

otto.test.data <- read.csv(file=paste(datapath,"test_sample.csv",sep="/"), header=TRUE,
                           stringsAsFactors = TRUE)
cbind(colnames(otto.test.data))
head(otto.test.data)

otto.train.data <- read.csv(file=paste(datapath,"train_sample.csv",sep="/"), header=TRUE,
                            stringsAsFactors = TRUE)
cbind(colnames(otto.train.data))
head(otto.train.data)

head(otto.train.data$target)

log(otto.train.data)

model1 <- glm(target ~ ., data=otto.train.data, family=binomial(link=logit))
summary(model1)


pred.model.1 <- predict(object = model1, type="response")
head(pred.model.1)


# Random Forest
suppressWarnings(library(randomForest))

otto.train.data$target <- as.factor(otto.train.data$target)
otto.train.data[1:50,]$target

model2 <- randomForest(target~.,ntree=250, data=otto.train.data,importance=TRUE)
print(model2)

importance(model2)

varImpPlot(model2, main="Variable Importance")

plot(model2)

replace(otto.test.data, TRUE, lapply(otto.test.data, factor))

apply(otto.test.data, 2, function(x) factor(x))

rfPred <- predict(model2, new = otto.test.data[, -1], type="prob")

MultiLogLoss <- function(act, pred)
{
  eps = 1e-15;
  nr <- nrow(pred)
  pred = matrix(sapply( pred, function(x) max(eps,x)), nrow = nr)      
  pred = matrix(sapply( pred, function(x) min(1-eps,x)), nrow = nr)
  #normalize rows
  ll = sum(act*log(sweep(pred, 1, rowSums(pred), FUN="/")))
  ll = -ll/nrow(act)
  return(ll);
}

suppressWarnings(library(dummies))

otto.test.mat <- dummy.data.frame(data=as.data.frame(otto.test.data), 
                 sep="_", verbose=F, 
                 dummy.class="ALL")

head(otto.test.mat[,1:10])

(rfLogLoss = MultiLogLoss(otto.test.mat, rfPred))



paste('class_', otto.train.data$target, sep='')

#Neural Networks
head(otto.train.data$target)
trainX = otto.train.data[,-c(1,ncol(otto.train.data))]
trainY = as.factor(paste('class_', otto.train.data$target, sep=''))
testX = otto.test.data
testY = as.factor(otto.test.data$target)

ctrl <- trainControl(method = "cv", number = 5, classProbs =  TRUE)

nnFitOtto <- train(trainX, trainY, method = "nnet", trControl = ctrl,
                   tuneGrid = expand.grid(.size=3:6,.decay=.3),
                   maxit = 1000, trace = FALSE,
                   preProc = c("center", "scale"))

print(nnFitOtto)

plot(nnFitOtto)

MultiLogLoss.2 <- function(act, pred)
{
  eps = 1e-15;
  pred[pred<eps] = eps      
  pred[pred> (1-eps)] = 1-eps
  #normalize rows
  ll = sum(act*log(sweep(pred, 1, rowSums(pred), FUN="/")))
  ll = -ll/nrow(act)
  return(ll);
}

actMatrix = model.matrix(~-1+trainY)
print(MultiLogLoss.2(actMatrix,
                   predict(nnFitOtto,trainX,type='prob')))
