
library(randomForest)
library(xgboost)
library(NMOF)
library(dummies)
datapath="C:/Users/Kush/Desktop/DataAnalytics/MachineLearning/Project"
Data<-read.csv(file=paste(datapath,"train_sample.csv",sep="/"))
dat.test<-read.csv(file=paste(datapath,"test_sample.csv",sep="/"))
head(Data$target,100)
dat.test=data.frame(dat.test)

set.seed(1)
testInd = sample(nrow(Data), nrow(Data)/3)
xTrain = Data[-testInd,-1]
xTest = Data[testInd,-1]

yTrain = as.factor(Data$target[-testInd])
yTest = as.factor(Data$target[testInd])

xgbTrain = data.matrix(xTrain[,-ncol(xTrain)])
xgbTest = data.matrix(xTest[,-ncol(xTest)])


yTrain = as.integer(yTrain)-1
yTest = as.integer(yTest)-1
table(yTrain)
table(yTest)

xgbTrain=apply(as.matrix(xgbTrain), 2, as.numeric)
yTrain=apply(as.matrix(yTrain), 2, as.numeric)
xgbTest=apply(as.matrix(xgbTest), 2, as.numeric)
yTest=apply(as.matrix(yTest), 2, as.numeric)

numClasses = max(yTrain)+1
param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "max_depth" = 5,
              "gamma" = 1,
              "eta" = .1,
              "num_class" = numClasses,
              "subsample"=.5,
              "nthread" = 16)
set.seed(13)
cv.nround <- 600
cv.nfold <- 3

#, max_depth=3,eta=.001
#(bst.cv = xgb.cv(param=param, data = xgbTrain, label = yTrain, 
                # nfold = cv.nfold,max_depth=10, eta=.01, nrounds = cv.nround,verbose=F))
bst = xgboost(param=param, data = xgbTrain,label = yTrain, 
              nrounds=cv.nround)

xgbPred <- matrix(predict(bst, newdata = xgbTest), ncol = numClasses, byrow = TRUE)
xgbPred=apply(as.matrix(xgbPred), 2, as.numeric)


gb_target_IndMat<-dummy.data.frame(data=as.data.frame(yTest), 
                                   sep="_", verbose=F, 
                                   dummy.class="ALL")

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
MultiLogLoss(gb_target_IndMat,xgbPred)


dat.test1=data.matrix(dat.test)
dat.test1=apply(as.matrix(dat.test), 2, as.numeric)

xgbPred1 <- matrix(predict(bst, newdata = dat.test1), ncol = numClasses, byrow = TRUE)
submission=cbind(dat.test$id,xgbPred1)
colnames(submission)=c("id","Target_0","Target_1","Target_2","Target_3","Target_4","Target_5","Target_6","Target_7","Target_8")
head(submission)
write.csv(submission,"submission.csv",row.names=FALSE)

#########
folds <- 3
eval_metric = list("mlogloss")  # evaluation metric for validation

# Parameters grid to search
eta = c(.3,.15,0.05)
max_depth = c(4,5,6)
nrounds <- c(seq(from=30,to=90,by=10),26)
numClasses = max(yTrain) + 1
Inputs=cbind(eta,max_depth,nrounds)
# Table to track performance from each worker node
res <- data.frame(Value=numeric(),Eta=numeric(),Max_Depth=numeric(),Nrounds=numeric())

xgbCV <- function (Inputs) {
  myEta<-Inputs$eta
  myMax_depth<-Inputs$max_depth
  myNRounds<-Inputs$n_Rounds
  set.seed(0)
  fit <- xgb.cv(
    params =list(eta=myEta,max_depth=myMax_depth),
    num_class = numClasses,
    data = xgbTrain,
    metrics=eval_metric,
    objective = "multi:softprob",
    label = yTrain, 
    nfold = folds, 
    nrounds = myNRounds,
    verbose=F
  )
  mybestNR = which.min(fit$evaluation_log$test_rmse_mean)
  val <- fit$evaluation_log$test_rmse_mean[mybestNR]
  res <<- rbind(res,c(val,myEta,myMax_depth,mybestNR))
  
  return(val)
}

library(NMOF)

sol <- gridSearch(
  fun = xgbCV,
  levels = list(eta=eta,max_depth=max_depth,n_Rounds=nrounds),
  method = 'loop',
  keepNames = TRUE,
  asList = TRUE
)


dat.test=data.matrix(dat.test)
dat.test1=apply(as.matrix(dat.test), 2, as.numeric)
yTest=apply(as.matrix(yTest), 2, as.numeric)
#submission=cbind(dat.test$id,xgbPred)
#colnames(submission)=c("id","Target_0","Target_1","Target_2","Target_3","Target_4","Target_5","Target_6","Target_7","Target_8")
#head(submission)
#write.csv(submission,"submission.csv",row.names=FALSE)

