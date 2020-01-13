datapath="C:/Users/Kush/Desktop/DataAnalytics/MachineLearning/Project"
dat<-read.csv(file=paste(datapath,"slice_localization_data.csv",sep="/"))
head(colnames(dat))
tail(colnames(dat))
pred<-dat[,c(-1,-386)]
Y<-dat[,386]
head(colnames(pred))
tail(colnames(pred))
plot(Y)
summary(Y)
hist(Y)
qqnorm(Y)
qqline(Y)
head(dat)

completeModelDataFrame<-data.frame(Y,pred)
lm.model=lm(Y~.,data = completeModelDataFrame)
summary(lm.model)
pred.count.lm=ncol(pred)

#Removing insignificant variables##
good.predictors=data.frame((summary(lm.model)$coefficients[,4]<=.05))
good.predictors=subset(good.predictors,good.predictors[1]=="TRUE")
rows=row.names(good.predictors)
rows=rows[-1]
columns=which(colnames(pred) %in% rows )
pred.goodpredictors=pred[,c(columns)]
head(pred.goodpredictors)
dim(pred.goodpredictors)


#model with just significant
completeModelDataFrame1<-data.frame(Y,pred.goodpredictors)
lm.model.gpred=lm(Y~.,data = completeModelDataFrame1)
summary(lm.model.gpred)
pred.count.gpred.lm=ncol(pred.goodpredictors)

mse.lm.model=mean(lm.model$residuals^2)
mse.lm.model.gpred=mean(lm.model.gpred$residuals^2)
rsq.lm.model=summary(lm.model)$r.squared
rsq.lm.model.gpred=summary(lm.model.gpred)$r.squared

mse=c(mse.lm.model,mse.lm.model.gpred)
rsq=c(rsq.lm.model,rsq.lm.model.gpred)
aic=c(AIC(lm.model),AIC(lm.model.gpred))
pred.count=c(pred.count.lm,pred.count.gpred.lm)
results.lm=cbind.data.frame(mse,rsq,aic,pred.count)
row.names(results.lm)[1]="fullModel"
row.names(results.lm)[2]="Good.Predictor.Model"
results.lm


###PCR##
xPCA<-prcomp(pred)

factorLoadings<-xPCA$rotation
factorScores <- xPCA$x
zeroLoading<-xPCA$center

##Removing last few columns to run calc.relimp##
factorLoadings=factorLoadings[, -359:-384]
factorScores=factorScores[, -359:-384]

factors491Data<-data.frame(Y,factorScores)
m491.PCA<-lm(Y~.,data=factors491Data)


library(relaimpo)

metrics.first.PCA <- calc.relimp(m491.PCA, type = "first")
sumMetrics.first=sum(metrics.first.PCA@first)
metrics.first.PCA.rank<-metrics.first.PCA@first.rank
orderedFactors<-factorScores[,order(metrics.first.PCA.rank)]
orderedPCAData<-data.frame(Y,orderedFactors)
head(orderedPCAData[,1:7])
orderR2<-sapply(2:333,function(z) summary(lm(Y~.,data=orderedPCAData[,1:z]))$r.squared)
plot(orderR2,type="l",xlab="Number of Ordered PCA Factors")


##243 factors give a close enough rsquared##
lm.orderedpca=lm(Y~.,data=orderedPCAData[,1:243])
summary(lm.orderedpca)
pca.aic=AIC(lm.orderedpca)
pca.mse=mean(lm.orderedpca$residuals^2)
pca.rsq=summary(lm.orderedpca)$r.squared
pred.count.pca=243

results.pca=cbind(pca.aic,pca.mse,pca.rsq,pred.count.pca)
row.names(results.pca)[1]="PCA"
results.pca
##LASSO##
suppressWarnings(library(glmnet))
set.seed(1) 
train=sample (1:nrow(pred), nrow(pred)/2)
head(train)
test=(-train)

Y.lasso=data.frame(Y)


dim(Y.lasso)
lassoSample=glmnet(x=data.matrix(pred[train,]),y=Y.lasso[train,],alpha=1,nlambda=100,lambda.min.ratio=.0001,standardize = F)
cv.out=cv.glmnet(x=data.matrix(pred[train,]),y=Y.lasso[train,],alpha=1)
plot(cv.out)

bestlam =cv.out$lambda.min
summary(cv.out)
lasso.coef=predict(lassoSample,s=bestlam, newx = data.matrix(pred[test,]))
lasso.fulldata=glmnet(as.matrix(pred),as.matrix(Y),alpha=1,lambda=bestlam, lambda.min.ratio=.0001)

lasso.pred=predict(lasso.fulldata,s=bestlam,newx = data.matrix(pred[test,]))
lasso.coef=predict(lasso.fulldata,type="coefficients",s=bestlam)


data.coeff=data.frame(as.table(as.matrix(lasso.coef)))

zero.counts=data.coeff[data.coeff$Freq==0,]
pred.count.lasso=ncol(pred)-nrow(zero.counts)



#Lasso Fit statistics##
lasso.fulldata
lasso.MSE<-mean((lasso.pred -Y.lasso[test,])^2)

##Lasso Coefficient Compare#
coef.compare=cbind(lasso=lasso.coef,lm=as.vector(lm.model$coefficients),PCA=lm.orderedpca$coefficients)





###Regression Tree Method##
library(caret)
suppressWarnings(library(rpart))
suppressWarnings(library(rpart.plot))
suppressWarnings(library(ISLR))
suppressWarnings(library(knitr))
suppressWarnings(library(caret))
suppressWarnings(library(MASS))


set.seed(1)
head(completeModelDataFrame)

ctrl <- trainControl(method = "cv", number = 10)

tree.slice <- train(Y~., data=completeModelDataFrame,
                    method = 'rpart', trControl = ctrl)

tree.slice
#tree.test=rpart(Y~.,data=completeModelDataFrame)
#tree.test$cptable

#prunedTree <- prune(tree.test, cp = tree.test$cptable[13,1])
#printcp(prunedTree)
#plotcp(prunedTree)
#(best.CP = tree.test$cptable[which.min(tree.test$cptable[,"xerror"]),"CP"])
#prunedTree <- prune(tree.test, cp = best.CP)
#printcp(prunedTree)

prp(prunedTree,extra=101, # display the number of observations that fall in the node
    branch=.5, # change angle of branch lines
    shadow.col="gray", # shadows under the leaves
    branch.lty=3, # draw branches using dotted lines
    split.cex=1.2, # make the split text larger than the node text
    split.prefix="is ", # put "is " before split text
    split.suffix="?", # put "?" after split text
    split.box.col="lightgray", # lightgray split boxes (default is white)
    split.border.col="darkgray", # darkgray border on split boxes
    split.round=.5,
    nn=TRUE) # display the node numbers, default is FALSE
#(prunedTree.MSE<-sum((completeModelDataFrame$Y-predict(prunedTree))^2))

prp(tree.slice$finalModel,extra=101, 
   branch=.1,
   shadow.col="blue", 
   branch.lty=3, 
  split.cex=1, 
    split.prefix="is ", 
    split.suffix="?", 
   split.box.col="lightblue",
    split.border.col="darkblue", 
    split.round=.5,
    nn=TRUE)

tree.slice$bestTune
tree.slice$results
tree.slice$finalModel
tree.slice$bestTune
tree.slice
##rsquared value for best cp#
tree.slice.Rsquared=0.5659641
##Number of predictors used for terminal nodes#
tree.slice.pred=2

Tree.MSE<-mean(residuals(tree.slice$finalModel)^2)

Tree.results=cbind.data.frame(mse.tree=Tree.MSE,tree.slice.Rsquared,tree.slice.pred)
Tree.results
#2 predictors used for splits, Value 18 and Value 379#

##Model Evaluation##
MSE.allmodels= cbind(mse.lm.model,mse.lm.model.gpred,pca.mse,lasso.MSE,Tree.MSE)
Predictors=cbind(lm=pred.count.lm,lm.gpred=pred.count.gpred.lm,pca=results.pca$pred.count,lasso=pred.count.lasso,tree.slice.pred)
r2.allmodels=cbind(lm=rsq.lm.model,lm.gpred=rsq.lm.model.gpred,pca=results.pca$pca.rsq,tree.slice.Rsquared)

coef.compare=cbind(lasso=lasso.coef,lm=as.vector(lm.model$coefficients),PCA=lm.orderedpca$coefficients) 
head(coef.compare)

tree.slice$finalModel
MSE.allmodels
Predictors
r2.allmodels

#The PCA model based on # of predictors and MSE is the best model for analysis#