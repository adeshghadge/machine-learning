datapath <- "/Users/adeshghadge/Box Sync/MScA/Courses/Machine Learning & Predictive Analytics/Project"

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

ks.test(unique(Y), "pnorm", mean(Y), sd(Y))

data.model <- dat[, -1]
head(data.model)

#Linear Model
lin.model <- lm(reference ~ ., data = data.model)
lin.model.sm <- summary(lin.model)

lin.model.cols <- rbind(cbind(rownames(lin.model.sm$coefficients[which(lin.model.sm$coefficients[,4] <= 0.05), ][-1,])), "reference")

data.model.2 <- data.model[, lin.model.cols]

lin.model.2 <- lm(reference ~ ., data = data.model.2)
lin.model.sm.2 <- summary(lin.model.2)

vect.lm.chars <- c(AIC = AIC(lin.model.2), R.Sq = lin.model.sm.2$r.squared, MSE = mean(lin.model.sm.2$residuals^2), num.pred = ncol(data.model.2) - 1)

mapply(format, vect.lm.chars)





#PCA
suppressWarnings(library(relaimpo))
suppressWarnings(library(Rfast))

pred.data <- data.model[,1:ncol(data.model)-1]

PCA.data <- prcomp(pred.data)

summary(PCA.data)$importance

plot(summary(PCA.data)$importance[3,],type="h",main="Relative Importance of Factors",
     xlab="Number of Factors",ylab="Explained Variance (%)")

factorLoadings<-PCA.data$rotation
factorScores<- as.matrix(pred.data)%*%PCA.data$rotation
zeroLoading<-PCA.data$center

eps <- 1e-6
cols.to.use <- which(colVars(factorScores) > eps)

factorsData<-data.frame(Y=data.model$reference,factorScores[, cols.to.use])
dim(factorsData)


model.PCA <- lm(Y~.,data=factorsData)
model.PCA.sm <- summary(model.PCA)

vect.chars.pca <- c(AIC = AIC(model.PCA), R.Sq = model.PCA.sm$r.squared, MSE = mean(model.PCA.sm$residuals^2), num.pred = ncol(factorScores))

mapply(format, vect.chars.pca)

metrics.PCA <- calc.relimp(model.PCA, type = c("first"))

sum(metrics.PCA@first)
metrics.PCA.rank <- (metrics.PCA@first.rank)

orderedFactors<- data.frame(Y=data.model$reference, factorScores[,order(metrics.PCA.rank)])
dim(orderedFactors)

model.orderd.PCA <- lm(Y~.,data=orderedFactors)
model.orderd.PCA.sm <- summary(model.orderd.PCA)

original.rsq<-sapply(2:ncol(factorsData),function(z) summary(lm(Y~.,data=factorsData[,1:z]))$r.squared)
ordered.rsq<-sapply(2:ncol(orderedFactors),function(z) summary(lm(Y~.,data=orderedFactors[,1:z]))$r.squared)

plot(ordered.rsq,type="l",xlab="Number of Ordered PCA Factors")

matplot(2:ncol(factorsData),cbind(original.rsq,ordered.rsq),type="l",lty=1,lwd=2,col=c("black","red"),
        main="Improvement of Fit with Number of Predictors",
        xlab="Number of Predictors",ylab="Determination Coefficient")
legend("bottomright",legend=c("Original","Improved"),lty=1,lwd=2,col=c("black","red"))


vect.chars.ordered.pca <- c(AIC = AIC(model.orderd.PCA), R.Sq = model.orderd.PCA.sm$r.squared, MSE = mean(model.orderd.PCA.sm$residuals^2), num.pred = ncol(orderedFactors) - 1)

mapply(format, vect.chars.ordered.pca)






#Lasso
suppressWarnings(library(glmnet))

data.model.mx <- as.matrix(data.model)

model.cv.lasso <- cv.glmnet(x=data.model.mx[,1:ncol(data.model.mx)-1],y=data.model.mx[,ncol(data.model.mx)], alpha = 1)
plot(model.cv.lasso)

(best.lamda = model.cv.lasso$lambda.min)

model.improved.lasso <- glmnet(x=data.model.mx[,1:ncol(data.model.mx)-1],y=data.model.mx[,ncol(data.model.mx)], lambda = best.lamda, alpha = 1, standardize = F)
plot(model.improved.lasso)


lasso.pred=predict(model.improved.lasso,s=best.lamda,
                   newx=data.model.mx[,1:ncol(data.model.mx)-1])

lasso.betas.cols <- names(model.improved.lasso$beta[(which(!(model.improved.lasso$beta == 0))),])

vect.lasso.chars <- c("DevRatio (RSq)" = model.improved.lasso$dev.ratio, MSE = mean((lasso.pred - data.model.mx[,ncol(data.model.mx)])^2), num.pred = length(lasso.betas.cols))
mapply(format, vect.lasso.chars)



lin.model.lasso <- lm(reference ~ ., data = data.model[,c("reference", lasso.betas.cols)])
lasso.model.sm <- summary(lin.model.lasso)

lin.model.cols.lasso <- rbind(cbind(rownames(lasso.model.sm$coefficients[which(lasso.model.sm$coefficients[,4] <= 0.05), ][-1,])), "reference")

data.model.lasso <- data.model[, lin.model.cols.lasso]

lin.model.lasso.2 <- lm(reference ~ ., data = data.model.lasso)
lasso.model.sm.2 <- summary(lin.model.lasso.2)
lasso.model.sm.2

vect.lasso.chars.2 <- c(AIC = AIC(lin.model.lasso.2), R.Sq = lasso.model.sm.2$r.squared, MSE = mean(lasso.model.sm.2$residuals^2), num.pred = ncol(data.model.lasso) - 1)

mapply(format, vect.lasso.chars.2)





#Regression Tree
suppressWarnings(library(rpart))
suppressWarnings(library(rpart.plot))
suppressWarnings(library(caret))

ctrl <- trainControl(method = "cv", number = 10)

treeTrain <- train(reference~., method="rpart", data=data.model, tuneLength = 20, trControl = ctrl)
treeTrain$results

treeTrain$bestTune
treeTrain$finalModel

#Using rpart package
prp(treeTrain$finalModel,extra=101, # display the number of observations that fall in the node
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


(treeTrain.MSE<-sum(residuals(treeTrain$finalModel)^2))

vect.caret.tree.chars <- c(R.Sq = treeTrain$results[which(treeTrain$results$cp == treeTrain$bestTune),]$Rsquared, MSE = sum(residuals(treeTrain$finalModel)^2), num.pred = length(predictors(treeTrain)))

mapply(format, vect.caret.tree.chars)

#Linear Model
mapply(format, vect.lm.chars)

#PCA Model
mapply(format, vect.chars.ordered.pca)

#Lasso Model
mapply(format, vect.lasso.chars)

#Regression Tree Model
mapply(format, vect.caret.tree.chars)


rbind(mapply(format, vect.lm.chars), mapply(format, vect.caret.tree.chars))
mapply(format, vect.chars.ordered.pca)
mapply(format, vect.lasso.chars)
mapply(format, vect.caret.tree.chars)

l <- list("Linear Model" = mapply(format, vect.lm.chars),
          "PCA Model" = mapply(format, vect.chars.ordered.pca),
          "Lasso Model" = mapply(format, vect.lasso.chars),
          "Regression Tree Model" = mapply(format, vect.caret.tree.chars))

do.call(rbind, lapply(l, function(x) x[match(names(l[[1]]), names(x))]))




regTreeFit <- rpart(reference~., data=data.model)

prp(regTreeFit,extra=101, # display the number of observations that fall in the node
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

summary(regTreeFit)

printcp(regTreeFit)
plotcp(regTreeFit)

(regTree.MSE<-sum((data.model$reference-predict(regTreeFit))^2))
par(mfrow=c(1,2))
rsq.rpart(regTreeFit)
par(mfrow=c(1,1))

rsq.val <- 1 - printcp(regTreeFit)[,c(3,4)]
colnames(rsq.val) <- c('apparent rsq', 'x relative rsq')

vect.regtree.chars <- c(R.Sq = max(rsq.val[,1]), MSE = regTree.MSE, num.pred = ncol(data.model) - 1)

mapply(format, vect.regtree.chars)

cbind(regTreeFit$cptable[,3]+regTreeFit$cptable[,5],regTreeFit$cptable[,4])

(best.CP = regTreeFit$cptable[which.min(regTreeFit$cptable[,"xerror"]),"CP"])




prunedTree <- prune(regTreeFit, cp = best.CP)
printcp(prunedTree)
plotcp(prunedTree)

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


plot(predict(prunedTree), jitter(resid(prunedTree)))
temp <- prunedTree$frame[prunedTree$frame$var == '<leaf>',]
axis(3, at = temp$yval, as.character(row.names(temp)))
mtext('leaf number', side = 3, line = 3)
abline(h = 0, lty = 2)


(prunedTree.MSE<-sum((data.model$reference-predict(prunedTree))^2))

par(mfrow=c(1,2))
rsq.rpart(prunedTree)
par(mfrow=c(1,1))

rsq.val.pruned <- 1 - printcp(prunedTree)[,c(3,4)]
colnames(rsq.val.pruned) <- c('apparent rsq', 'x relative rsq')
rsq.val.pruned


vect.pruned.regtree.chars <- c(R.Sq = max(rsq.val.pruned[,1]), MSE = prunedTree.MSE, num.pred = ncol(data.model) - 1)

mapply(format, vect.pruned.regtree.chars)




modelFitControlled <- rpart(reference~., data=data.model, minbucket=30)

prp(modelFitControlled,extra=101, # display the number of observations that fall in the node
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


printcp(modelFitControlled)
plotcp(modelFitControlled)

(modelFitControlled.MSE<-sum((data.model$reference-predict(modelFitControlled))^2))

par(mfrow=c(1,2))
rsq.rpart(prunedTree)
par(mfrow=c(1,1))

rsq.val.controlled <- 1 - printcp(modelFitControlled)[,c(3,4)]
colnames(rsq.val.controlled) <- c('apparent rsq', 'x relative rsq')
rsq.val.controlled

vect.regtree.controlled.chars <- c(R.Sq = max(rsq.val.controlled[,1]), MSE = modelFitControlled.MSE, num.pred = ncol(data.model) - 1)

mapply(format, vect.regtree.controlled.chars)


cbind(modelFitControlled$cptable[,3]+modelFitControlled$cptable[,5],modelFitControlled$cptable[,4])







prunedTreeControlled <- prune(modelFitControlled, cp = modelFitControlled$cptable[7,1])
printcp(prunedTreeControlled)

prp(prunedTreeControlled,extra=101, # display the number of observations that fall in the node
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

plotcp(prunedTreeControlled)
printcp(prunedTreeControlled)


plot(predict(prunedTreeControlled), jitter(resid(prunedTreeControlled)))
temp <- prunedTreeControlled$frame[prunedTreeControlled$frame$var == '<leaf>',]
axis(3, at = temp$yval, as.character(row.names(temp)))
mtext('leaf number', side = 3, line = 3)
abline(h = 0, lty = 2)


(prunedTreeControlled.MSE<-sum((data.model$reference-predict(prunedTreeControlled))^2))

par(mfrow=c(1,2))
rsq.rpart(prunedTreeControlled)
par(mfrow=c(1,1))

rsq.val.pruned.controlled <- 1 - printcp(prunedTreeControlled)[,c(3,4)]
colnames(rsq.val.pruned.controlled) <- c('apparent rsq', 'x relative rsq')
rsq.val.pruned.controlled
