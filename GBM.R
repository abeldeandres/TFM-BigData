library(rpart)
library(caret)
library(RColorBrewer)
library(randomForest)
library(ROCR)
library(caTools)
library(gbm)
library(neuralnet)

rm(list = ls())

set.seed(42)
final <- read.csv("C:/Users/Abel de Andrés Gómez/OneDrive/TFM/TraumaticData.csv",na.strings=c("","NA"))

summary(final)
#Max-Min Normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

final <- as.data.frame(lapply(final, normalize))
summary(final)


final$outcome<-as.factor(ifelse(final$outcome == "1", "F", "V"))
#final$outcome<-class.ind(final$outcome)
ind<-sample.split(Y=final$outcome,SplitRatio =0.7)
train <- final[ind,]
test <- final[!ind,]



trainGBM<-train
trainGBM$outcome<-as.numeric(trainGBM$outcome)
trainGBM = transform(trainGBM, outcome=outcome-1)
set.seed(42)
model.gbm = gbm(outcome ~ ., data = trainGBM, n.minobsinnode=10,interaction.depth = 1,shrinkage=0.1, distribution = 'bernoulli', cv.folds=10, n.trees=100, verbose=F)

best.iter = gbm.perf(model.gbm, method="cv")
best.iter
summary(model.gbm)

mPred = predict(model.gbm, test)
out_predGBOOST<-as.factor(ifelse(mPred > 0.5, 1, 0))
levels(out_predGBOOST)=c("F","V")
confusionMatrixGBM<-confusionMatrix(data=out_predGBOOST,reference=test$outcome,positive="F")
confusionMatrixGBM




set.seed(42)
fitControl = trainControl(method="cv", number=10)
metric <- "Accuracy"

model2 = train(outcome~., data=train, method="gbm",distribution="bernoulli",metric=metric, trControl=fitControl, verbose=F )

print(model2)
plot(model2)

mPred = predict(model2, test, na.action = na.pass)

confusionMatrixGBM<-confusionMatrix(data=mPred,reference=test$outcome,positive="F")
confusionMatrixGBM
fourfoldplot(confusionMatrixGBM$table)