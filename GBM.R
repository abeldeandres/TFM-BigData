library(rpart)
library(caret)
library(RColorBrewer)
library(randomForest)
library(ROCR)
library(caTools)
library(gbm)
library(neuralnet)
library(pROC)

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



#For a gradient boosting machine (GBM) model, there are three main tuning parameters:
#number of iterations, i.e. trees, (called n.trees in the gbm function)
#complexity of the tree, called interaction.depth
#learning rate: how quickly the algorithm adapts, called shrinkage
#the minimum number of training set samples in a node to commence splitting (n.minobsinnode)

trainGBM<-train
trainGBM$outcome<-as.numeric(trainGBM$outcome)
trainGBM = transform(trainGBM, outcome=outcome-1)
set.seed(42)


nnetGrid <-  expand.grid(
  n.trees = seq(from = 1, to = 200, by = 10),
  interaction.depth = seq(from = 1, to = 5, by = 1),
  shrinkage = seq(from = 0.1, to = 0.4, by = 0.1),
  n.minobsinnode = seq(from = 1, to = 15, by = 1)
)

trainControl <- trainControl(method="cv", number=10)
metric <- "Accuracy"
model.gbm <- train(outcome~., data=train, method="gbm", 
                  trControl=trainControl,metric=metric,tuneGrid = nnetGrid)
print(model.gbm)
plot(model.gbm)


pred.GBM <- predict(model.gbm , test, na.action = na.pass)
confusionMatrixGBM<-caret::confusionMatrix(data=pred.GBM,reference=test$outcome,positive="F")
confusionMatrixGBM


