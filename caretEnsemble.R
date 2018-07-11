library(rpart)
library(caret)
library(RColorBrewer)
library(randomForest)
library(pROC)
library(caTools)
library(gbm)
library(neuralnet)
library(ada)
library(rpart)
library(klaR)



#Libraries
library(caret)
library(devtools)
library(caretEnsemble)
library(RSNNS)
library(glmnet)
library(earth)
rm(list = ls())
rm(list = ls(all = TRUE)) 
gc(reset=TRUE)

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

ind<-sample.split(Y=final$outcome,SplitRatio =0.7)
train <- final[ind,]
test <- final[!ind,]




set.seed(42)
trainControl <- trainControl(method="cv", number=10, classProbs=TRUE)

algorithmList <- c("rf","knn")

models <- caretList(outcome~., data=train, trControl=trainControl,methodList=algorithmList,metric="Accuracy")
results <- resamples(models)
summary(results)
dotplot(results)
splom(results)

greedy_ensemble <- caretEnsemble(
  models,
  metric="Accuracy",
  trControl=trainControl
)
summary(greedy_ensemble)
set.seed(42)
stack.glm <- caretStack(models, method="glm", metric="Accuracy", trControl=trainControl)
print(stack.glm)


pred <- predict(stack.glm, newdata=test)
conf<-caret::confusionMatrix(data=pred, reference=test$outcome)
conf


set.seed(42)
stack.gbm <- caretStack(models, method="gbm", metric="Accuracy", trControl=trainControl)
print(stack.gbm)


pred <- predict(stack.gbm, newdata=test)
conf<-caret::confusionMatrix(data=pred, reference=test$outcome)
conf


# set.seed(42)
# 
# greedy_ensemble <- caretEnsemble(
#   models,
#   metric="ROC", 
#   trControl=trainControl
# )
# summary(greedy_ensemble)
# 
#  pred <- predict(greedy_ensemble, newdata=test,type="prob")
# # conf<-caret::confusionMatrix(data=pred, reference=test$outcome)
# # conf
#  
#  out_predGBOOST<-as.factor(ifelse(pred > 0.5, 1, 0))
#  levels(out_predGBOOST)=c("F","V")
#  
#  confusionMatrixGBOOST<-caret::confusionMatrix(data = out_predGBOOST, reference = test$outcome)
#  confusionMatrixGBOOST


set.seed(42)

y <- as.numeric(train[,14])-1
ytest <- as.numeric(test[,14])-1

x <- data.frame(train[,1:13])
xtest <- data.frame(test[,1:13])
# Fit the ensemble model
model <- SuperLearner(y,
                      x,
                      family=binomial(),
                      SL.library=list("SL.nnet",
                                      "SL.knn",
                                      "SL.gbm",
                                      "SL.xgboost",
                                      "SL.glm",
                                      "SL.randomForest"))

# Return the model
model
predictions <- predict.SuperLearner(model, newdata=xtest)
conv.preds <- ifelse(predictions$pred>=0.5,1,0)