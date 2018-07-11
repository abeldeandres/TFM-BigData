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


ControlParamteres <- trainControl(method = "cv",
                                  number = 10, #usamos 5 fold cross-validation
                                  savePredictions = TRUE,
                                  classProbs = TRUE #give the probabilities for each class.Not just the class labels
)

parametersGrid <-  expand.grid(
#                                eta = 0.1, 
#                               colsample_bytree=c(0.5,0.7),
#                              max_depth=c(3,6),
#                               nrounds=100,
#                              gamma=1,
#                               min_child_weight=2,
#                               subsample = 1
  eta = seq(from = 0.1, to = 0.5, by = 0.1),
  colsample_bytree = seq(from = 0.4, to = 0.8, by = 0.1),
  #max_depth = seq(from = 1, to = 3, by = 1),
  nrounds = seq(from = 1, to = 100, by = 10),
  gamma = 1,
  min_child_weight = seq(from = 1, to = 4, by = 1),
  subsample = 1

)




modelxgboost <- train(outcome~., 
                      data = train,
                      method = "xgbTree",
                      trControl = ControlParamteres,
                      tuneGrid=parametersGrid
                      )

print(modelxgboost)


pred.XGB<-predict(modelxgboost,test)
#out_predGBOOST<-as.factor(ifelse(predictions > 0.5, 1, 0))
#levels(out_predGBOOST)=c("F","V")
confusionMatrixGBOOST<-confusionMatrix(data = pred.XGB, reference = test$outcome,positive="F")
confusionMatrixGBOOST