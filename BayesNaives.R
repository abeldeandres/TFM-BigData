library(rpart)
library(caret)
library(RColorBrewer)
library(randomForest)
library(ROCR)
library(caTools)
library(gbm)
library(neuralnet)
library(ada)
library(rpart)
library(klaR)

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

ind<-sample.split(Y=final$outcome,SplitRatio =0.7)
train <- final[ind,]
test <- final[!ind,]

set.seed(42)
nnetGrid <-  expand.grid(
  usekernel = FALSE, #c(TRUE,FALSE),
  fL = 0, #seq(from = 0, to = 10, by = 1),
  adjust = 0 #seq(from = 0, to = 10, by = 1)
)

trainControl <- trainControl(method="cv", number=10)
model.nb <- caret::train(outcome~., data=train, method="nb", 
                                           trControl=trainControl,tuneGrid = nnetGrid
)


pred.NB<-predict(model.nb,test)

confusionMatrixNaive<-caret::confusionMatrix(data = pred.NB, reference = test$outcome,positive="F")
