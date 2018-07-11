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


set.seed(42)
#model.RF=randomForest(outcome~.,data=train,ntree=500)
#print(model.RF)

# train model
control <- trainControl(method="cv", number=10)
#tunegrid <- expand.grid(.mtry=c(1:15), .ntree=c(1:2500))
tunegrid <-  expand.grid(
  mtry = seq(from = 1, to = 15, by = 1)
)
metric <- "Accuracy"
custom <- caret::train(outcome~., data=train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
plot(custom)
print(custom)