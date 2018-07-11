library(rpart)
library(caret)
library(RColorBrewer)
library(randomForest)
library(ROCR)
library(caTools)
library(gbm)
library(nnet)
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

#Max-Min Normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
trainNorm<-train 
testNorm<-test

trainNorm$outcome<-as.numeric(trainNorm$outcome)
testNorm$outcome<-as.numeric(testNorm$outcome)

trainNorm <- as.data.frame(lapply(trainNorm, normalize))
testNorm <- as.data.frame(lapply(testNorm, normalize))

trainNorm$outcome<-train$outcome
testNorm$outcome<-test$outcome



fitControl <- trainControl(method = "repeatedcv", 
                           number = 10, 
                           repeats = 5, 
                           classProbs = TRUE, 
                           summaryFunction = twoClassSummary)

#nnetGrid <-  expand.grid(size = seq(from = 1, to = 10, by = 1),
                         #decay = seq(from = 0.1, to = 0.5, by = 0.1))
nnetGrid <-  expand.grid(size = 4,decay = 0.5)
nnetFit <- train(outcome ~ ., 
                 data = trainNorm,
                 method = "nnet",
                 metric = "ROC",
                 trControl = fitControl,
                 tuneGrid = nnetGrid,
                 verbose = FALSE)
ps <- predict(nnetFit, testNorm)

confusionMatrixNET<-confusionMatrix(data=ps,reference=test$outcome,positive="F")
confusionMatrixNET


#Usando softmax
trainNorm$outcome = class.ind(trainNorm$outcome)
testNorm$outcome = class.ind(testNorm$outcome)
set.seed(42)
nnSOFT <- nnet(outcome~., trainNorm, size=4, decay=0.5,softmax=TRUE)



df<-data.frame(predicted=predict(nnSOFT, testNorm)[,2] > 0.5,
           actual=testNorm$outcome[,2]>0.5)

df$predicted<-as.factor(ifelse(df$predicted == FALSE, "F", "V"))
df$actual<-as.factor(ifelse(df$actual == FALSE, "F", "V"))

confusionMatrixGBOOST<-confusionMatrix(data =df$predicted, reference = test$outcome,positive="F")
confusionMatrixGBOOST



net <- neuralnet(outcome ~ age+ec+verbal+hmt+motor+eye+oblt+mdls+sah+cause+pupils+sex+ec, 
                 data = trainNorm,
                 act.fct = "tanh",
                 stepmax = 1e7,
                 hidden = c(10, 8, 6),
                 linear.output = T)

set.seed(42)
fitControl <- trainControl(method = "cv", number = 10, returnResamp = "all", search = "random",classProbs = TRUE,summaryFunction = twoClassSummary)

nnetGrid <-  expand.grid(
  nhid = seq(from = 0, to = 20, by = 1),
  actfun= c("sig","sin","radbas","hardlim","hardlims","satlins","tansig","tribas","poslin","purelin")
)
elm_fun <- getModelInfo("elm")[[1]]
elm_fun$prob <- function (modelFit, newdata, submodels = NULL)  {
  out <- exp(predict(modelFit, newdata))
  t(apply(out, 1, function(x) x/sum(x)))
}
nnetFit <- train(outcome ~ ., 
                 data = trainNorm,
                 method = elm_fun,
                 trControl = fitControl,
                 metric = "ROC",
                 tuneGrid = nnetGrid
                 )
print(nnetFit)
plot(nnetFit)

ps <- predict(nnetFit, testNorm)

confusionMatrixNET<-confusionMatrix(data=ps,reference=test$outcome,positive="F")
confusionMatrixNET


plot(test$medv,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(test$medv,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))

