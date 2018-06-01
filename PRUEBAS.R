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


set.seed(42)
modelo.logit1 <- glm(outcome ~ age+ec+eye+motor+verbal+pupils+phm+sah+oblt+mdls+hmt+sex+cause, 
                    data = train, family = "binomial")

modelo.logit2 <- glm(outcome ~ age, 
                     data = train, family = "binomial")

prob1=predict(modelo.logit1,type=c("response"))
prob2=predict(modelo.logit2,type=c("response"))
library(pROC)


resRoc1 <- roc(train$outcome, modelo.logit1$fitted)
resRoc2 <- roc(train$outcome, modelo.logit2$fitted)
plot(resRoc1, legacy.axes = TRUE,print.thres = "best")
plot(resRoc2, add=TRUE, col='red',print.thres = "best")
#https://rpubs.com/Wangzf/pROC
plot.roc(train$outcome, modelo.logit2$fitted,          # data
         percent = TRUE,                    # show all values in percent
         partial.auc=c(100, 90), 
         partial.auc.correct=TRUE,          # define a partial AUC (pAUC)
         print.auc=TRUE,                    
         #display pAUC value on the plot with following options:
         print.auc.pattern = "Corrected pAUC (100-90%% SP):\n%.1f%%",
         print.auc.col = "#1c61b6",
         auc.polygon = TRUE, 
         auc.polygon.col = "#1c61b6",       # show pAUC as a polygon
         max.auc.polygon = TRUE, 
         max.auc.polygon.col = "#1c61b622", # also show the 100% polygon
         main = "Partial AUC (pAUC)")

plot.roc(train$outcome, modelo.logit2$fitted,
         percent = TRUE, 
         add = TRUE, 
         type = "n",                        # add to plot, but don't re-add the ROC itself (useless)
         partial.auc = c(100, 90), 
         partial.auc.correct = TRUE,
         partial.auc.focus = "se",          # focus pAUC on the sensitivity
         print.auc = TRUE, 
         print.auc.pattern = "Corrected pAUC (100-90%% SE):\n%.1f%%", 
         print.auc.col = "#008600",
         print.auc.y = 40,                  # do not print auc over the previous one
         auc.polygon = TRUE, 
         auc.polygon.col = "#008600",
         max.auc.polygon = TRUE, 
         max.auc.polygon.col = "#00860022")




#http://blog.revolutionanalytics.com/2016/05/using-caret-to-compare-models.html

ci.auc(resRoc1, conf.level=0.95)
ci.auc(resRoc2, conf.level=0.95)





ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE,classProbs=TRUE,
                     summaryFunction=twoClassSummary)

mod_fit <- train(outcome ~ .,data = train, method="glm", family="binomial",
                 trControl = ctrl, tuneLength = 5,metric="ROC")


gbm.probs <- predict(mod_fit,test,type="prob")
head(gbm.probs)

gbm.ROC <- roc(predictor=gbm.probs$F,
               response=test$outcome,
               levels=rev(levels(test$outcome)))
#gbm.ROC$auc

#plot(gbm.ROC,main="GLM ROC")


plot.roc(test$outcome, gbm.probs$V,          # data
         percent = TRUE,                    # show all values in percent
         partial.auc=c(100, 90), 
         partial.auc.correct=TRUE,          # define a partial AUC (pAUC)
         print.auc=TRUE,                    
         #display pAUC value on the plot with following options:
         print.auc.pattern = "Corrected pAUC (100-90%% SP):\n%.1f%%",
         print.auc.col = "#1c61b6",
         auc.polygon = TRUE, 
         auc.polygon.col = "#1c61b6",       # show pAUC as a polygon
         max.auc.polygon = TRUE, 
         max.auc.polygon.col = "#1c61b622", # also show the 100% polygon
         main = "Partial AUC (pAUC)")

plot.roc(test$outcome, gbm.probs$V,
         percent = TRUE, 
         add = TRUE, 
         type = "n",                        # add to plot, but don't re-add the ROC itself (useless)
         partial.auc = c(100, 90), 
         partial.auc.correct = TRUE,
         partial.auc.focus = "se",          # focus pAUC on the sensitivity
         print.auc = TRUE, 
         print.auc.pattern = "Corrected pAUC (100-90%% SE):\n%.1f%%", 
         print.auc.col = "#008600",
         print.auc.y = 40,                  # do not print auc over the previous one
         auc.polygon = TRUE, 
         auc.polygon.col = "#008600",
         max.auc.polygon = TRUE, 
         max.auc.polygon.col = "#00860022")