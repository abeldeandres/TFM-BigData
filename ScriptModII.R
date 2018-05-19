library(rpart)
library(caret)
library(rattle)
library(RColorBrewer)
library(randomForest)
library(ROCR)
library(caTools)

rm(list = ls())

set.seed(42)
final <- read.csv("C:/Users/Abel de Andrés Gómez/OneDrive/TFM/TraumaticData.csv",na.strings=c("","NA"))
final$outcome<-as.factor(final$outcome)
ind<-sample.split(Y=final$outcome,SplitRatio =0.7)
train <- final[ind,]
test <- final[!ind,]


modelo.logit <- glm(outcome ~ ., 
                    data = train, family = "binomial")

anova(modelo.logit, test="Chisq")
summary(modelo.logit)

#Un gran valor p aquí indica que el modelo sin la variable explica más o menos la misma cantidad de variación.
#La variable Cause vemos que no nos aporta gran variacion.Sin embargo, las variables de "eye", "motor" y "oblt" al tener un valor
#p proximo a cero nos indica que son variables relevantes.
anova(modelo.logit, test="Chisq")
#NagelkerkeR2(modelo.logit)
#Diferenciar summary() y anova aqui: https://stats.stackexchange.com/questions/59879/logistic-regression-anova-chi-square-test-vs-significance-of-coefficients-ano 

options(warn=-1) 
pred <- predict(modelo.logit, newdata = test, type = "response")  # predicted probabilities
options(warn=1) 

out_pred_num <- ifelse(pred > 0.5, 1, 0)
out_pred <- factor(out_pred_num, levels=c(0, 1))
confusionMatrix(data = out_pred, reference = test$outcome,positive="1")



################################ RANDOM FOREST #################################################################
#https://cran.r-project.org/web/packages/randomForest/randomForest.pdf
model=randomForest(outcome~.,data=train,ntree=500)
print(model)  

pred <- predict(model, newdata = test)
confusionMatrix<-confusionMatrix(data=pred,reference=test$outcome,positive="1")
fourfoldplot(confusionMatrix$table)


#PARA INTERPRETAR LA MATRIZ http://www.dataschool.io/simple-guide-to-confusion-matrix-terminology/





################################ REDES NEURONALES #################################################################
library(nnet)

model.nn = nnet(outcome ~ ., data = train, size = 2, rang = 0.1, decay = 5e-4, maxit = 200)

model.predict = predict(model.nn, test, type="class")
model.predict<-as.factor(model.predict)

nn.table = table(test$outcome, model.predict)

nn.table

confusionMatrixNET<-confusionMatrix(data=model.predict,reference=test$outcome,positive="1")
confusionMatrixNET
fourfoldplot(confusionMatrixNET$table)









library(ROCR)
options(warn=-1) 
glm.probs <- predict(modelo.logit, newdata = test, type = "response")  # predicted probabilities
options(warn=1) 
glm.pred <- rep(0, 45)  # Muere
glm.pred[glm.probs > 0.5] = "1"  #  Vive

confusionMatrix(table(glm.pred, test$outcome), positive = "1")  # from the caret package, also need e1071 package

#Model Performance
pr <- prediction(glm.probs, test$outcome)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf,colorize = TRUE)
# Añadimos la recta y=x que sería la correspondiente al peor clasificador
abline(a = 0, b = 1)
# añadimos el valor del área bajo la curva


#AUC. a model with good predictive ability should have an AUC closer to 1 (1 is ideal) than to 0.5. 
AUC <- performance(pr, measure = "auc")
AUC@y.name
AUC@y.values
AUC

text(0.4, 0.6, paste(AUC@y.name, "\n", round(unlist(AUC@y.values), 3)), cex = 0.7)
