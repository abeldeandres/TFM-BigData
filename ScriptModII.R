library(rpart)
library(caret)
library(rattle)
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

#Neural Network
library(neuralnet)



#-------------------------------------------------------------------------------
# Cross validating function

crossvalidate <- function(data,hidden_l=c(5))
{
  # @params
  
  # data          Boston dataset (data.frame)
  # hidden_l      a numeric vector with number of neurons for each hidden layer
  #               default to 5.
  
  # Scaling the data (min-max scaling)
  maxs <- apply(data, 2, max) 
  mins <- apply(data, 2, min)
  scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
  
  # Initialize cv.error vector
  cv.error <- NULL
  
  # Number of train-test splits
  k <- 10
  
  # Cross validating
  for(j in 1:k)
  {
    # Train-test split
    index <- sample(1:nrow(data),round(0.90*nrow(data)))
    train.cv <- scaled[index,]
    test.cv <- scaled[-index,]
    
    # NN fitting
    nn <- neuralnet(f,data=train.cv,hidden=hidden_l,linear.output=TRUE,lifesign = "full", stepmax=1e7)
    
    # Predicting
    pr.nn <- compute(nn,test.cv[,1:13])
    
    # Scaling back the predicted results
    pr.nn <- pr.nn$net.result*(max(data$outcome)-min(data$outcome))+min(data$outcome)
    
    # Real results
    test.cv.r <- (test.cv$outcome)*(max(data$outcome)-min(data$outcome))+min(data$outcome)
    
    # Calculating MSE test error
    cv.error[j] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
  }
  
  # Return average MSE
  return(mean(cv.error))
}


#-------------------------------------------------------------------------------
# Selecting the number of neurons in the hidden layer

# Data
data_ <- final

# Initializing test and train error vectors
test.error <- NULL
train.error <- NULL

# Scaling for NN
maxs <- apply(data_, 2, max) 
mins <- apply(data_, 2, min)
scaled <- as.data.frame(scale(data_, center = mins, scale = maxs - mins))

n <- names(scaled)
f <- as.formula(paste("outcome ~", paste(n[!n %in% "outcome"], collapse = " + ")))

# Testing and Cross validating (may take a while to compute)
for(i in 1:13)
{
  # Fit the net and calculate training error (point estimate)
  nn <- neuralnet(f,data=scaled,hidden=c(i),linear.output=TRUE,lifesign = "full", stepmax=1e7)
  train.error[i] <- sum(((as.data.frame(nn$net.result)*(50-5)+5) - (scaled$outcome*(50-5)+5))^2)/nrow(scaled)
  
  # Calculate test error through cross validation
  test.error[i] <- crossvalidate(data_,hidden_l=c(i))
  
}

# Print out test and train error vectors
test.error
train.error

# Plot train error
plot(train.error,main='MSE vs hidden neurons',xlab="Hidden neurons",ylab='Train error MSE',type='l',col='red',lwd=2)
# Plot test error
plot(test.error,main='MSE vs hidden neurons',xlab="Hidden neurons",ylab='Test error MSE',type='l',col='blue',lwd=2)

# Number of neurons (index) that minimizes test/train error
which(min(test.error) == test.error)
which(min(train.error) == train.error)





#try to increase the number of neurones in hidden layers or the number of hidden layers to acheive the best performance.
# act.fct = "tanh"
nn <- neuralnet(outcome ~ sex + age + cause + ec + eye + motor 
                + verbal + pupils + phm 
                + sah + oblt + mdls + hmt,  
                data=train, hidden=c(5,5), linear.output=FALSE, threshold=0.01)
nn$result.matrix
plot(nn)
nn.results <- compute(nn, test[,1:13])
#Accuracy
results <- data.frame(actual = test$outcome, prediction = nn.results$net.result)
results
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
#table(actual,prediction)

cmNN <-confusionMatrix(as.factor(prediction), as.factor(actual))
print(cmNN)

#Sin layers: 0.7633588
#Con layers: 0.7552481

library(nnet)

ctrl <- trainControl(method = "cv", number = 10,savePredictions = TRUE, 
                     classProbs = TRUE #give the probabilities for each class.Not just the class labels
                     )
nnetGrid <-  expand.grid(size = 1,
                         decay = 0.007498942093)
modelSoft<- nnet(outcome~., train, size=1, softmax=TRUE)


model <- train(outcome~., data = train, 
               method = "nnet", 
               #trControl = ctrl,
               #tuneLength=10 #Se ha usado para obtener el mejor resultado
               #tuneGrid =nnetGrid
               )
NNPredictions <-predict(model, test)
cmNN <-confusionMatrix(NNPredictions, test$outcome)
print(cmNN)

################################### FIN
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
