library(corrplot)
library(normtest) ###REALIZA 5 PRUEBAS DE NORMALIDAD###
library(nortest) ###REALIZA 10 PRUEBAS DE NORMALIDAD###
library(moments) ###REALIZA 1 PRUEBA DE NORMALIDAD###
library(mvnTest)
library(randomForest)
library(rpart)
library(rpart.plot)
library(mvShapiroTest)

library("devtools")
library("factoextra")

rm(list = ls())
dev.off(dev.list()["RStudioGD"])

final <- read.csv("C:/Users/Abel de Andrés Gómez/OneDrive/TFM/TraumaticData.csv",na.strings=c("","NA"))

#Coeficiente de correlacion de Pearson R
MCOR <- cor(final)
corrplot(MCOR, method = "number") # Display the correlation coefficient

correlacion<-cor(final$age, final$outcome, method=c("pearson"))

boxplot(final,style = "tukey")
#Busqueda de Outliers
b<-boxplot(final$age,  ylab = "age",style = "tukey")
b<-b$out
b
#b1<-outlier(final$age,opposite = FALSE, logical = FALSE)
#b1
#rm.outlier(final$age, fill = FALSE, median = FALSE, opposite = FALSE)
#NROW(final$age[which(final$age>84)])
#final <- final[final$age<85,]
#boxplot(final$age,  ylab = "age")


#boxplot(final$sex,  ylab = "sex")
#dotchart(final$sex, xlab = "sex",  ylab = "Orden de los datos")
#boxplot(final$cause,  ylab = "cause")
#boxplot(final$ec,  ylab = "ec")
#boxplot(final$eye,  ylab = "eye")
#boxplot(final$motor,  ylab = "motor")
#boxplot(final$verbal,  ylab = "verbal")
#boxplot(final$pupils,  ylab = "pupils")
#boxplot(final$phm,  ylab = "phm")
#boxplot(final$sah,  ylab = "sah")
#boxplot(final$oblt,  ylab = "oblt")
#boxplot(final$mdls,  ylab = "mdls")
#boxplot(final$hmt,  ylab = "hmt")

#Se eliminan los datos que esten por encima o por debajo del quantil (Q1 y Q3) o fuera del rango
#interquantilico
eliminar_outliers <- function(x, na.rm = TRUE) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

final$age= eliminar_outliers(final$age)
final <-na.omit(final)
#final <- final[!is.na(final$age),]
boxplot(final$age,  ylab = "age")

MCOR <- cor(final)
corrplot(MCOR, method = "number") # Display the correlation coefficient

#No se puede utilizar MVN ya que la matriz no es cuadrada y el determinante vale 0
#matrix<-data.matrix(final[1:14,1:14], rownames.force = NA)
#result <- mvn(data = final[1:5000, ], mvnTest = "hz")
HZ.test(final, qqplot = TRUE)

mvShapiro.Test(as.matrix(final[,1:14]))
#ANALISIS DE NORMALIDAD
par(mfrow=c(2,2))
plot(density(final$sex))
plot(density(final$age))
plot(density(final$cause))
plot(density(final$ec))
par(mfrow=c(2,2))
plot(density(final$eye))
plot(density(final$motor))
plot(density(final$verbal))
plot(density(final$pupils))
par(mfrow=c(2,2))
plot(density(final$phm))
plot(density(final$sah))
plot(density(final$oblt))
plot(density(final$mdls))
par(mfrow=c(1,2))
plot(density(final$hmt))
plot(density(final$outcome))

shapiro.test(final$sex[1:5000])
shapiro.test(final$age[1:5000])
qqnorm(final$age);qqline(final$age, col = 2)
shapiro.test(final$cause[1:5000])
shapiro.test(final$ec[1:5000])
shapiro.test(final$eye[1:5000])
shapiro.test(final$motor[1:5000])
shapiro.test(final$verbal[1:5000])
shapiro.test(final$pupils[1:5000])
shapiro.test(final$phm[1:5000])
shapiro.test(final$sah[1:5000])
shapiro.test(final$oblt[1:5000])
shapiro.test(final$mdls[1:5000])
shapiro.test(final$hmt[1:5000])
shapiro.test(final$outcome[1:5000])

mean<-colMeans(final)
Sx<-cov(final)
D2<-mahalanobis(final,mean,Sx)
D2

#http://www.cotradingclub.com/2017/05/25/prueba-de-normalidad-en-modelos-de-prediccion/
#jb.norm.test(final)
#hist(final$age)
#mardia(final)

#Random Trees (Importance Variable) https://www.r-bloggers.com/variable-importance-plot-and-variable-selection/
fit1=randomForest(outcome ~.,
                 data=final)
importancia=data.frame(importance(fit1))

importancia<-sort_df(importancia,vars='IncNodePurity')
importancia

VI_F=importance(fit1)
View(VI_F[order(abs(VI_F),decreasing=T),])

#El siguiente grafico representa la importancia
#de las variables segun su media y los valores del Random Forest
varImpPlot(fit1,type=2)

fit=rpart(outcome ~sex+age+cause+ec+eye+motor+verbal+
            pupils+phm+sah+oblt+mdls+hmt,
          data=final)

rpart.plot(fit)
printcp(fit)
fit

##plot(fit)
#text(fit)

barplot(t(VI_F/sum(VI_F)), horiz=TRUE)


#ANALISIS PCA
#Obtenemos las componentes principales, en principio son 14, que corresponde con el numero de variables
pca.final <- prcomp(final,scale=FALSE)
summary(pca.final)
#En el siguiente grafico, vemos que
#La primera componente captura 'casi toda' la variabilidad de los datos! 
#Esto quiere decir que podríamos reducir las \(7\) variables originales a una sola variable (componente principal) 
#manteniendo (prácticamente) constante la cantidad de información disponible con respecto al conjunto de datos originales.
plot(pca.final)

#Esto ocurre debido a las unidades en las que se miden las variables, que son distintas
#Pero esto es debido exclusivamente al efecto de 'escala' con el que hemos medido los datos. 
#Por ello, en vez de utilizar la matriz de covarianzas para hacer PCA, se utiliza la matriz de correlación:

pca.final2 <- prcomp(final,scale=T)
summary(pca.final2)
#Al homogeneizar la 'escala' en la que hemos medido las variables, 
#la distribución de la variabilidad entre las com ponentes parece más racional. 
plot(pca.final2)
pca.final2$sdev # Varianza de cada componente.

pca.final2$rotation # cargas de cada componente.

# plot method
plot(pca.final2, type = "l")
biplot(pca.final2, scale = 0)

fviz_eig(pca.final2,ncp = 14)

# plot percentage of variance explained for each principal component    
barplot(100*p.variance.explained, las=2, xlab='', ylab='% Variance Explained')


#Distancia de cooks para determinar los Outliers
#But, what does cook's distance mean? It computes the influence exerted by each data point (row) on the predicted outcome.
mod <- lm(outcome ~ ., data=final)
cooksd <- cooks.distance(mod)

plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
head(final[influential, ])  # influential observations.

#***********************************************************************************
# Nombre: MODELO COMPLETO
# Descripción:  
# Autor:                      Fecha:              Modificación:     
# Modificación: 
# ***********************************************************************************

#data_train <- final[1:3465, ]
#data_test <- final[3466:NROW(final ), ]


fit <- glm(outcome ~sex+age+cause+ec+eye+motor+verbal+
             pupils+phm+sah+oblt+mdls+hmt,
           data=final,family = binomial(link="logit"))
summary(fit) # show results
jb.norm.test(fit$residuals)
hist(fit$residuals)

#CROSS VALIDATION
set.seed(1337)
#Fit a logistic model using default and income values
glm.fit <- glm(outcome ~SEX+AGE+EO_Cause+EO_Major.EC.injury+GCS_EYE+GCS_MOTOR+GCS_VERBAL+
                 pupils+phm+sah+oblt+mdls+hmt ,data=final,family = binomial)
# Create a vector with three blank values
cv.error <- rep(0,3)


# Store the results of each K  validation set into cv.error.  Use K= {3,5,10} 
cv.error[1] <- cv.glm(final, glm.fit, K=3)$delta[1]
cv.error[2] <- cv.glm(final, glm.fit, K=5)$delta[1]
cv.error[3] <- cv.glm(final, glm.fit, K=10)$delta[1]

1- mean(cv.error) 

#ANOVA
anova(fit, test = 'Chisq')

NagelkerkeR2(fit)



glm.probs <- predict(fit, newdata = data_test, type = "response")  # predicted probabilities
glm.pred <- rep(0, 45)  # Muere
glm.pred[glm.probs > 0.5] = "1"  #  Vive

confusionMatrix(table(glm.pred, data_test$outcome), positive = "1")  # from the caret package, also need e1071 package

#Model Performance
pr <- prediction(glm.probs, data_test$outcome)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf,colorize = TRUE)
# Añadimos la recta y=x que sería la correspondiente al peor clasificador
abline(a = 0, b = 1)
# añadimos el valor del área bajo la curva


#AUC. a model with good predictive ability should have an AUC closer to 1 (1 is ideal) than to 0.5. 
#Nuestro valor es de 0.73
AUC <- performance(pr, measure = "auc")
AUC@y.name
AUC@y.values
AUC

text(0.4, 0.6, paste(AUC@y.name, "\n", round(unlist(AUC@y.values), 3)), cex = 0.7)

#***********************************************************************************
# Nombre: MODELO 1
# Descripción:  
# Autor:                      Fecha:              Modificación:     
# Modificación: 
# ***********************************************************************************

data_train <- datos.modelo[1:3581, ]
data_test <- datos.modelo[3582:NROW(datos.modelo ), ]


fit <- glm(GOS5 ~ SEX+AGE+EO_Cause+GCS_EYE+GCS_MOTOR+GCS_VERBAL+
             pupils+EO_1.or.more.PH+
             EO_Subarachnoid.bleed+EO_Obliteration.3rdVorBC+
             EO_Midline.shift..5mm+EO_Non.evac.haem+EO_Evac.haem,data=data_train,family = binomial(link="logit"))
summary(fit) # show results


#ANOVA
anova(fit, test = 'Chisq')

NagelkerkeR2(fit)




#BLEARN
#head(datos.modelo, n = 4)
#datos.modelo$SEX <- factor(datos.modelo$SEX)
#datos.modelo$AGE <- factor(datos.modelo$AGE)
#datos.modelo$EO_Cause <- factor(datos.modelo$EO_Cause)
#datos.modelo$EO_Major.EC.injury <- factor(datos.modelo$EO_Major.EC.injury)
#datos.modelo$GCS_EYE <- factor(datos.modelo$GCS_EYE)
#datos.modelo$GCS_MOTOR <- factor(datos.modelo$GCS_MOTOR)
#datos.modelo$GCS_VERBAL <- factor(datos.modelo$GCS_VERBAL)
#datos.modelo$PUPIL_REACT_LEFT <- factor(datos.modelo$PUPIL_REACT_LEFT)
#datos.modelo$PUPIL_REACT_RIGHT <- factor(datos.modelo$PUPIL_REACT_RIGHT)
#datos.modelo$EO_1.or.more.PH <- factor(datos.modelo$EO_1.or.more.PH)
#datos.modelo$EO_Subarachnoid.bleed <- factor(datos.modelo$EO_Subarachnoid.bleed)
#datos.modelo$EO_Obliteration.3rdVorBC <- factor(datos.modelo$EO_Obliteration.3rdVorBC)
#datos.modelo$EO_Midline.shift..5mm <- factor(datos.modelo$EO_Midline.shift..5mm)
#datos.modelo$EO_Non.evac.haem <- factor(datos.modelo$EO_Non.evac.haem)
#datos.modelo$EO_Evac.haem <- factor(datos.modelo$EO_Evac.haem)
#print(iamb(datos.modelo))
#print(hc(datos.modelo))

#net.data <- bn.fit(hc(datos.modelo), datos.modelo)
#class(net.data)
#graphviz.plot(net.data, shape = "ellipse")
