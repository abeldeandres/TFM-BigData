#source("http://bioconductor.org/biocLite.R")
#biocLite(c("graph", "Rgraphviz"))
library(stats) 
library(caret)
library(gdata)
library(Amelia)
library(dplyr)
library(ROCR)
library(caret)
library(e1071)
library(boot)
library(rcompanion)
library(fmsb)
library(bnlearn)
library(ROCR) 
library(Metrics)

#cat("\014") 
rm(list = ls())
CRASH_data_1 <- read.csv("C:/Users/Marta.Rodriguez/Desktop/OneDrive/TFM/CRASH_data-1.csv",na.strings=c("","NA"))

#View(CRASH_data_1)
datos.modelo <- subset(CRASH_data_1, select = 
                         c(SEX,AGE,EO_Cause,EO_Major.EC.injury,GCS_EYE,GCS_MOTOR,GCS_VERBAL,
                           PUPIL_REACT_LEFT,PUPIL_REACT_RIGHT,EO_1.or.more.PH,
                           EO_Subarachnoid.bleed,EO_Obliteration.3rdVorBC,
                           EO_Midline.shift..5mm,EO_Non.evac.haem,EO_Evac.haem,EO_Head.CT.scan,GOS5,GOS8,EO_Outcome))
#datos.modelo <- datos.modelo[datos.modelo$EO_Head.CT.scan=="1",]

#NROW(which(datos.modelo$GOS5=="1"))



#***********************************************************************************
# Nombre: PREPARACION DE DATOS
# Descripción:  
# Autor:                      Fecha:              Modificación:     
# Modificación: 
# ***********************************************************************************

#Preparamos la variable de outcome
datos.modelo$outcome<-NA
datos.modelo$outcome[which(!is.na(datos.modelo$GOS5))]<-as.character(datos.modelo$GOS5[which(!is.na(datos.modelo$GOS5))])
datos.modelo$outcome[which(!is.na(datos.modelo$GOS8))]<-as.character(datos.modelo$GOS8[which(!is.na(datos.modelo$GOS8))])
datos.modelo$outcome[which(datos.modelo$EO_Outcome=="1")] <- "D"



#datos.modelo$GOS5 <- as.character(datos.modelo$GOS5)
#datos.modelo$GOS5[which(datos.modelo$GOS5=="D")] <- "0"
#datos.modelo$GOS5[which(datos.modelo$GOS5=="SD")] <- "0"
#datos.modelo$GOS5[which(datos.modelo$GOS5=="SD*")] <- "0"
#datos.modelo$GOS5[which(datos.modelo$GOS5=="GR*")] <- "1"
#datos.modelo$GOS5[which(datos.modelo$GOS5=="GR")] <- "1"
#datos.modelo$GOS5[which(datos.modelo$GOS5=="MD*")] <- "1"
#datos.modelo$GOS5[which(datos.modelo$GOS5=="MD")] <- "1"
#datos.modelo$GOS5 <- as.numeric(datos.modelo$GOS5)


#datos.modelo$GOS8 <- as.character(datos.modelo$GOS8)
#datos.modelo$GOS8[which(datos.modelo$GOS8=="GR-")] <- "1"
#datos.modelo$GOS8[which(datos.modelo$GOS8=="GR+")] <- "1"
#datos.modelo$GOS8[which(datos.modelo$GOS8=="MD-")] <- "1"
#datos.modelo$GOS8[which(datos.modelo$GOS8=="MD+")] <- "1"
#datos.modelo$GOS8[which(datos.modelo$GOS8=="SD-")] <- "0"
#datos.modelo$GOS8[which(datos.modelo$GOS8=="SD*")] <- "0"
#datos.modelo$GOS8[which(datos.modelo$GOS8=="SD+")] <- "0"
#datos.modelo$GOS8[which(datos.modelo$GOS8=="D")] <- "0"
#datos.modelo$GOS8 <- as.numeric(datos.modelo$GOS8)

#factor(datos.modelo$GOS8)


#Creamos una columna con la union de las pupilas
datos.modelo$pupils<-NA
datos.modelo$pupils[which(datos.modelo$PUPIL_REACT_LEFT=="1" & datos.modelo$PUPIL_REACT_RIGHT=="1")] <- "1" #both reactive
datos.modelo$pupils[which(datos.modelo$PUPIL_REACT_LEFT=="1" & datos.modelo$PUPIL_REACT_RIGHT=="2")] <- "2" #no response unilateral
datos.modelo$pupils[which(datos.modelo$PUPIL_REACT_LEFT=="2" & datos.modelo$PUPIL_REACT_RIGHT=="1")] <- "2" #no response unilateral
datos.modelo$pupils[which(datos.modelo$PUPIL_REACT_LEFT=="2" & datos.modelo$PUPIL_REACT_RIGHT=="2")] <- "3" #no response 
datos.modelo$pupils[which(datos.modelo$PUPIL_REACT_LEFT=="3" & datos.modelo$PUPIL_REACT_RIGHT=="3")] <- "4" #unable to asseses
factor(datos.modelo$pupils)


#Eliminamos las variables que no necesitemos


#Borramos todos los NA
datos.modelo<-na.omit(datos.modelo)

#datos.modelo[datos.modelo$AGE == -1, ]

#nrow(datos.modelo) Tenemos 10008 pacientes
#Con esto limpiamos las variables de NA y campos vacios
if(TRUE){
  #datos.modelo <- datos.modelo[!is.na(datos.modelo$EO_Cause),]
  #NROW(datos.modelo)
  #datos.modelo <- datos.modelo[!is.na(datos.modelo$EO_1.or.more.PH),]
  #NROW(datos.modelo)
  #datos.modelo <- datos.modelo[!is.na(datos.modelo$EO_Major.EC.injury),]
  #NROW(datos.modelo)
  #datos.modelo <- datos.modelo[datos.modelo$GOS5!="",]
  NROW(datos.modelo)
  datos.modelo$GOS5 <- as.character(datos.modelo$GOS5)
  datos.modelo$GOS5[which(datos.modelo$GOS5=="D")] <- "0"
  datos.modelo$GOS5[which(datos.modelo$GOS5=="SD")] <- "0"
  datos.modelo$GOS5[which(datos.modelo$GOS5=="SD*")] <- "0"
  datos.modelo$GOS5[which(datos.modelo$GOS5=="GR*")] <- "1"
  datos.modelo$GOS5[which(datos.modelo$GOS5=="GR")] <- "1"
  datos.modelo$GOS5[which(datos.modelo$GOS5=="MD*")] <- "1"
  datos.modelo$GOS5[which(datos.modelo$GOS5=="MD")] <- "1"
  datos.modelo$GOS5 <- as.numeric(datos.modelo$GOS5)
  #factor(datos.modelo$EO_Major.EC.injury)
}


#datos.modelo=na.omit(datos.modelo$GOS5)
NROW(datos.modelo)
datos.modelo<-na.omit(datos.modelo)
#Vemos los valores nulos de cada variable

sapply(datos.modelo,function(x) sum(is.na(x)))
sapply(datos.modelo, function(x) length(unique(x)))
missmap(datos.modelo, main = "Missing values vs observed")
NROW(datos.modelo )

#***********************************************************************************
# Nombre: MODELO COMPLETO
# Descripción:  
# Autor:                      Fecha:              Modificación:     
# Modificación: 
# ***********************************************************************************

data_train <- datos.modelo[1:3581, ]
data_test <- datos.modelo[3582:NROW(datos.modelo ), ]


fit <- glm(GOS5 ~SEX+AGE+EO_Cause+EO_Major.EC.injury+GCS_EYE+GCS_MOTOR+GCS_VERBAL+
             pupils+EO_1.or.more.PH+
             EO_Subarachnoid.bleed+EO_Obliteration.3rdVorBC+
             EO_Midline.shift..5mm+EO_Non.evac.haem+EO_Evac.haem,
           data=data_train,family = binomial(link="logit"))
summary(fit) # show results

#CROSS VALIDATION
set.seed(1337)
#Fit a logistic model using default and income values
glm.fit <- glm(GOS5 ~ SEX+ AGE+GCS_EYE+GCS_MOTOR+GCS_VERBAL+pupils ,data=datos.modelo,family = binomial)
# Create a vector with three blank values
cv.error <- rep(0,3)


# Store the results of each K  validation set into cv.error.  Use K= {3,5,10} 
cv.error[1] <- cv.glm(datos.modelo, glm.fit, K=3)$delta[1]
cv.error[2] <- cv.glm(datos.modelo, glm.fit, K=5)$delta[1]
cv.error[3] <- cv.glm(datos.modelo, glm.fit, K=10)$delta[1]

1- mean(cv.error) 

#ANOVA
anova(fit, test = 'Chisq')

NagelkerkeR2(fit)



glm.probs <- predict(fit, newdata = data_test, type = "response")  # predicted probabilities
glm.pred <- rep(0, 45)  # Muere
glm.pred[glm.probs > 0.5] = "1"  #  Vive

confusionMatrix(table(glm.pred, data_test$GOS5), positive = "1")  # from the caret package, also need e1071 package

#Model Performance
pr <- prediction(glm.probs, data_test$GOS5)
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
 
