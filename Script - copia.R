#source("http://bioconductor.org/biocLite.R")
#biocLite(c("graph", "Rgraphviz"))
library(stats) 
library(caret)
library(gdata)
cat("\014") 
CRASH_data_1 <- read.csv("C:/Users/Marta.Rodriguez/Desktop/OneDrive/TFM/CRASH_data-1.csv")

#View(CRASH_data_1)
datos.modelo <- subset(CRASH_data_1, select = 
                         c(SEX,AGE,EO_Cause,EO_Major.EC.injury,GCS_EYE,GCS_MOTOR,GCS_VERBAL,
                           PUPIL_REACT_LEFT,PUPIL_REACT_RIGHT,EO_1.or.more.PH,
                           EO_Subarachnoid.bleed,EO_Obliteration.3rdVorBC,
                           EO_Midline.shift..5mm,EO_Non.evac.haem,EO_Evac.haem,GOS5    ))

#nrow(datos.modelo) Tenemos 10008 pacientes
#Con esto limpiamos las variables de NA y campos vacios
if(TRUE){
  datos.modelo <- datos.modelo[!is.na(datos.modelo$EO_Cause),]
  datos.modelo <- datos.modelo[!is.na(datos.modelo$EO_1.or.more.PH),]
  datos.modelo <- datos.modelo[!is.na(datos.modelo$EO_Major.EC.injury),]
  datos.modelo <- datos.modelo[datos.modelo$GOS5!="",]
  datos.modelo$GOS5 <- as.character(datos.modelo$GOS5)
  datos.modelo$GOS5[which(datos.modelo$GOS5=="D")] <- "0"
  datos.modelo$GOS5[which(datos.modelo$GOS5=="SD")] <- "0"
  datos.modelo$GOS5[which(datos.modelo$GOS5=="SD*")] <- "0"
  datos.modelo$GOS5[which(datos.modelo$GOS5=="GR*")] <- "1"
  datos.modelo$GOS5[which(datos.modelo$GOS5=="GR")] <- "1"
  datos.modelo$GOS5[which(datos.modelo$GOS5=="MD*")] <- "1"
  datos.modelo$GOS5[which(datos.modelo$GOS5=="MD")] <- "1"
  datos.modelo$GOS5 <- as.numeric(datos.modelo$GOS5)
  factor(datos.modelo$EO_Major.EC.injury)
  
}

#datos.modelo=na.omit(datos.modelo)
NROW(datos.modelo)

#Vemos los valores nulos de cada variable
sapply(datos.modelo,function(x) sum(is.na(x)))
sapply(datos.modelo, function(x) length(unique(x)))
library(Amelia)
missmap(datos.modelo, main = "Missing values vs observed")
NROW(datos.modelo )


#VALIDACION CROSS k=5
# define training control
train_control<- trainControl(method="cv", number=5)

# train the model 
model<- train(factor(GOS5) ~ SEX + AGE+GCS_EYE+GCS_MOTOR+
                GCS_VERBAL+PUPIL_REACT_LEFT+PUPIL_REACT_RIGHT+
                EO_1.or.more.PH+EO_Subarachnoid.bleed+EO_Obliteration.3rdVorBC+EO_Midline.shift..5mm+EO_Non.evac.haem+EO_Evac.haem, data=datos.modelo, trControl=train_control, method="glm", family=binomial())

# print cv scores
summary(model)


#fit <- glm(factor(GOS5) ~ SEX + AGE+GCS_EYE+GCS_MOTOR+
             #GCS_VERBAL+PUPIL_REACT_LEFT+PUPIL_REACT_RIGHT+
             #EO_1.or.more.PH+EO_Subarachnoid.bleed+EO_Obliteration.3rdVorBC+EO_Midline.shift..5mm+EO_Non.evac.haem+EO_Evac.haem,data=datos.modelo,binomial(link='logit'))
#summary(fit) # show results

#Akaike Information Criterio. The best model istheone withthe lowest AIC score
AIC(fit)

# Calculate Pseudo R-2
gos <- factor(datos.modelo$GOS5)
FullcovModel <- glm(datos.modelo$GOS5~0, family=binomial(link=logit))
1-(logLik(fit))/(logLik(FullcovModel))

#Anova
anova(fit, test="Chisq")

#Nagelkerke
nagelkerke(fit, null = NULL, restrictNobs = FALSE)
#Nagelkerke debe tenerun valor entre 0 y 1, como se puede comprobar, nuestro modelo no se ajusta muy bien ya que tenemos un valor de 0.18

#Random Forest model
#rf <- randomForest(factor(GOS5) ~ SEX + AGE+GCS_EYE+GCS_MOTOR+GCS_VERBAL+PUPIL_REACT_LEFT+PUPIL_REACT_RIGHT,data=datos.modelo)
# Model output
#print(rf) 
#levels(as.factor(datos.modelo$GOS5))


#ROC
preds=predict(fit)
rocP<-roc(GOS5 ~ SEX + SEX+AGE+EO_Cause+EO_Major.EC.injury+GCS_EYE+GCS_MOTOR+GCS_VERBAL+
          PUPIL_REACT_LEFT+PUPIL_REACT_RIGHT+EO_1.or.more.PH+
          EO_Subarachnoid.bleed+EO_Obliteration.3rdVorBC+
          EO_Midline.shift..5mm+EO_Non.evac.haem+EO_Evac.haem,data=datos.modelo,levels=base::levels(as.factor(datos.modelo$GOS5)))
plot.roc(GOS5 ~  SEX + SEX+AGE+EO_Cause+EO_Major.EC.injury+GCS_EYE+GCS_MOTOR+GCS_VERBAL+
           PUPIL_REACT_LEFT+PUPIL_REACT_RIGHT+EO_1.or.more.PH+
           EO_Subarachnoid.bleed+EO_Obliteration.3rdVorBC+
           EO_Midline.shift..5mm+EO_Non.evac.haem+EO_Evac.haem,data=datos.modelo,levels=base::levels(as.factor(datos.modelo$GOS5)))
#roc(GOS5 ~ SEX + AGE+GCS_EYE+GCS_MOTOR+GCS_VERBAL+PUPIL_REACT_LEFT+PUPIL_REACT_RIGHT,data=datos.modelo, smooth=TRUE)
#auc(rocP, partial.auc=FALSE, partial.auc.focus=c("specificity","sensitivity"), partial.auc.correct=FALSE,allow.invalid.partial.auc.correct = FALSE)


#BLEARN
 head(datos.modelo, n = 4)
 datos.modelo$SEX <- factor(datos.modelo$SEX)
 datos.modelo$AGE <- factor(datos.modelo$AGE)
 datos.modelo$EO_Cause <- factor(datos.modelo$EO_Cause)
 datos.modelo$EO_Major.EC.injury <- factor(datos.modelo$EO_Major.EC.injury)
 datos.modelo$GCS_EYE <- factor(datos.modelo$GCS_EYE)
 datos.modelo$GCS_MOTOR <- factor(datos.modelo$GCS_MOTOR)
 datos.modelo$GCS_VERBAL <- factor(datos.modelo$GCS_VERBAL)
 datos.modelo$PUPIL_REACT_LEFT <- factor(datos.modelo$PUPIL_REACT_LEFT)
 datos.modelo$PUPIL_REACT_RIGHT <- factor(datos.modelo$PUPIL_REACT_RIGHT)
 datos.modelo$EO_1.or.more.PH <- factor(datos.modelo$EO_1.or.more.PH)
 datos.modelo$EO_Subarachnoid.bleed <- factor(datos.modelo$EO_Subarachnoid.bleed)
 datos.modelo$EO_Obliteration.3rdVorBC <- factor(datos.modelo$EO_Obliteration.3rdVorBC)
 datos.modelo$EO_Midline.shift..5mm <- factor(datos.modelo$EO_Midline.shift..5mm)
 datos.modelo$EO_Non.evac.haem <- factor(datos.modelo$EO_Non.evac.haem)
 datos.modelo$EO_Evac.haem <- factor(datos.modelo$EO_Evac.haem)
 #print(iamb(datos.modelo))
 #print(hc(datos.modelo))

 net.data <- bn.fit(hc(datos.modelo), datos.modelo)
 class(net.data)
 graphviz.plot(net.data, shape = "ellipse")
 
