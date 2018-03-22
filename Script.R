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
library(xlsx)

#cat("\014") 
rm(list = ls())
CRASH_data_1 <- read.csv("C:/Users/Marta.Rodriguez/Desktop/OneDrive/TFM/CRASH_data-1.csv",na.strings=c("","NA"))

#View(CRASH_data_1)
datos.modelo <- subset(CRASH_data_1, select = 
                         c(SEX,AGE,EO_Cause,EO_Major.EC.injury,GCS_EYE,GCS_MOTOR,GCS_VERBAL,
                           PUPIL_REACT_LEFT,PUPIL_REACT_RIGHT,EO_Head.CT.scan,EO_1.or.more.PH,
                           EO_Subarachnoid.bleed,EO_Obliteration.3rdVorBC,
                           EO_Midline.shift..5mm,EO_Non.evac.haem,EO_Evac.haem,EO_Outcome,
                           EO_Symptoms,TH_Cause,TH_Major.EC.injury,TH_Head.CT.scan,TH_1.or.more.PH,TH_Subarachnoid.bleed,
                           TH_Obliteration.3rdVorBC,TH_Midline.shift..5mm,TH_Non.evac.haem,TH_Evac.haem,TH_Outcome,TH_Symptoms,GOS5,GOS8))





#datos.modelo<-datos.modelo$EO_Outcome[which(is.na(datos.modelo$EO_Outcome))]
#NROW(which(is.na(datos.modelo$GOS5)&is.na(datos.modelo$GOS8)))



#***********************************************************************************
# Nombre: PREPARACION DE DATOS. FASE 1
# Descripción:  
# Autor:                      Fecha:              Modificación:     
# Modificación: 
# ***********************************************************************************

#Preparamos la variable de outcome
datos.modelo$outcome<-NA
datos.modelo$outcome[which(!is.na(datos.modelo$GOS5))]<-as.character(datos.modelo$GOS5[which(!is.na(datos.modelo$GOS5))])
datos.modelo$outcome[which(!is.na(datos.modelo$GOS8))]<-as.character(datos.modelo$GOS8[which(!is.na(datos.modelo$GOS8))])


#Si el Outcome es 1 o los sintomas son 6, el paciente fallece
datos.modelo$outcome[which(datos.modelo$EO_Outcome=="1" | datos.modelo$EO_Symptoms=="6" | datos.modelo$TH_Outcome=="1" | datos.modelo$TH_Symptoms=="6")]<-"D"



#Si tienen Outcome 4 y synthom 1 en el TH, entonces estan vivos y sin resultados finales
datos.modelo$outcome[which(is.na(datos.modelo$outcome) & datos.modelo$TH_Outcome=="4" & datos.modelo$TH_Symptoms=="1")]<-"ALIVE"
datos.modelo$outcome[which(is.na(datos.modelo$outcome) & is.na(datos.modelo$TH_Symptoms)& is.na(datos.modelo$TH_Outcome) & datos.modelo$EO_Outcome=="4" & datos.modelo$EO_Symptoms=="1")]<-"ALIVE"

#Los que no tengan datos sobre los sintomas, deberan estar en NODATA
datos.modelo$outcome[which(is.na(datos.modelo$outcome) & is.na(datos.modelo$TH_Symptoms)& is.na(datos.modelo$EO_Symptoms))]<-"NODATA"
#Los que se hayan transferido a otro hospital y en dicho hospital no se tengan datos
datos.modelo$outcome[which(is.na(datos.modelo$outcome) & datos.modelo$EO_Outcome=="2" & is.na(datos.modelo$TH_Outcome))]<-"NODATA"  

#Si los sintomas son de 4 o 5, entonces le consideramos por D
datos.modelo$outcome[which(is.na(datos.modelo$outcome) & datos.modelo$TH_Symptoms=="5")]<-"D"
datos.modelo$outcome[which(is.na(datos.modelo$outcome) & datos.modelo$TH_Symptoms=="4")]<-"D"
datos.modelo$outcome[which(is.na(datos.modelo$outcome) & datos.modelo$EO_Symptoms=="5"& is.na(datos.modelo$TH_Outcome))]<-"D"
datos.modelo$outcome[which(is.na(datos.modelo$outcome) & datos.modelo$EO_Symptoms=="4"& is.na(datos.modelo$TH_Outcome))]<-"D"

#Si el outcome es de 4 (alta) y el sintoma es de 9, entonces esta vivo, pero no tenemos resultados
datos.modelo$outcome[which(is.na(datos.modelo$outcome) & datos.modelo$EO_Outcome=="4" & datos.modelo$EO_Symptoms=="9"& is.na(datos.modelo$TH_Symptoms)& is.na(datos.modelo$TH_Outcome))]<-"ALIVE"
datos.modelo$outcome[which(is.na(datos.modelo$outcome) & datos.modelo$TH_Symptoms=="9")]<-"ALIVE"
datos.modelo$outcome[which(is.na(datos.modelo$outcome) & datos.modelo$EO_Symptoms=="9")]<-"ALIVE"

#Se han visto 3 elementos de NODATA, cuyos pacientes obtienen un estado de symptoma 4, 
#por lo que se envia a estado de fallecido, son datos anomalos.
datos.modelo$outcome[which(datos.modelo$outcome=="NODATA" & datos.modelo$TH_Symptoms=="4")]<-"D"

#Transformamos las variables a 2 unicas categorias D y MDGR que son las que se estudiaran.
datos.modelo$outcome[which(datos.modelo$outcome=="D")] <- "D"
datos.modelo$outcome[which(datos.modelo$outcome=="SD")] <- "D"
datos.modelo$outcome[which(datos.modelo$outcome=="SD-")] <- "D"
datos.modelo$outcome[which(datos.modelo$outcome=="SD*")] <- "D"
datos.modelo$outcome[which(datos.modelo$outcome=="SD+")] <- "D"
datos.modelo$outcome[which(datos.modelo$outcome=="GR")] <- "MDGR"
datos.modelo$outcome[which(datos.modelo$outcome=="GR-")] <- "MDGR"
datos.modelo$outcome[which(datos.modelo$outcome=="GR+")] <- "MDGR"
datos.modelo$outcome[which(datos.modelo$outcome=="MD")] <- "MDGR"
datos.modelo$outcome[which(datos.modelo$outcome=="MD-")] <- "MDGR"
datos.modelo$outcome[which(datos.modelo$outcome=="MD+")] <- "MDGR"
#datos.modelo$outcome[which(datos.modelo$outcome=="ALIVE")] <- "ALIVE"
factor(datos.modelo$outcome)


#datos.modeloNULL<-subset(datos.modelo, is.na(outcome))
#write.xlsx(datos.modeloNULL, "C:/Users/Marta.Rodriguez/Desktop/OneDrive/TFM/NULL.xlsx")
#datos.modeloNODATA<-subset(datos.modelo, outcome == "NODATA")
#write.xlsx(datos.modeloNODATA, "C:/Users/Marta.Rodriguez/Desktop/OneDrive/TFM/NODATA.xlsx")
#datos.modeloALIVE<-subset(datos.modelo, outcome == "ALIVE" )
#write.xlsx(datos.modeloALIVE, "C:/Users/Marta.Rodriguez/Desktop/OneDrive/TFM/ALIVE2.xlsx")
#datos.modeloDEATH<-subset(datos.modelo, outcome == "D")
#write.xlsx(datos.modeloDEATH, "C:/Users/Marta.Rodriguez/Desktop/OneDrive/TFM/DEATH.xlsx")
#datos.modeloMDGR<-subset(datos.modelo, outcome == "MDGR") 
#write.xlsx(datos.modeloMDGR, "C:/Users/Marta.Rodriguez/Desktop/OneDrive/TFM/MDGR.xlsx")

#factor(datos.modeloMDGR$TH_Symptoms)
#NROW(which(is.na(datos.modelo3$GOS5)&is.na(datos.modelo3$GOS8)))
#datos.modelo <- datos.modelo[is.na(datos.modelo$outcome),]

#***********************************************************************************
# Nombre: PREPARACION DE DATOS. SCANEADO DE MDGR
# Descripción:  
# Autor:                      Fecha:              Modificación:     
# Modificación: 
# ***********************************************************************************
#Si no tenemos datos de los scanner, los eliminamos.
datos.modelo$ESTADOESCANER<-NA

#Si esta escaneado 1 y no tiene datos de scanner, entonces lo ponemos a scaneado 2
datos.modelo$EO_Head.CT.scan[which((datos.modelo$outcome == "MDGR" | datos.modelo$outcome == "D") & datos.modelo$EO_Head.CT.scan=="1" 
                                   & is.na(datos.modelo$EO_1.or.more.PH)
                                   & is.na(datos.modelo$EO_Subarachnoid.bleed)
                                   & is.na(datos.modelo$EO_Obliteration.3rdVorBC)
                                   & is.na(datos.modelo$EO_Midline.shift..5mm)
                                   & is.na(datos.modelo$EO_Non.evac.haem)
                                   & is.na(datos.modelo$EO_Evac.haem)
                                     )]<-"2"

#Lo mismo con TH
datos.modelo$TH_Head.CT.scan[which((datos.modelo$outcome == "MDGR" | datos.modelo$outcome == "D") & datos.modelo$TH_Head.CT.scan=="1" 
                                   & is.na(datos.modelo$TH_1.or.more.PH)
                                   & is.na(datos.modelo$TH_Subarachnoid.bleed)
                                   & is.na(datos.modelo$TH_Obliteration.3rdVorBC)
                                   & is.na(datos.modelo$TH_Midline.shift..5mm)
                                   & is.na(datos.modelo$TH_Non.evac.haem)
                                   & is.na(datos.modelo$TH_Evac.haem)
)]<-"2"


datos.modelo$ESTADOESCANER[which((datos.modelo$outcome == "MDGR" | datos.modelo$outcome == "D") & datos.modelo$EO_Head.CT.scan=="1")]<-"SCANEADO"
datos.modelo$ESTADOESCANER[which((datos.modelo$outcome == "MDGR" | datos.modelo$outcome == "D") & datos.modelo$EO_Head.CT.scan=="2")]<-"NOSCANEADO"
datos.modelo$ESTADOESCANER[which((datos.modelo$outcome == "MDGR" | datos.modelo$outcome == "D") & is.na(datos.modelo$EO_Head.CT.scan) )]<-"ENANALISIS"



datos.modelo$ESTADOESCANER[which(datos.modelo$ESTADOESCANER == "SCANEADO" 
                           & datos.modelo$EO_Outcome=="2"
                           & is.na(datos.modelo$TH_1.or.more.PH)
                           & is.na(datos.modelo$TH_Subarachnoid.bleed)
                           & is.na(datos.modelo$TH_Obliteration.3rdVorBC)
                           & is.na(datos.modelo$TH_Midline.shift..5mm)
                           & is.na(datos.modelo$TH_Non.evac.haem)
                           & is.na(datos.modelo$TH_Evac.haem)
)]<-"ENANALISIS"

datos.modelo$ESTADOESCANER[which(datos.modelo$ESTADOESCANER == "NOSCANEADO" 
                           & datos.modelo$EO_Outcome=="2"
                           & !is.na(datos.modelo$TH_1.or.more.PH)
                           & !is.na(datos.modelo$TH_Subarachnoid.bleed)
                           & !is.na(datos.modelo$TH_Obliteration.3rdVorBC)
                           & !is.na(datos.modelo$TH_Midline.shift..5mm)
                           & !is.na(datos.modelo$TH_Non.evac.haem)
                           & !is.na(datos.modelo$TH_Evac.haem)
                          
)]<-"SCANEADO"


#Nos hemos dado cuenta que existen datos anomalos, que contienen varios escaneres, pero sin embargo, no se indica como
#escaneado, son los registros: 2628,3276,3279,8469,8655, etc. (En total son 12)
datos.modelo$EO_Head.CT.scan[which(datos.modelo$ESTADOESCANER == "NOSCANEADO"  
                           & !is.na(datos.modelo$EO_1.or.more.PH)
                           & !is.na(datos.modelo$EO_Subarachnoid.bleed)
                           & !is.na(datos.modelo$EO_Obliteration.3rdVorBC)
                           & !is.na(datos.modelo$EO_Midline.shift..5mm)
                           & !is.na(datos.modelo$EO_Non.evac.haem)
                           & !is.na(datos.modelo$EO_Evac.haem)
)]<-"1"
datos.modelo$ESTADOESCANER[which(datos.modelo$ESTADOESCANER == "NOSCANEADO"  
                           & !is.na(datos.modelo$EO_1.or.more.PH)
                           & !is.na(datos.modelo$EO_Subarachnoid.bleed)
                           & !is.na(datos.modelo$EO_Obliteration.3rdVorBC)
                           & !is.na(datos.modelo$EO_Midline.shift..5mm)
                           & !is.na(datos.modelo$EO_Non.evac.haem)
                           & !is.na(datos.modelo$EO_Evac.haem)
                           )]<-"SCANEADO"

#Nos hemos dado cuenta que existen datos anomalos, que no contienen completamente la variable de scanner a 2,
#se contiene la variable de TH (han sido mandados a otro hospital) y las todas las variables del scanner
#se encuentran en NA, por lo tanto se ha decidido poner la variable de CT.Scan a 2, e interpretar que no se realizaron
#los escaneres
datos.modelo$TH_Head.CT.scan[which(datos.modelo$ESTADOESCANER == "ENANALISIS"  
                                   & !is.na(datos.modelo$EO_Cause) 
                                   & !is.na(datos.modelo$EO_Major.EC.injury)
                                   & !is.na(datos.modelo$TH_Major.EC.injury)
)]<-"2"

datos.modelo$ESTADOESCANER[which(datos.modelo$ESTADOESCANER == "ENANALISIS"  
                                   & !is.na(datos.modelo$EO_Cause) 
                                   & !is.na(datos.modelo$EO_Major.EC.injury)
                                   & !is.na(datos.modelo$TH_Major.EC.injury)
)]<-"SCANEADO"


#Eliminamos los que no tengan el EC Injury en el TH
datos.modelo$ESTADOESCANER[which(datos.modelo$ESTADOESCANER == "SCANEADO"  & datos.modelo$EO_Outcome=="2" & is.na(datos.modelo$TH_Major.EC.injury))]<-"ENANALISIS"
#Eliminamos los que no tengan el EC Injury en el EO
datos.modelo$ESTADOESCANER[which(datos.modelo$ESTADOESCANER == "SCANEADO"  & is.na(datos.modelo$EO_Major.EC.injury))]<-"ENANALISIS"
#Eliminamos los que no tengan el EO_Outcome
datos.modelo$ESTADOESCANER[which(datos.modelo$ESTADOESCANER == "SCANEADO"  & is.na(datos.modelo$EO_Outcome))]<-"ENANALISIS"

#Comprobamos que no existan variables que contengan nulos
datos.modelo$ESTADOESCANER[which(datos.modelo$ESTADOESCANER == "SCANEADO" & (is.na(datos.modelo$EO_Cause ) | is.na(datos.modelo$EO_Symptoms)))]<-"ENANALISIS"


scaneadoVIVOS<-subset(datos.modelo, ESTADOESCANER == "SCANEADO" & datos.modelo$outcome == "MDGR")
noscaneadoVIVOS<-subset(datos.modelo, ESTADOESCANER == "NOSCANEADO" & datos.modelo$outcome == "MDGR") 
enalisisVIVOS<-subset(datos.modelo, ESTADOESCANER == "ENANALISIS" & datos.modelo$outcome == "MDGR")
scaneadosDEATH<-subset(datos.modelo, ESTADOESCANER == "SCANEADO" & datos.modelo$outcome == "D")
noscaneadoDEATH<-subset(datos.modelo, ESTADOESCANER == "NOSCANEADO" & datos.modelo$outcome == "D") 
enanalisisDEATH<-subset(datos.modelo, ESTADOESCANER == "ENANALISIS" & datos.modelo$outcome == "D")

final <- merge(scaneadoVIVOS, scaneadosDEATH, all=TRUE) 

#write.xlsx(final, "C:/Users/Marta.Rodriguez/Desktop/OneDrive/TFM/FINAL.xlsx")                   
#write.xlsx(scaneadosDEATH, "C:/Users/Marta.Rodriguez/Desktop/OneDrive/TFM/ESCANEADO_DEATHS.xlsx")
#write.xlsx(scaneadoVIVOS, "C:/Users/Marta.Rodriguez/Desktop/OneDrive/TFM/ESCANEADO.xlsx")
#write.xlsx(enanalisisDEATH, "C:/Users/Marta.Rodriguez/Desktop/OneDrive/TFM/ENANALISIS_DEATHS.xlsx")
#write.xlsx(enalisisVIVOS, "C:/Users/Marta.Rodriguez/Desktop/OneDrive/TFM/ENANALISIS.xlsx")
#write.xlsx(datos.modeloNOSCANEADO, "C:/Users/Marta.Rodriguez/Desktop/OneDrive/TFM/NOESCANEADO.xlsx")
#write.xlsx(datos.modeloENANALISIS, "C:/Users/Marta.Rodriguez/Desktop/OneDrive/TFM/ENANALISIS.xlsx")



#***********************************************************************************
# Nombre: PREPARACION DE DATOS. FASE 3
# Descripción:  
# Autor:                      Fecha:              Modificación:     
# Modificación: 
# ***********************************************************************************

#Creamos una columna con la union de las pupilas
final$pupils<-NA
final$pupils[which(final$PUPIL_REACT_LEFT=="1" & final$PUPIL_REACT_RIGHT=="1")] <- "1" #both reactive
final$pupils[which(final$PUPIL_REACT_LEFT=="3" & final$PUPIL_REACT_RIGHT=="1")] <- "1" #both reactive
final$pupils[which(final$PUPIL_REACT_LEFT=="1" & final$PUPIL_REACT_RIGHT=="3")] <- "1" #both reactive
final$pupils[which(final$PUPIL_REACT_LEFT=="1" & final$PUPIL_REACT_RIGHT=="2")] <- "2" #no response unilateral
final$pupils[which(final$PUPIL_REACT_LEFT=="2" & final$PUPIL_REACT_RIGHT=="1")] <- "2" #no response unilateral
final$pupils[which(final$PUPIL_REACT_LEFT=="3" & final$PUPIL_REACT_RIGHT=="2")] <- "2" #no response unilateral 
final$pupils[which(final$PUPIL_REACT_LEFT=="2" & final$PUPIL_REACT_RIGHT=="3")] <- "2" #no response unilateral 
final$pupils[which(final$PUPIL_REACT_LEFT=="2" & final$PUPIL_REACT_RIGHT=="2")] <- "3" #no response
final$pupils[which(final$PUPIL_REACT_LEFT=="3" & final$PUPIL_REACT_RIGHT=="3")] <- "4" #unable to asseses
factor(final$pupils)
#final <- final[,c(8,9,33)]
#write.xlsx(final, "C:/Users/Marta.Rodriguez/Desktop/OneDrive/TFM/ANALISISPUPILAR.xlsx")
NROW(final$pupils[which(final$pupils==1)])
NROW(final$pupils[which(final$pupils==2)])
NROW(final$pupils[which(final$pupils==3)])
NROW(final$pupils[which(final$pupils==4)])


NROW(final$EO_Outcome[which(final$EO_Outcome==2)])


#PHM
final$phm<-NA
final$phm[which(final$EO_Outcome=="2" & final$TH_Head.CT.scan=="1")] <- final$TH_1.or.more.PH[which(final$EO_Outcome=="2" & final$TH_Head.CT.scan=="1")]
final$phm[which(final$EO_Outcome!="2"  | final$TH_Head.CT.scan!="1")] <- final$EO_1.or.more.PH[which(final$EO_Outcome!="2" | final$TH_Head.CT.scan!="1")]

#SAH
final$sah<-NA
final$sah[which(final$EO_Outcome=="2" & final$TH_Head.CT.scan=="1")] <- final$TH_Subarachnoid.bleed[which(final$EO_Outcome=="2" & final$TH_Head.CT.scan=="1")]
final$sah[which(final$EO_Outcome!="2"  | final$TH_Head.CT.scan!="1")] <- final$EO_Subarachnoid.bleed[which(final$EO_Outcome!="2"  | final$TH_Head.CT.scan!="1")]

#OBLT
final$oblt<-NA
final$oblt[which(final$EO_Outcome=="2" & final$TH_Head.CT.scan=="1")] <- final$TH_Obliteration.3rdVorBC[which(final$EO_Outcome=="2" & final$TH_Head.CT.scan=="1")]
final$oblt[which(final$EO_Outcome!="2"  | final$TH_Head.CT.scan!="1")] <- final$EO_Obliteration.3rdVorBC[which(final$EO_Outcome!="2"  | final$TH_Head.CT.scan!="1")]

#mdls
final$mdls<-NA
final$mdls[which(final$EO_Outcome=="2" & final$TH_Head.CT.scan=="1")] <- final$TH_Midline.shift..5mm[which(final$EO_Outcome=="2" & final$TH_Head.CT.scan=="1")]
final$mdls[which(final$EO_Outcome!="2"  | final$TH_Head.CT.scan!="1")] <- final$EO_Midline.shift..5mm[which(final$EO_Outcome!="2"  | final$TH_Head.CT.scan!="1")]

#final$hmt<-NA
write.xlsx(final, "C:/Users/Marta.Rodriguez/Desktop/OneDrive/TFM/FINALAUNADOS.xlsx")

#Eliminamos las variables que no necesitemos
final <- subset(final, select = 
                         c(SEX,AGE,EO_Cause,EO_Major.EC.injury,GCS_EYE,GCS_MOTOR,GCS_VERBAL,
                           pupils,EO_Head.CT.scan,EO_1.or.more.PH,
                           EO_Subarachnoid.bleed,EO_Obliteration.3rdVorBC,
                           EO_Midline.shift..5mm,EO_Non.evac.haem,EO_Evac.haem,EO_Outcome,
                           EO_Symptoms,TH_Cause,TH_Major.EC.injury,TH_Head.CT.scan,TH_1.or.more.PH,TH_Subarachnoid.bleed,
                           TH_Obliteration.3rdVorBC,TH_Midline.shift..5mm,TH_Non.evac.haem,TH_Evac.haem,TH_Outcome,TH_Symptoms,outcome))

#final <- final[which(!is.na(final$TH_Major.EC.injury) & (final$EO_Major.EC.injury != final$TH_Major.EC.injury)),c(4,19)]
#QUe diferencia hay entre los dos evac.haem?

#datos.modelo <- datos.modelo[datos.modelo$EO_Head.CT.scan=="1",]
datos.modelo <- datos.modelo[!is.na(datos.modelo$outcome),]
#datos.modelo <- datos.modelo[!is.na(datos.modelo$outcome),]

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
 
