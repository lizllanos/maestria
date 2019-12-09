# Trabajo final Fundamentos de Analítica I
# Lizeth Llanos y Diego Agudelo

library(reshape2)
library(ggplot2)
library(caret)
library(MASS)
library(naivebayes)
library(dplyr)

# Lectura de datos --------------------------------------------------------
setwd("C:/Users/lllanos/Dropbox/ICESI/Semestre I/Fundamentos de analítica I/Proyecto final")
data = read.csv("PF-02-SitiosMalignos.csv", stringsAsFactors = FALSE)

`%notin%` <- Negate(`%in%`)

# Limpieza de datos -------------------------------------------------------
dim(data)
str(data)
sum(duplicated(data))

names(data) = tolower(names(data))

# Verificar datosNA
summary(data) #La variable LargoHeader es la que más datos faltantes tiene

data$largoheadermissing = as.character(ifelse(is.na(data$largoheader), 1, 0))
table(data$largoheadermissing)

data$largoheader = ifelse(is.na(data$largoheader), 0, data$largoheader)
summary(data$largoheader)

data$tipo = factor(data$tipo)

data_num = data[,which(sapply(data,class) %notin% c("character", "factor"))]

data_m = melt(data_num)
ggplot(data_m, aes(variable, value))+geom_boxplot() + facet_wrap(~variable, scales = "free")

x11()
ggplot(data_m, aes(value))+geom_histogram() + facet_wrap(~variable, scales = "free")

# Análisis de datos atípicos

qnt<-quantile(data_num[,1], probs=c(.25, .75), na.rm = T)
H <-0.5 * IQR(data_num[,1], na.rm = T)
train_qc$atipicosraz<-ifelse(data_num[,1]> qnt[2]+H | train_qc$saleprice< qnt[1]-H ,1,0)


# Baseline
prop.table(table(data$tipo))

# Modelo logistico --------------------------------------------------------
# arreglar la codificación

data_model = na.omit(data[, -c(1,4,5,7,8,20)])
set.seed(500) 
trainIndex <- createDataPartition(data_model$tipo, p = .75, list = FALSE, times = 1)
length(trainIndex)

churnTrain <- data_model[ trainIndex,]
churnTest <-  data_model[-trainIndex,]

set.seed(123) 
model_logreg1 <- train(tipo~., churnTrain, 
                       method="glm", family="binomial",
                       trControl=trainControl(method="cv", number=5),
                       preProcess=c("center", "scale"))

model_logreg1 # No hay parámetros a tunear con Caret




trainControl=trainControl(method="cv", number=5)
set.seed(123) 
model_stepBoth <- train(tipo ~., data = data_model,
                        method = "glmStepAIC", direction ="forward", 
                        trControl = trainControl,
                        preProcess=c("center", "scale"),
                        trace=TRUE)


model_stepBoth$finalModel

predictions<-predict(object=model_stepBoth, churnTest)
confusionMatrix(predictions, factor(churnTest$tipo))



# Naives ------------------------------------------------------------------

#install.packages("naivebayes")

id= which(sapply(data_model,class)!="character")
data_model_n = mutate_each(data_model, funs(as.double), id)

tgrid1 = expand.grid(laplace=c(10, 7, 6, 5, 4.5, 4, 3.5, 3, 2, 1, 0.1, 0.01, 0.001, 0.0001, 0), 
                     usekernel=c(FALSE), adjust=c(0))
tgrid2 = expand.grid(laplace=c(10, 7, 6, 5, 4.5, 4, 3.5, 3, 2, 1, 0.1, 0.01, 0.001, 0.0001, 0), 
                     usekernel=c(TRUE), adjust=c(1))
tgrid = rbind(tgrid1, tgrid2)


data_model %>%                           # Se va a trabajar sobre el dataset completo
  filter(tipo == "0") %>%      # Sólo vamos a considerar los registros con el "LEAVE" en la variable objetivo
  select_if(is.numeric) %>%         # Sólo nos interesan las variables numéricas
  cor() %>%                         # obtenemos la matriz de correlaciones
  corrplot::corrplot()    


churnTrainX = churnTrain[,-14]
set.seed(123) 
model_nb5 <- train(churnTrainX, churnTrain$tipo,
                   method = "naive_bayes", 
                   tuneGrid=tgrid,
                   trControl = trainControl)
model_nb5
model_nb5$bestTune

predictions<-predict(object=model_nb5, churnTest)
confusionMatrix(predictions, factor(churnTest$tipo))
