# Trabajo final Fundamentos de Analítica I
# Lizeth Llanos y Diego Agudelo

library(reshape2)
library(ggplot2)
library(caret)
library(MASS)

# Lectura de datos --------------------------------------------------------
setwd("C:/Users/lllanos/Dropbox/ICESI/Semestre I/Fundamentos de analítica I/Proyecto final")
data = read.csv("PF-02-SitiosMalignos.csv", stringsAsFactors = FALSE)


# Limpieza de datos -------------------------------------------------------
dim(data)
str(data)
sum(duplicated(data))

names(data) = tolower(names(data))
# Baseline
data$tipo = as.character(data$tipo)
table(data$ripo)/nrow(data)

# Verificar datosNA
summary(data) #La variable LargoHeader es la que más datos faltantes tiene

data$largoheadermissing = ifelse(is.na(data$largoheader), 1, 0)
table(data$largoheadermissing)

data$largoheader = ifelse(is.na(data$largoheader), 0, data$largoheader)
summary(data$largoheader)

data_num = data[,which(sapply(data,class)!="character")]

data_m = melt(data_num)
ggplot(data_m, aes(variable, value))+geom_boxplot() + facet_wrap(~variable, scales = "free")


# Modelo logistico --------------------------------------------------------
# arreglar la codificación

data_model = na.omit(data[, -c(1,4,5,7,8)])
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


