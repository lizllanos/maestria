# Trabajo final Fundamentos de Analítica I
# Lizeth Llanos y Diego Agudelo

library(reshape2)
library(ggplot2)
library(caret)
library(MASS)
library(naivebayes)
library(dplyr)
library(stringr)
library(XML)
library(RCurl)
library(corrplot)

# Lectura de datos --------------------------------------------------------
setwd("D:/Master_CD/Fundamentos_1/proyecto_final")
data = read.csv("PF-02-SitiosMalignos.csv", stringsAsFactors = FALSE)

# Categorización de las variables.

attach(data)

'%notin%' <- Negate('%in%')
'%ni%' <- Negate('%in%')

Codificacion[Codificacion %in% c("ISO-8859","iso-8859-1","ISO-8859-1","windows-1251","windows-1252")] <- "iso"
Codificacion[Codificacion %in% c("UTF-8","utf-8")] <- "utf-8"
Codificacion[Codificacion %in% c("None")] <- "none"

OS[grep("apache",OS,ignore.case =T,fixed=F )] <- "apache"
OS[grep("ats",OS,ignore.case =T,fixed=F )] <- "apache"
OS[grep("nginx",OS,ignore.case =T,fixed=F )] <- "nginx"
OS[grep("None",OS,ignore.case =T,fixed=F )] <- "none"
OS[grep("MICROSOFT-HTTPAPI",OS,ignore.case =T,fixed=F )] <- "microsoft-httpapi"
OS[grep("Microsoft-IIS",OS,ignore.case =T,fixed=F )] <- "microsoft-iis"
OS[ OS %ni% c("apache", "nginx","none","microsoft-httpapi","microsoft-iis") ] <- "none"


url_main="https://laendercode.net/es/2-letter-list.html"
theurl=getURL(url_main,.opts = list(ssl.verifypeer = FALSE))
Sys.sleep (0.2) 
tables=readHTMLTable(theurl)[[1]][-1,]

data$Pais <- toupper(data$Pais)
data$Pais[tolower(data$Pais) %ni% tolower(tables$`ISO 3166 ALPHA-2`)]<-"none"
data$Pais[data$Pais %in% names(table(data$Pais))[table(data$Pais)<=10]] <- "other" 

table(data$Pais)
names(data) = tolower(names(data))

data$os = as.factor(data$os)
data$codificacion = as.factor(data$codificacion)
summary(data) #La variable LargoHeader es la que más datos faltantes tiene

data$largoheadermissing = as.character(ifelse(is.na(data$largoheader), 1, 0))
data$largoheader = ifelse(is.na(data$largoheader), 0, data$largoheader)

data$tipo = factor(data$tipo)

data = data[-which(is.na(data$numpaquetesdns)),]

data_num = data[,which(sapply(data,class) %notin% c("character", "factor"))]
data_m = melt(data_num)


atipicos = function(x,p,r){
  qnt<-mean(x, na.rm = T)
  H <-p * sd(x, na.rm = T)
  x2<-ifelse(x> qnt+H | x< qnt-H ,1,0)
  
  if(r==1){
    pos = which(x2==T)
    return(pos)
  }else{
    return(sum(x2))
  }
  
}

atip = table(unlist(sapply(data_num, atipicos,4.2,1)))
atip
pos_atip = as.numeric(names(atip)[which(atip>=5)])
pos_atip
#Remove atipicos
data[pos_atip,] = NA
data = na.omit(data)

data_num = data[,which(sapply(data,class) %notin% c("character", "factor"))]
data_m = melt(data_num)


# punto 3 -------------------------------------------------------

data_3 <- cbind.data.frame(data_num,tipo=data$tipo) %>% filter(tipo==1) %>% dplyr::select(-tipo) 


corMat <- cor(data_3)
corrplot(corMat, type="upper", order="hclust")

library("factoextra")
library("FactoMineR")

res.pca <- PCA(data_3,ncp=4,  graph = FALSE)

get_eig(res.pca)

fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 80))

fviz_pca_var(res.pca, col.var = "black")
load_cor <- t(cor(res.pca$ind$coord[,1:4,drop=F],data_3))
data_plot=melt(load_cor)


fviz_pca_ind(res.pca, label="none", habillage=data$tipo,
             addEllipses=TRUE, ellipse.level=0.95,
             palette = c("#999999", "#E69F00"))

fviz_pca_ind(res.pca)

fviz_pca_biplot(res.pca, repel = TRUE)


ggplot(aes( x = factor(Var1), y=value),data =data_plot ) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip()+
  facet_wrap(~ factor(Var2)) +
  ylab("Correlación de pearson") +
  xlab("Variables") +
  theme_bw() 
  


k <- 3
set.seed(1234)
kmClustering3 <- kmeans(data_3, k, nstart=100, iter.max=150)


fviz_cluster(kmClustering3, data_3, geom="point")#labelsize = 5)




vars <- apply(data_3,2,var)
sumvars <- sum(vars)
wss <- (nrow(data_3) - 1)*sumvars # Para obtener el TSS multiplicamos la varianza por (N-1)

# Para K>1, calculamos las diferencias cuadradas en un ciclo
set.seed(1234)
maxK <- 10
for (k in 2:maxK) { 

  wssK <- 0 # Aquí va a quedar el total de WSS para el k actual
  kmClustering <- kmeans(data_3, k, nstart=30, iter.max=150)
  data_3$clusters <- kmClustering$cluster
  
  for(i in 1:k) { # Recorrido de los clusters

    clusterData <- subset(data_3, clusters==i)
    centroide <- apply(clusterData, 2, FUN=mean)
    wssK <- wssK + sum(apply(clusterData, 1, FUN=function(fila) {sum((fila-centroide)^2)}))
  }
  # Una vez calculado el wss del K actual, lo guardamos en el vector de WSS
  wss[k] <- wssK
}
wss

plot(1:maxK, wss, type = "b", xlab = "Número de Clusters", ylab = "Within groups sum of squares")  
grid()

