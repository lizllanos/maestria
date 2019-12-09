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

# Lectura de datos --------------------------------------------------------
setwd("D:/Master_CD/Fundamentos_1/proyecto_final")
data = read.csv("PF-02-SitiosMalignos.csv", stringsAsFactors = FALSE)

# Categorización de las variables.

attach(data)

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
tables=readHTMLTable(theurl)[[1]][-1,]

data$Pais <- toupper(data$Pais)
data$Pais[tolower(data$Pais) %ni% tolower(tables$`ISO 3166 ALPHA-2`)]<-"none"
data$Pais[data$Pais %in% names(table(data$Pais))[table(data$Pais)<=10]] <- "other" 

table(data$Pais)


# punto 3 -------------------------------------------------------


data %>% filter(Tipo==1)

