# Trabajo final Fundamentos de Analítica I
# Lizeth Llanos y Diego Agudelo

library(reshape2)
library(ggplot2)
library(caret)
library(MASS)
library(naivebayes)
library(dplyr)
library(stringr)

# Lectura de datos --------------------------------------------------------
setwd("D:/Master_CD/Fundamentos_1/proyecto_final")
data = read.csv("PF-02-SitiosMalignos.csv", stringsAsFactors = FALSE)

# Categorización de las variables.

attach(data)

Codificacion[Codificacion %in% c("ISO-8859","iso-8859-1","ISO-8859-1","windows-1251","windows-1252")] <- "iso"
Codificacion[Codificacion %in% c("UTF-8","utf-8")] <- "utf-8"
Codificacion[Codificacion %in% c("None")] <- "none"

OS[grep("apache",OS,ignore.case =T,fixed=F )] <- "apache"
OS[grep("ats",OS,ignore.case =T,fixed=F )] <- "apache"
OS[grep("nginx",OS,ignore.case =T,fixed=F )] <- "nginx"
OS[grep("None",OS,ignore.case =T,fixed=F )] <- "ninguno"
OS[grep("MICROSOFT-HTTPAPI",OS,ignore.case =T,fixed=F )] <- "microsoft-httpapi"
OS[grep("Microsoft-IIS",OS,ignore.case =T,fixed=F )] <- "microsoft-iis"

'%ni%' <- Negate('%in%')
OS[ OS %ni% c("apache", "nginx","none","microsoft-httpapi","microsoft-iis") ] <- "ninguno"


table(tolower(Pais))


# Limpieza de datos -------------------------------------------------------
dim(data)
str(data)
sum(duplicated(data))


