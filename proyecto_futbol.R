 
# Proyecto Analítica Fútbol -----------------------------------------------
# Cargar librerías
options(stringsAsFactors = F)

library(dplyr)
library(tidyverse)
library(purrr)

# Lizeth Llanos y Diego Agudelo
# Septiembre 2019
# Script para proyecto de Análisis Exploratorio de datos

#Configurar directorio de trabajo
setwd("C:/Users/lllanos/Dropbox/ICESI/Semestre I/Análisis exploratorio de datos/Proyecto_grupal")

# Cargar bases de datos
data_s1 = read.csv("season-1718_csv.csv", header = T, stringsAsFactors = F)
data_s2 = read.csv("season-1819_csv.csv", header = T, stringsAsFactors = F)

names(data_s1) = tolower(names(data_s1))
names(data_s2) = tolower(names(data_s2))

var_sel = c("fthg", "ftag","hometeam","awayteam", "b365h","b365d","b365a",
            "whh","whd","wha","hst","ast","ftr")

sel_1 = which(names(data_s1) %in% var_sel)
sel_2 = which(names(data_s2) %in% var_sel)


# Selección de variables para construir las posibles variables res --------

data_s1_sel = data_s1[,sel_1]
data_s2_sel = data_s2[,sel_2]

# Identificar equipos que descendieron en S1 y los equipos nuevos en S2
`%notin%` <- Negate(`%in%`)
teams_s1 = unique(data_s1_sel$hometeam)
teams_s2 = unique(data_s2_sel$hometeam)

desc = teams_s1[which(teams_s1 %notin% teams_s2)]
asc =teams_s2[(teams_s2 %notin% teams_s1)]

# Calculo del promedio de remates de la temporada anterior s1 2018

hstm_18 = data_s1_sel %>%  mutate(hometeam = replace(hometeam,which(hometeam %in% desc), "desc")) %>% 
  group_by(hometeam) %>% summarise(stm_18 = mean(hst)) %>% rbind(.,cbind(hometeam = asc, .[which(.$hometeam=="desc"),2]) )

astm_18 = data_s1_sel %>% mutate(awayteam = replace(awayteam,which(awayteam %in% desc), "desc")) %>% 
  group_by(awayteam) %>% summarise(stm_18 = mean(ast)) %>% rbind(.,cbind(awayteam = asc, .[which(.$awayteam=="desc"),2]) )



# Calculo del promedio de remates de la temporada actual s2 2019
hstm_19 = data_s2_sel %>% group_by(hometeam) %>% summarise(stm_19 = mean(hst))
astm_19 = data_s2_sel %>% group_by(awayteam) %>% summarise(stm_19 = mean(ast))

# Puntos promedio temp anterior s1 2018

data_s1_sel$pointh =  ifelse(data_s1_sel$ftr == "H",3, ifelse( data_s1_sel$ftr == "D",1,0))
data_s1_sel$pointa =  ifelse(data_s1_sel$ftr == "A",3, ifelse( data_s1_sel$ftr == "D",1,0))

pointhm_18 = data_s1_sel %>%  mutate(hometeam = replace(hometeam,which(hometeam %in% desc), "desc")) %>% 
  group_by(hometeam) %>% summarise(point_18 = mean(pointh)) %>% rbind(.,cbind(hometeam = asc, .[which(.$hometeam=="desc"),2]) )

pointam_18 = data_s1_sel %>% mutate(awayteam = replace(awayteam,which(awayteam %in% desc), "desc")) %>% 
  group_by(awayteam) %>% summarise(point_18 = mean(pointa)) %>% rbind(.,cbind(awayteam = asc, .[which(.$awayteam=="desc"),2]) )

# Puntos promedio temp actual s2 2019

data_s2_sel$pointh =  ifelse(data_s2_sel$ftr == "H",3, ifelse( data_s2_sel$ftr == "D",1,0))
data_s2_sel$pointa =  ifelse(data_s2_sel$ftr == "A",3, ifelse( data_s2_sel$ftr == "D",1,0))

pointhm_19 = data_s2_sel %>% group_by(hometeam) %>% summarise(point_19 = mean(pointh))
pointam_19 = data_s2_sel %>% group_by(awayteam) %>% summarise(point_19 = mean(pointa))


# Goles promedio temp anterior s1 2018
goalhm_18 = data_s1_sel %>%  mutate(hometeam = replace(hometeam,which(hometeam %in% desc), "desc")) %>% 
  group_by(hometeam) %>% summarise(goal_18 = mean(fthg)) %>% rbind(.,cbind(hometeam = asc, .[which(.$hometeam=="desc"),2]) )

goalam_18 = data_s1_sel %>% mutate(awayteam = replace(awayteam,which(awayteam %in% desc), "desc")) %>% 
  group_by(awayteam) %>% summarise(goal_18 = mean(ftag)) %>% rbind(.,cbind(awayteam = asc, .[which(.$awayteam=="desc"),2]) )

# Goles promedio temp actual s2 2019

goalhm_19 = data_s2_sel %>% group_by(hometeam) %>% summarise(goal_19 = mean(fthg))
goalam_19 = data_s2_sel %>% group_by(awayteam) %>% summarise(goal_19 = mean(ftag))

# Construcción de la base de datos con las variables calculadas
data_home = data.frame(home=1, goals=data_s2_sel$fthg,
               team=data_s2_sel$hometeam,
               opponent=data_s2_sel$awayteam, 
               whw = data_s2_sel$whh, whd = data_s2_sel$whd,
               b365w = data_s2_sel$b365h, b365d = data_s2_sel$b365d) %>%
               merge(.,hstm_18, by.x = "team" , by.y = "hometeam") %>%
               merge(.,hstm_19, by.x = "team" , by.y = "hometeam") %>% 
               merge(.,pointhm_18, by.x = "team" , by.y = "hometeam") %>% 
               merge(.,pointhm_19, by.x = "team" , by.y = "hometeam") %>% 
               merge(.,goalhm_18, by.x = "team" , by.y = "hometeam") %>% 
               merge(.,goalhm_19, by.x = "team" , by.y = "hometeam")


data_away = data.frame(home=0, goals=data_s2_sel$ftag,
               team=data_s2_sel$awayteam,
               opponent=data_s2_sel$hometeam,
               whw = data_s2_sel$wha, whd = data_s2_sel$whd,
               b365w = data_s2_sel$b365a, b365d = data_s2_sel$b365d) %>% 
               merge(.,astm_18, by.x = "team" , by.y = "awayteam") %>% 
               merge(.,astm_19, by.x = "team" , by.y = "awayteam") %>% 
               merge(.,pointam_18, by.x = "team" , by.y = "awayteam") %>% 
               merge(.,pointam_19, by.x = "team" , by.y = "awayteam") %>% 
               merge(.,goalam_18, by.x = "team" , by.y = "awayteam") %>% 
               merge(.,goalam_19, by.x = "team" , by.y = "awayteam")
