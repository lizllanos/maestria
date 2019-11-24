 
# Proyecto Analítica Fútbol -----------------------------------------------

# Lizeth Llanos y Diego Agudelo
# Septiembre 2019
# Script para proyecto de Análisis Exploratorio de datos

# Cargar librerías
options(stringsAsFactors = F)

library(dplyr)
library(tidyverse)
library(purrr)


#Configurar directorio de trabajo
setwd("C:/Users/lllanos/Dropbox/ICESI/Semestre I/Análisis exploratorio de datos/Proyecto_grupal")

# Cargar bases de datos
data_s1 = read.csv("season-1718_csv.csv", header = T, stringsAsFactors = F)
data_s2 = read.csv("season-1819_csv.csv", header = T, stringsAsFactors = F)

names(data_s1) = tolower(names(data_s1))
names(data_s2) = tolower(names(data_s2))


# Selección de variables para construir las posibles variables res --------

data_s1_sel = data_s1
data_s2_sel = data_s2

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


# Goles promedio temp anterior s1 2018
goalhm_18 = data_s1_sel %>%  mutate(hometeam = replace(hometeam,which(hometeam %in% desc), "desc")) %>% 
  group_by(hometeam) %>% summarise(goal_18 = mean(fthg)) %>% rbind(.,cbind(hometeam = asc, .[which(.$hometeam=="desc"),2]) )

goalam_18 = data_s1_sel %>% mutate(awayteam = replace(awayteam,which(awayteam %in% desc), "desc")) %>% 
  group_by(awayteam) %>% summarise(goal_18 = mean(ftag)) %>% rbind(.,cbind(awayteam = asc, .[which(.$awayteam=="desc"),2]) )


# Construcción de la base de datos con las variables calculadas
data_home = data.frame(date =data_s2_sel$date, home="Local", 
               team=data_s2_sel$hometeam, goalt=data_s2_sel$fthg,
               opponent=data_s2_sel$awayteam, goalo=data_s2_sel$ftag,
               point_19 = data_s2_sel$pointh, ts = data_s2_sel$hs, tr = data_s2_sel$hr, 
               ty = data_s2_sel$hy, tc = data_s2_sel$hc, tf = data_s2_sel$hf,
               b365t = data_s2_sel$b365h,wht = data_s2_sel$whh,b365o = data_s2_sel$b365a,who = data_s2_sel$wha,
               st_19 = data_s2_sel$hst ) %>%
               merge(.,hstm_18, by.x = "team" , by.y = "hometeam") %>%
               merge(.,pointhm_18, by.x = "team" , by.y = "hometeam") %>% 
               merge(.,goalhm_18, by.x = "team" , by.y = "hometeam") 

data_away = data.frame(date =data_s2_sel$date, home="Visitante",
               team=data_s2_sel$awayteam, goalt=data_s2_sel$ftag,
               opponent=data_s2_sel$hometeam, goalo=data_s2_sel$fthg,
               point_19 = data_s2_sel$pointa, ts = data_s2_sel$as, tr = data_s2_sel$ar, 
               ty = data_s2_sel$ay, tc = data_s2_sel$ac, tf = data_s2_sel$af,
               b365t = data_s2_sel$b365a, wht = data_s2_sel$wha, b365o = data_s2_sel$b365h,who = data_s2_sel$whh,
               st_19 = data_s2_sel$ast) %>% 
               merge(.,astm_18, by.x = "team" , by.y = "awayteam") %>% 
               merge(.,pointam_18, by.x = "team" , by.y = "awayteam") %>% 
               merge(.,goalam_18, by.x = "team" , by.y = "awayteam") 

epl = rbind(data_away, data_home) 
epl$date = as.Date(epl$date, "%d/%m/%Y")
epl$home = as.factor(epl$home)

epl = epl %>%  arrange(.,date)%>% arrange(.,team) %>% 
  #Creación del promedio de goles acumulado para s2 2019
  group_by(team)%>% arrange(.,date)  %>% mutate(goaltm_19=cumsum(goalt)/seq_along(goalt))%>% 
  arrange(.,date) %>% arrange(.,team)%>% mutate(goaltm_19=c(NA,goaltm_19[-n()])) %>% 
  
  group_by(team)%>% arrange(.,date)  %>% mutate(goalom_19=cumsum(goalo)/seq_along(goalo))%>% 
  arrange(.,date) %>% arrange(.,team)%>% mutate(goalom_19=c(NA,goalom_19[-n()])) %>% 
  
  #Creación del promedio de puntos acumulado para s2 2019
  group_by(team)%>% arrange(.,date)  %>% mutate(pointm_19=cumsum(point_19)/seq_along(point_19))%>% 
  arrange(.,date) %>% arrange(.,team)%>% mutate(pointm_19=c(NA,pointm_19[-n()])) %>% 
  #Creación del promedio de remates acumulado para s2 2019
  group_by(team)%>% arrange(.,date)  %>% mutate(stm_19=cumsum(st_19)/seq_along(st_19))%>% 
  arrange(.,date) %>% arrange(.,team)%>% mutate(stm_19=c(NA,stm_19[-n()])) 
  



# Pregunta 1: Distribución Poisson ----------------------------------------

#Variable respuesta: goals
my_post_theme=
  theme_minimal() +
  theme(axis.text.x = element_text(face="bold", color="#666666", 
                                   size=10, margin = margin(t = -5)),
        axis.title = element_text(color="black", face="bold",size=12),
        plot.title = element_text(color="black", face="bold", size=14),
        axis.text.y = element_text(face="bold", color="#666666", 
                                   size=10),
        legend.text=element_text(size=12),
        legend.title=element_text(size=13),
        legend.key=element_blank(),
        axis.ticks.length=unit(0, "cm"))


# Gráfica de ajuste de los goles a una distribución poisson
epl %>% group_by(goalt) %>% summarize(actual=n()/nrow(.)) %>% 
  mutate(pred=dpois(0:max(epl$goalt), 
                    mean(epl$goalt))) %>% 
  ggplot(aes(x=as.factor(goalt))) + 
  geom_bar(aes(y=actual, fill="Observado"), stat="identity",position="dodge") +
  geom_line(aes( y = pred,group = 1, color="Estimado (Poisson)"),size=1.25)  +
  scale_fill_manual(values=c("#20B2AA"), name = " ") +
  scale_color_manual(values=c("#CD5C5C"),name=" ")  +
  ggtitle("Número de goles por partido (Temporada Liga Premier 2018/19)")  + 
  xlab("Goles por partido") + ylab("Porcentaje de partidos") +
  my_post_theme


mean(epl$goalt)
var(epl$goalt)

summary(fitdist(epl$goalt,"nbinom"))
summary(fitdist(epl$goalt,"pois"))

library(vcd)
gf<-goodfit(epl$goalt,type= "poisson",method= "MinChisq")
summary(gf)
plot(gf)


epl %>% group_by(home, goalt) %>% summarize(actual=n()/nrow(.)) %>% 
  mutate(pred=dpois(0:max(epl$goalt), 
                    mean(epl$goalt))) %>% 
  ggplot(aes(x=as.factor(goalt))) + 
  geom_bar(aes(y=actual, fill=home), stat="identity",position="dodge") +
  geom_line(aes( y = pred,group = home, color=home),size=1.25)  +
  scale_fill_manual(values=c("#20B2AA"), name = " ") +
  scale_color_manual(values=c("#CD5C5C"),name=" ")  +
  ggtitle("Número de goles por partido (Temporada Liga Premier 2018/19)")  + 
  xlab("Goles por partido") + ylab("Porcentaje de partidos") +
  my_post_theme
# Pregunta 2: Estadística de la temporada actual --------------------------

# Promedio de goles a favor y en contra
# Promedio de remates
# Promedio de remates a puerta
# Tiros de esquina
# Faltas
# Tarjetas amarillas y rojas

st_gen = epl %>% group_by(home) %>% summarise(Goles_a_favor= mean(goalt),Goles_en_contra= mean(goalo),
                                      Remates = mean(ts), Remates_a_puerta = mean(st_19),
                                      Tiros_esquina = mean(tc), Faltas = mean(tf), Tarjetas_amarillas = mean(ty),
                                      Tarjetas_rojas = mean(tr) )

st_by_team = epl %>% group_by(home,team) %>% summarise(Goles_a_favor= mean(goalt),Goles_en_contra= mean(goalo),
                                        Remates = mean(ts), Remates_a_puerta = mean(st_19),
                                        Tiros_esquina = mean(tc), Faltas = mean(tf), Tarjetas_amarillas = mean(ty),
                                        Tarjetas_rojas = mean(tr) ) %>% gather("stat","mean",-home,-team)

x11()
ggplot(st_by_team, aes(x = home, y = team, fill = mean)) + geom_tile() +
  scale_fill_gradient2(low = "red", high = "green", mid = "black") +facet_grid(~stat,scales = 'free')



lapply(unique(st_by_team$stat), function(cc) {
  gg <- ggplot(filter(st_by_team,stat==cc),
               aes(x=home, y=team, fill=mean))+
     geom_tile() +
    scale_fill_gradient2(low = "red", high = "green")
  gg})-> cclist

cclist[["ncol"]] <- 4
library(gridExtra)
do.call(grid.arrange, cclist)


ph = data_s2_sel %>% group_by(team =hometeam) %>% summarise(point = sum(pointh))
pa = data_s2_sel %>% group_by(team =awayteam) %>% summarise(point = sum(pointa))
merge(ph,pa, by = "team") %>% mutate(sum = point.x+point.y) %>% arrange(.,desc(sum))


# goals per teams (e.g. Chelsea & Sunderland)

inner_join(epl %>% filter(team %in% c("Man City","Huddersfield")) %>% group_by(team,home,goalt) %>% summarize(num_games=n()),
epl %>% group_by(team) %>% summarize(tot_games=n()/2),
by=c("team")) %>% mutate(actual=num_games/tot_games)  %>%  complete(home,goalt, fill = list(actual = 0)) %>% 
  complete(nesting(team,home), fill = list(actual = 0)) %>% 

ggplot(aes(x=as.factor(goalt),fill=team)) + 
  geom_bar(aes(y = actual),stat="identity",position="dodge") +
 # geom_col(position = position_dodge2(width = 0.9, preserve = "single"))+
  facet_grid(home ~ .) +


  scale_fill_manual(values=c("#034694", "cornflowerblue"), 
                    name = " ",
                    
                    guide = guide_legend(override.aes = list(linetype = c(0,1)))) +
    
  ggtitle("Goles por partido (Temporada 2018/19)")  + xlab("Goles") + ylab("Porcentaje de partidos") +
  my_post_theme +
  theme(strip.text.y = element_text(size=12, face="bold"),
        strip.background = element_rect(colour="black", fill="gray")) 

# Pregunta 3: Selección de posibles predictores ---------------------------

var_sel = c("date","fthg", "ftag","hometeam","awayteam", "b365h","b365a", "hf","af",
            "whh","wha","hst","ast", "hs", "as","ftr", "hc", "ac","hy", "ay" ,"hr", "ar", "hc", "ac")

sel_1 = which(names(data_s1) %in% var_sel)
sel_2 = which(names(data_s2) %in% var_sel)


v_class = unlist(lapply(sapply(epl,class), '[[', 1))
epl_cual = epl [ ,which(v_class== "character" | v_class== "factor")]
epl_num = epl [ ,which(v_class != "character" & v_class != "factor")[-1]]

apply(epl_num,2, function(col)cor(col, epl_num$goalt,use = "complete.obs"))

