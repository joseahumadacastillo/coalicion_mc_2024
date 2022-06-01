#----------------------------------
# (1)Packages:
#-----------------------------------
# install.packages("ggplot2")
#install.packages("patchwork")
#install.packages("janitor")
#install.packages("tidyverse")
require(ggplot2)
library(tidyverse)
library(ggthemes)
library(ggrepel)
install.packages("car")
library(car)
#library(dslabs)
#library(broom)
#library(moments)
#library(quantmod)
#library(lmtest)
#library(sandwich)
#library("nlme")
#library("ivreg")
#library("skedastic")
#library("strucchange")
#library("ARDL")
#library("tseries")
#library("egcm")
#library("janitor")

#-------------------------------------------------------------------------------
# data y modificando lenghts
#-------------------------------------------------------------------------------
getwd()
setwd("C:/Users/Jose Ahumada/Desktop/mapa distritos federales/mapas_electorales_secciones/SECCION")

diputaciones2021 <- readRDS('diputaciones-2021.rds')%>%
  filter(unidad_territorial!="NA")%>%filter(unidad_territorial!="Inf")


partidos21 <- diputaciones2021[, c("pan", "pri", "prd", "pvem", "pt", "morena", "pes", "pan_pri_prd", "pan_pri", "pan_prd", "pri_prd", "pvem_pt_morena", "pvem_pt", "pvem_morena", "pt_morena" )]
partidos21$ganador21 = names(partidos21)[apply(partidos21, 1, which.max)]


revocacion <- readRDS("revocacion.rds")%>%
  filter(lista_nominal22!=0)


#length(diputaciones2021$unidad_territorial)
#length(revocacion$unidad_territorial)
#length(revocacion$total_votos22)
#length(revocacion$lista_nominal22)

#-------------------------------------------------------------------------------
# transformaciones
#-------------------------------------------------------------------------------

revocacion <- mutate(revocacion, participacion = total_votos22 / lista_nominal22)
revocacion <- mutate(revocacion, p_participacion = participacion * 100) %>%
  mutate(p_abstencion = 100 -p_participacion)%>%
  mutate(p_que_se_quede = ((que_se_quede / lista_nominal22) *100))%>%
  mutate(p_que_se_vaya = ((que_se_vaya / lista_nominal22) *100))%>%
  mutate(p_nulos22 = ((nulos22 / lista_nominal22) *100))%>%
  filter( p_que_se_quede < 100)

diputaciones2021 <- mutate(diputaciones2021, p_pan = ((pan / lista_nominal21)*100)) %>%
  mutate(p_pri = ((pri / lista_nominal21) *100)) %>%
  mutate(p_prd = ((prd / lista_nominal21) *100)) %>%
  mutate(p_total_morena = ((total_morena / lista_nominal21) *100))%>%
  mutate(p_nulos21 = ((nulos21 / lista_nominal21) *100))


data_diputaciones_revocacion <- left_join(revocacion, diputaciones2021)
data_diputaciones_revocacion <- left_join(data_diputaciones_revocacion, partidos21)# de aqui va a haber que borrar las columnas de partidos que se repitan
data_diputaciones_revocacion <- data_diputaciones_revocacion%>%filter( p_que_se_quede < 100) # se eliminaron 2 casos por fallas atribuibles a capturas



#-------------------------------------------------------------------------------
# Nacional
#-------------------------------------------------------------------------------

lle

# max(data_diputaciones_revocacion$p_que_se_quede)# probando lenghts

Pan_SeQuede <- data_diputaciones_revocacion %>% ggplot(aes(x= p_pan, y=p_que_se_quede))+
  geom_point(size = .5, color = "blue")+
  geom_smooth(method = lm, se = FALSE)+
  labs(x=" % de Votos por Diputaciones Federales para PAN 2021",
       y="% de Votos AMLO se quede",
       title="% de Votos por el PAN  2021 vs \n 5 de Votos para que AMLO se quede 2022",
       subtitle="Por unidad territorial", caption = "José Ahumada Castillo") +
  theme_economist()

print(Pan_SeQuede)


#-------------------------------------------------------------------------------
# Yucatán
#-------------------------------------------------------------------------------
yucatandip <- data_diputaciones_revocacion%>%
  filter(unidad_territorial!="NA")%>%
  filter(cve_ent==31)

#length(yucatandip$pan)

yucatanrev <- data_diputaciones_revocacion%>%
  filter(unidad_territorial!="NA")%>%
  filter(cve_ent==31)

#length(yucatanrev$unidad_territorial)

plot(x=yucatandip$pan, y=yucatanrev$que_se_vaya)
plot(x=yucatandip$pan,y=yucatanrev$que_se_quede)
plot(x=yucatandip$total_morena,y=yucatanrev$que_se_vaya)
plot(x=yucatandip$total_morena,y=yucatanrev$que_se_quede)
plot(x=yucatandip$pan, y=yucatanrev$participacion)
plot(x=yucatandip$total_morena, y=yucatanrev$participacion)

plot(x=yucatandip$p_pan, y=yucatanrev$p_que_se_vaya)
plot(x=yucatandip$p_pan,y=yucatanrev$p_que_se_quede)
plot(x=yucatandip$p_total_morena,y=yucatanrev$p_que_se_vaya)
plot(x=yucatandip$p_total_morena,y=yucatanrev$p_que_se_quede)
plot(x=yucatandip$p_pan, y=yucatanrev$p_participacion)
plot(x=yucatandip$p_total_morena, y=yucatanrev$p_participacion)

scatterplot(x=yucatandip$pan, y=yucatanrev$que_se_vaya, col = "blue")
scatterplot(x=yucatandip$pan,y=yucatanrev$que_se_quede, col = "blue")
scatterplot(x=yucatandip$total_morena,y=yucatanrev$que_se_vaya, col = "violetred4")
scatterplot(x=yucatandip$total_morena,y=yucatanrev$que_se_quede, col = "violetred4")

marti scatterplot(x=yucatandip$pan, y=yucatanrev$participacion, col = "blue")
scatterplot(x=yucatandip$total_morena, y=yucatanrev$participacion, col = "violetred4")

scatterplot(x=yucatandip$p_pan, y=yucatanrev$p_que_se_vaya)
scatterplot(x=yucatandip$p_pan,y=yucatanrev$p_que_se_quede)
scatterplot(x=yucatandip$p_total_morena,y=yucatanrev$p_que_se_vaya)
scatterplot(x=yucatandip$p_total_morena,y=yucatanrev$p_que_se_quede)
scatterplot(x=yucatandip$p_pan, y=yucatanrev$p_participacion)
scatterplot(x=yucatandip$p_total_morena, y=yucatanrev$p_participacion)


lmodel_pan_vaya <- lm(yucatanrev$que_se_vaya ~ yucatandip$pan)
summary(lmodel_pan_vaya)

lmodel_pan_quede <- lm(yucatanrev$que_se_quede~ yucatandip$pan)
summary(lmodel_pan_quede)

lmodel_morena_vaya <- lm(yucatanrev$que_se_vaya ~ yucatandip$total_morena)
summary(lmodel_morena_vaya)

lmodel_morena_quede <- lm(yucatanrev$que_se_quede ~ yucatandip$total_morena)
summary(lmodel_morena_quede)

#-------------------------------------------------------------------------------
# CDMX
#-------------------------------------------------------------------------------

cdmx_dip <- diputaciones2021%>%
  filter(unidad_territorial!="NA")%>%
  filter(cve_ent == "09")

cdmx_rev <- revocacion%>%
  filter(cve_ent == "09")

plot(x=cdmx_dip$pan, y=cdmx_rev$que_se_vaya)
plot(x=cdmx_dip$pan,y=cdmx_rev$que_se_quede)
plot(x=cdmx_dip$total_morena,y=cdmx_rev$que_se_vaya)
plot(x=cdmx_dip$total_morena,y=cdmx_rev$que_se_quede)

plot(x=cdmx_dip$p_pan, y=cdmx_rev$p_que_se_vaya)
plot(x=cdmx_dip$p_pan,y=cdmx_rev$p_que_se_quede)
plot(x=cdmx_dip$p_total_morena,y=cdmx_rev$p_que_se_vaya)
plot(x=cdmx_dip$p_total_morena,y=cdmx_rev$p_que_se_quede)

scatterplot(x=cdmx_dip$pan, y=cdmx_rev$que_se_vaya)
scatterplot(x=cdmx_dip$pan,y=cdmx_rev$que_se_quede)
scatterplot(x=cdmx_dip$total_morena,y=cdmx_rev$que_se_vaya)
scatterplot(x=cdmx_dip$total_morena,y=cdmx_rev$que_se_quede)

scatterplot(x=cdmx_dip$p_pan, y=cdmx_rev$p_que_se_vaya)
scatterplot(x=cdmx_dip$p_pan,y=cdmx_rev$p_que_se_quede)
scatterplot(x=cdmx_dip$p_total_morena,y=cdmx_rev$p_que_se_vaya)
scatterplot(x=cdmx_dip$p_total_morena,y=cdmx_rev$p_que_se_quede)

