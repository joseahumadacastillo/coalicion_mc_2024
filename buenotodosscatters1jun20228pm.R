#Proyecto:
#  MC y la Coalicion Opositora
# Autor: Jose Ahumada Castillo Tw @AhumadaReal
#_______________________________________________________________________________
#
#_______________________________________________________________________________

rm(list = ls())#Clears variable environment
cat("\014")     #Clears console


#_______________________________________________________________________________
# (1)Packages:
#_______________________________________________________________________________

# install.packages("ggplot2")
#install.packages("patchwork")
#install.packages("ggforce")
#install.packages("ggrepel")
#install.packages("janitor")
#install.packages("tidyverse")
#install.packages("reshape2")
#install.packages("ggpubr")
#install.packages("car")

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
#library(ggforce)

library(tidyverse)
library(reshape2)
library(ggrepel)
library(ggthemes)
library(janitor)
library(patchwork)
library(ggpubr)
library(sf)



################################################################################
################################# scatterplots #################################
################################## Unidades, distritales, 2018, 2021, 2022######
################################################################################

############# Unidades Territoriales diputaciones 2021 revocacion 2022 #########

setwd("C:/Users/Jose Ahumada/Desktop/github/coalicion_mc_2024")

unidades <- read.csv("inputs/CATALOGO_UNIDADES_TERRITORIALES_RM2022.csv") %>%
clean_names() %>%
  rename(id_estado = id_entidad)%>%
  select(id_estado,
         entidad,
         id_distrito_federal,
         seccion,
         unidad_territorial)
saveRDS(unidades , ("outputs/unidades_territoriales_revocacion_2022.rds"))


diputaciones_2021 <- read.csv("inputs/diputaciones2021.csv")%>%
  clean_names() %>%
  filter(seccion!=0) %>%
  select(id_estado, nombre_estado, id_distrito, nombre_distrito, seccion, pan, pri, prd, pvem, pt, mc, morena, votos_nulos, total_votos_calculados, lista_nominal_casilla) %>%
  rename(pan_2021 = pan,
         pri_2021 = pri,
         prd_2021 = prd,
         pvem_2021 = pvem,
         pt_2021 = pt,
         mc_2021 = mc,
         morena_2021 = morena,
         votos_nulos_2021 = votos_nulos,
         total_votos_calculados_2021 = total_votos_calculados,
         lista_nominal_2021 = lista_nominal_casilla)

saveRDS(diputaciones_2021 , ("outputs/diputaciones_2021_seccional.rds"))


diputaciones_unidades <- left_join(diputaciones_2021, unidades)%>%
  group_by(id_estado, unidad_territorial)%>%
  summarize(pan_2021        = sum(pan_2021, na.rm=T),
            pri_2021        = sum(pri_2021, na.rm=T),
            prd_2021        = sum(prd_2021, na.rm=T),
            pvem_2021       = sum(pvem_2021, na.rm=T),
            pt_2021 = sum(pt_2021, na.rm=T),
            mc_2021 = sum(mc_2021, na.rm=T),
            morena_2021 = sum(morena_2021, na.rm=T),
            votos_nulos_2021  = sum(votos_nulos_2021, na.rm=T),
            total_votos_calculados_2021 = sum(total_votos_calculados_2021, na.rm=T),
            lista_nominal_2021 = sum(lista_nominal_2021, na.rm=T)) %>%
  ungroup()

saveRDS(diputaciones_unidades , ("outputs/diputaciones_2021_unidades.rds"))


revocacion <- read.csv("inputs/20220411_1845_COMPUTOS_RM2022.csv")
revocacion <- revocacion %>%
  clean_names() %>%
  rename(id_estado = id_entidad) %>%
  filter(id_estado != 0) %>%
  rename(revoque = que_se_le_revoque_el_mandato_por_perdida_de_la_confianza) %>%
  rename(siga = que_siga_en_la_presidencia_de_la_republica) %>%
  rename(nulos_2022 = nulos) %>%
  rename(total_votos_calculados_2022 = total_votos_calculados) %>%
  rename(lista_nominal_2022 = lista_nominal) %>%
  select(id_estado,
         entidad,
         id_distrito_federal,
         distrito_federal,
         seccion,
         ubicacion_casilla,
         revoque,
         siga,
         nulos_2022,
         total_votos_calculados_2022,
         lista_nominal_2022)

saveRDS(revocacion , ("outputs/revocacion_2022_seccional.rds"))


revocacion_unidades <- left_join(revocacion, unidades)%>%
  group_by(id_estado, unidad_territorial)%>%
  summarize(revoque=sum(revoque, na.rm=T),
            siga=sum(siga, na.rm=T),
            nulos_2022=sum(nulos_2022, na.rm=T),
            total_votos_calculados_2022=sum(total_votos_calculados_2022, na.rm=T),
            lista_nominal_2022=sum(lista_nominal_2022, na.rm=T))

saveRDS(revocacion_unidades , ("outputs/revocacion_unidades.rds"))


diputaciones_revocacion_unidades <- left_join(diputaciones_unidades, revocacion_unidades) %>%
  group_by(id_estado, unidad_territorial)%>%
  summarize(pan_2021                       = sum(pan_2021, na.rm=T),
            pri_2021                       = sum(pri_2021, na.rm=T),
            prd_2021                       = sum(prd_2021, na.rm=T),
            pvem_2021                      = sum(pvem_2021, na.rm=T),
            pt_2021                        = sum(pt_2021, na.rm=T),
            mc_2021                        = sum(mc_2021, na.rm=T),
            morena_2021                    = sum(morena_2021, na.rm=T),
            votos_nulos_2021               = sum(votos_nulos_2021, na.rm=T),
            total_votos_calculados_2021    = sum(total_votos_calculados_2021, na.rm=T),
            lista_nominal_2021             = sum(lista_nominal_2021, na.rm=T),
            revoque                        =sum(revoque, na.rm=T),
            siga                           =sum(siga, na.rm=T),
            nulos_2022                     =sum(nulos_2022, na.rm=T),
            total_votos_calculados_2022    =sum(total_votos_calculados_2022, na.rm=T),
            lista_nominal_2022             =sum(lista_nominal_2022, na.rm=T))

diputaciones_revocacion_unidades <- diputaciones_revocacion_unidades %>%
  mutate(p_pan_2021 = ((pan_2021 / total_votos_calculados_2021 ) * 100),
         p_pri_2021 = ((pri_2021 / total_votos_calculados_2021)* 100),
         p_prd_2021 = ((prd_2021 / total_votos_calculados_2021)* 100),
         p_pvem_2021 = ((pvem_2021 / total_votos_calculados_2021)* 100),
         p_pt_2021 = ((pt_2021 / total_votos_calculados_2021)* 100),
         p_mc_2021 = ((mc_2021 / total_votos_calculados_2021)* 100),
         p_morena_2021 = ((morena_2021 / total_votos_calculados_2021)* 100),
         p_revoque = ((revoque / total_votos_calculados_2022)* 100),
         p_siga = ((siga / total_votos_calculados_2022 )* 100))

diputaciones_revocacion_unidades <- na.omit(diputaciones_revocacion_unidades) # en algunos casos la votacion por revocacion fue cero por lo que quedad NaN, se procede a removerlos

saveRDS(diputaciones_revocacion_unidades , ("outputs/diputaciones_revocacion_unidades.rds"))


############# Distritos diputaciones 2021 revocacion 2022 ######################


setwd("C:/Users/Jose Ahumada/Desktop/github/coalicion_mc_2024")

diputaciones_2021 <- read.csv("inputs/diputaciones2021.csv")%>%
  clean_names() %>%
  filter(seccion!=0) %>%
  select(id_estado, nombre_estado, id_distrito, nombre_distrito, seccion, pan, pri, prd, pvem, pt, mc, morena, votos_nulos, total_votos_calculados, lista_nominal_casilla) %>%
  rename(pan_2021 = pan,
         pri_2021 = pri,
         prd_2021 = prd,
         pvem_2021 = pvem,
         pt_2021 = pt,
         mc_2021 = mc,
         morena_2021 = morena,
         votos_nulos_2021 = votos_nulos,
         total_votos_calculados_2021 = total_votos_calculados,
         lista_nominal_2021 = lista_nominal_casilla)






revocacion <- read.csv("inputs/20220411_1845_COMPUTOS_RM2022.csv") %>%
  clean_names() %>%
filter(id_entidad != 0) %>%
select(id_entidad,
         entidad,
         id_distrito_federal,
         distrito_federal,
         que_se_le_revoque_el_mandato_por_perdida_de_la_confianza,
         que_siga_en_la_presidencia_de_la_republica,
         nulos,
         total_votos_calculados,
         lista_nominal) %>%
  rename(id_estado = id_entidad,
         nombre_entidad = entidad,
         id_distrito = id_distrito_federal,
         nombre_distrito = distrito_federal,
         revoque = que_se_le_revoque_el_mandato_por_perdida_de_la_confianza,
         siga = que_siga_en_la_presidencia_de_la_republica,
         nulos_2022 = nulos,
         total_votos_calculados_2022 = total_votos_calculados,
         lista_nominal_2022 = lista_nominal)


diputaciones_2021_revocacion_distrito <- left_join(diputaciones_2021, revocacion)%>%
  group_by(id_estado, id_distrito)%>%
  summarize(pan_2021                       = sum(pan_2021, na.rm=T),
            pri_2021                       = sum(pri_2021, na.rm=T),
            prd_2021                       = sum(prd_2021, na.rm=T),
            pvem_2021                      = sum(pvem_2021, na.rm=T),
            pt_2021                        = sum(pt_2021, na.rm=T),
            mc_2021                        = sum(mc_2021, na.rm=T),
            morena_2021                    = sum(morena_2021, na.rm=T),
            votos_nulos_2021               = sum(votos_nulos_2021, na.rm=T),
            total_votos_calculados_2021    = sum(total_votos_calculados_2021, na.rm=T),
            lista_nominal_2021             = sum(lista_nominal_2021, na.rm=T),
            revoque                        =sum(revoque, na.rm=T),
            siga                           =sum(siga, na.rm=T),
            nulos_2022                     =sum(nulos_2022, na.rm=T),
            total_votos_calculados_2022    =sum(total_votos_calculados_2022, na.rm=T),
            lista_nominal_2022             =sum(lista_nominal_2022, na.rm=T))

diputaciones_2021_revocacion_distrito <- diputaciones_2021_revocacion %>%
  mutate(p_pan_2021 = ((pan_2021 / total_votos_calculados_2021 ) * 100),
         p_pri_2021 = ((pri_2021 / total_votos_calculados_2021)* 100),
         p_prd_2021 = ((prd_2021 / total_votos_calculados_2021)* 100),
         p_pvem_2021 = ((pvem_2021 / total_votos_calculados_2021)* 100),
         p_pt_2021 = ((pt_2021 / total_votos_calculados_2021)* 100),
         p_mc_2021 = ((mc_2021 / total_votos_calculados_2021)* 100),
         p_morena_2021 = ((morena_2021 / total_votos_calculados_2021)* 100),
         p_revoque = ((revoque / total_votos_calculados_2022)* 100),
         p_siga = ((siga / total_votos_calculados_2022 )* 100))

saveRDS(diputaciones_2021_revocacion_distrito , ("diputaciones_2021_revocacion_distrito.rds"))

############# Distritos presidencial 2018  revocacion 2022 ######################


setwd("C:/Users/Jose Ahumada/Desktop/github/coalicion_mc_2024")


presidencial_2018 <- read.csv("inputs/2018_presidencia.csv")
class(presidencial_2018$PAN)
class(presidencial_2018$NOMBRE_DISTRITO)


cols <- names(presidencial_2018)[13:39]                                         #Problema de origen de la base, algunas columnas vienen con datos no numerico
presidencial_2018[cols] <- lapply(presidencial_2018[cols], as.numeric)

class(presidencial_2018$PAN)
class(presidencial_2018$NOMBRE_DISTRITO)



presidencial_2018 <- presidencial_2018 %>%
  clean_names()

presidencial_2018 <- presidencial_2018 %>%
  filter(seccion != 0) %>%
  select(id_estado,
         nombre_estado,
         id_distrito,
         nombre_distrito,
         seccion,
         pan,
         pri,
         prd,
         pvem,
         pt,
         movimiento_ciudadano,
         morena,
         total_votos_calculados,
         lista_nominal_casilla)

presidencial_2018 <- presidencial_2018 %>%
rename(pan_2018 = pan,
         pri_2018 = pri,
         prd_2018 = prd,
         pvem_2018 = pvem,
         pt_2018 = pt,
         mc_2018 = movimiento_ciudadano,
         morena_2018 = morena,
         total_votos_calculados_2018 = total_votos_calculados,
         lista_nominal_2018 =  lista_nominal_casilla )


presidencial_2018 <- presidencial_2018 %>%                                      # Transformacion a nivel seccional
  group_by(id_estado, id_distrito) %>%
  summarize(pan_2018                    = sum(pan_2018 , na.rm = T),
            pri_2018                    = sum(pri_2018 , na.rm = T),
            prd_2018                    = sum(prd_2018 , na.rm = T),
            pvem_2018                   = sum(pvem_2018 , na.rm = T),
            pt_2018                     = sum(pt_2018 , na.rm = T),
            mc_2018                     = sum(mc_2018 , na.rm = T),
            morena_2018                 = sum(morena_2018 , na.rm = T),
            total_votos_calculados_2018 = sum(total_votos_calculados_2018 , na.rm = T),
            lista_nominal_2018  = sum(lista_nominal_2018, na.rm = T))


saveRDS(presidencial_2018, ("presidencial_2018_distritos_federales.rds"))




revocacion_distrital <- read.csv("inputs/20220411_1845_COMPUTOS_RM2022.csv")%>%
  clean_names() %>%
  filter(id_entidad != 0) %>%
  select(id_entidad,
         entidad,
         id_distrito_federal,
         distrito_federal,
         que_se_le_revoque_el_mandato_por_perdida_de_la_confianza,
         que_siga_en_la_presidencia_de_la_republica,
         total_votos_calculados,
         lista_nominal) %>%
  rename(id_estado = id_entidad,
         nombre_estado = entidad,
         id_distrito = id_distrito_federal,
         nombre_distrito = distrito_federal,
         revoque = que_se_le_revoque_el_mandato_por_perdida_de_la_confianza,
         siga = que_siga_en_la_presidencia_de_la_republica,
         total_votos_calculados_2022 = total_votos_calculados,
         lista_nominal_2022 = lista_nominal)%>%
  group_by(id_estado, id_distrito)%>%
  summarize(id_estado                   = id_estado,
            nombre_estado               = nombre_estado,
            id_distrito                 = id_distrito,
            nombre_distrito             = nombre_distrito,
            revoque                     = sum(revoque , na.rm = T),
            siga                        = sum(siga , na.rm = T),
            total_votos_calculados_2022 = sum(total_votos_calculados_2022 , na.rm = T),
            lista_nominal_2022          = sum(lista_nominal_2022 , na.rm = T))


saveRDS(revocacion_distrital , ("revocacion_distrital.rds"))



presidencia_2018_revocacion_distrito <- left_join(presidencial_2018, revocacion_distrital)%>%
  group_by(id_estado, id_distrito)%>%
  summarize(pan_2018                       = sum(pan_2018 , na.rm=T),
            pri_2018                       = sum(pri_2018 , na.rm=T),
            prd_2018                       = sum(prd_2018 , na.rm=T),
            pvem_2018                      = sum(pvem_2018 , na.rm=T),
            pt_2018                         = sum(pt_2018 , na.rm=T),
            mc_2018                         = sum(mc_2018 , na.rm=T),
            morena_2018                    = sum(morena_2018 , na.rm=T),
            total_votos_calculados_2018     = sum(total_votos_calculados_2018 , na.rm=T),
            lista_nominal_2018              = sum(lista_nominal_2018 , na.rm=T),
            revoque                        =sum(revoque, na.rm=T),
            siga                           =sum(siga, na.rm=T),
            total_votos_calculados_2022    =sum(total_votos_calculados_2022, na.rm=T),
            lista_nominal_2022             =sum(lista_nominal_2022, na.rm=T))

presidencia_2018_revocacion_distrito <- presidencia_2018_revocacion_distrito %>%
  mutate(p_pan_2018  = ((pan_2018  / total_votos_calculados_2018  ) * 100),
         p_pri_2018  = ((pri_2018  / total_votos_calculados_2018 )* 100),
         p_prd_2018  = ((prd_2018  / total_votos_calculados_2018 )* 100),
         p_pvem_2018  = ((pvem_2018  / total_votos_calculados_2018 )* 100),
         p_pt_2018  = ((pt_2018  / total_votos_calculados_2018 )* 100),
         p_mc_2018  = ((mc_2018  / total_votos_calculados_2018 )* 100),
         p_morena_2018  = ((morena_2018  / total_votos_calculados_2018 )* 100),
         p_revoque  = ((revoque  / total_votos_calculados_2022 )* 100),
         p_siga = ((siga  / total_votos_calculados_2022 )* 100))


saveRDS(presidencia_2018_revocacion_distrito , ("presidencia_2018_revocacion_distrito"))




#################################################################################



########## scatterplots#########################################################
##### pres 2018 distritos revocacion############################################
################################################################################
# Morena
# Se quede

MORENA_2018_SeQuede_2022_distritos <- presidencia_2018_revocacion_distrito %>% ggplot(aes(x= p_morena_2018, y=p_siga))+
  geom_point(size = 1.5, color = "violetred4")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por Morena, 2018 (%) - Nacional",
       y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por Morena, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(MORENA_2018_SeQuede_2022_distritos)

ggsave("outputs/MORENA_2018_SeQuede_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
# Morena
# Se vaya
MORENA_2018_SeaRevocado_2022_distritos <- presidencia_2018_revocacion_distrito %>% ggplot(aes(x= p_morena_2018, y=p_revoque))+
  geom_point(size = 1.5, color = "violetred4")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por Morena, 2018 (%) - Nacional",
       y="Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por Morena, 2018 (%) - Nacional vs \n Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(MORENA_2018_SeaRevocado_2022_distritos)

ggsave("outputs/MORENA_2018_SeaRevocado_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")


# PAN
# Se quede

pan_2018_SeQuede_2022_distritos <- presidencia_2018_revocacion_distrito %>% ggplot(aes(x= p_pan_2018, y=p_siga))+
  geom_point(size = 1.5, color = "blue")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PAN, 2018 (%) - Nacional",
       y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PAN, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(pan_2018_SeQuede_2022_distritos)

ggsave("outputs/PAN_2018_SeQuede_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
# pan
# Se vaya
pan_2018_SeaRevocado_2022_distritos <- presidencia_2018_revocacion_distrito %>% ggplot(aes(x= p_pan_2018, y=p_revoque))+
  geom_point(size = 1.5, color = "blue")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PAN, 2018 (%) - Nacional",
       y="Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PAN, 2018 (%) - Nacional vs \n Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(pan_2018_SeaRevocado_2022_distritos)

ggsave("outputs/pan_2018_SeaRevocado_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")

# pri
# Se quede

pri_2018_SeQuede_2022_distritos <- presidencia_2018_revocacion_distrito %>% ggplot(aes(x= p_pri_2018, y=p_siga))+
  geom_point(size = 1.5, color = "red")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PRI, 2018 (%) - Nacional",
       y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PRI, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(pri_2018_SeQuede_2022_distritos)

ggsave("outputs/pri_2018_SeQuede_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
# pri
# Se vaya
pri_2018_SeaRevocado_2022_distritos <- presidencia_2018_revocacion_distrito %>% ggplot(aes(x= p_pri_2018, y=p_revoque))+
  geom_point(size = 1.5, color = "red")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PRI, 2018 (%) - Nacional",
       y="Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PRI, 2018 (%) - Nacional vs \n Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(pri_2018_SeaRevocado_2022_distritos)

ggsave("outputs/pri_2018_SeaRevocado_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")



# prd
# Se quede

prd_2018_SeQuede_2022_distritos <- presidencia_2018_revocacion_distrito %>% ggplot(aes(x= p_prd_2018, y=p_siga))+
  geom_point(size = 1.5, color = "gold3")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PRD, 2018 (%) - Nacional",
       y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PRD, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(prd_2018_SeQuede_2022_distritos)

ggsave("outputs/prd_2018_SeQuede_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
# prd
# Se vaya
prd_2018_SeaRevocado_2022_distritos <- presidencia_2018_revocacion_distrito %>% ggplot(aes(x= p_prd_2018, y=p_revoque))+
  geom_point(size = 1.5, color = "gold3")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PRD, 2018 (%) - Nacional",
       y="Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PRD, 2018 (%) - Nacional vs \n Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(prd_2018_SeaRevocado_2022_distritos)

ggsave("outputs/prd_2018_SeaRevocado_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")

# pvem
# Se quede

pvem_2018_SeQuede_2022_distritos <- presidencia_2018_revocacion_distrito %>% ggplot(aes(x= p_pvem_2018, y=p_siga))+
  geom_point(size = 1.5, color = "green")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PVEM, 2018 (%) - Nacional",
       y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PVEM, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(pvem_2018_SeQuede_2022_distritos)

ggsave("outputs/pvem_2018_SeQuede_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
# pvem
# Se vaya
pvem_2018_SeaRevocado_2022_distritos <- presidencia_2018_revocacion_distrito %>% ggplot(aes(x= p_pvem_2018, y=p_revoque))+
  geom_point(size = 1.5, color = "green")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PVEM, 2018 (%) - Nacional",
       y="Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PVEM, 2018 (%) - Nacional vs \n Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(pvem_2018_SeaRevocado_2022_distritos)

ggsave("outputs/pvem_2018_SeaRevocado_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")

# pt
# Se quede

pt_2018_SeQuede_2022_distritos <- presidencia_2018_revocacion_distrito %>% ggplot(aes(x= p_pt_2018, y=p_siga))+
  geom_point(size = 1.5, color = "red3")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PT, 2018 (%) - Nacional",
       y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PT, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(pt_2018_SeQuede_2022_distritos)

ggsave("outputs/pt_2018_SeQuede_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
# pt
# Se vaya
pt_2018_SeaRevocado_2022_distritos <- presidencia_2018_revocacion_distrito %>% ggplot(aes(x= p_pt_2018, y=p_revoque))+
  geom_point(size = 1.5, color = "red3")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PT, 2018 (%) - Nacional",
       y="Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PT, 2018 (%) - Nacional vs \n Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(pt_2018_SeaRevocado_2022_distritos)

ggsave("outputs/pt_2018_SeaRevocado_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")

# mc
# Se quede

mc_2018_SeQuede_2022_distritos <- presidencia_2018_revocacion_distrito %>% ggplot(aes(x= p_mc_2018, y=p_siga))+
  geom_point(size = 1.5, color = "orange")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por MC, 2018 (%) - Nacional",
       y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por MC, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(mc_2018_SeQuede_2022_distritos)

ggsave("outputs/mc_2018_SeQuede_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
# mc
# Se vaya
mc_2018_SeaRevocado_2022_distritos <- presidencia_2018_revocacion_distrito %>% ggplot(aes(x= p_mc_2018, y=p_revoque))+
  geom_point(size = 1.5, color = "orange")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el MC, 2018 (%) - Nacional",
       y="Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el MC, 2018 (%) - Nacional vs \n Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(mc_2018_SeaRevocado_2022_distritos)

ggsave("outputs/mc_2018_SeaRevocado_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
################################################################################################################################
################################################################################
############# ESTADOS RELEVANTES : SCATTERPLOTS 2021-2022 distritos
################################################################################

# JALISCO

jalisco_presidencia_2018_revocacion_distritos <- presidencia_2018_revocacion_distrito%>%
  clean_names() %>%
  filter(id_estado != 0) %>%
  filter(id_estado == 14)
# Morena
# Se quede

jalisco_MORENA_2018_SeQuede_2022_distritos <- jalisco_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_morena_2018, y=p_siga))+
  geom_point(size = 1.5, color = "violetred4")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por Morena, 2018 (%) - Nacional",
       y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por Morena, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(jalisco_MORENA_2018_SeQuede_2022_distritos)

ggsave("outputs/jalisco_MORENA_2018_SeQuede_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
# Morena
# Se vaya
jalisco_MORENA_2018_SeaRevocado_2022_distritos <- jalisco_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_morena_2018, y=p_revoque))+
  geom_point(size = 1.5, color = "violetred4")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por Morena, 2018 (%) - Nacional",
       y="Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por Morena, 2018 (%) - Nacional vs \n Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(jalisco_MORENA_2018_SeaRevocado_2022_distritos)

ggsave("outputs/jalisco_MORENA_2018_SeaRevocado_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")


# PAN
# Se quede

jalisco_pan_2018_SeQuede_2022_distritos <- jalisco_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_pan_2018, y=p_siga))+
  geom_point(size = 1.5, color = "blue")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PAN, 2018 (%) - Nacional",
       y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PAN, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(jalisco_pan_2018_SeQuede_2022_distritos)

ggsave("outputs/jalisco_PAN_2018_SeQuede_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
# pan
# Se vaya
jalisco_pan_2018_SeaRevocado_2022_distritos <- jalisco_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_pan_2018, y=p_revoque))+
  geom_point(size = 1.5, color = "blue")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PAN, 2018 (%) - Nacional",
       y="Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PAN, 2018 (%) - Nacional vs \n Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(jalisco_pan_2018_SeaRevocado_2022_distritos)

ggsave("outputs/jalisco_pan_2018_SeaRevocado_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")

# pri
# Se quede

jalisco_pri_2018_SeQuede_2022_distritos <- jalisco_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_pri_2018, y=p_siga))+
  geom_point(size = 1.5, color = "red")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PRI, 2018 (%) - Nacional",
       y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PRI, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(jalisco_pri_2018_SeQuede_2022_distritos)

ggsave("outputs/jalisco_pri_2018_SeQuede_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
# pri
# Se vaya
jalisco_pri_2018_SeaRevocado_2022_distritos <- jalisco_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_pri_2018, y=p_revoque))+
  geom_point(size = 1.5, color = "red")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PRI, 2018 (%) - Nacional",
       y="Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PRI, 2018 (%) - Nacional vs \n Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(jalisco_pri_2018_SeaRevocado_2022_distritos)

ggsave("outputs/jalisco_pri_2018_SeaRevocado_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")



# prd
# Se quede

jalisco_prd_2018_SeQuede_2022_distritos <- jalisco_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_prd_2018, y=p_siga))+
  geom_point(size = 1.5, color = "gold3")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PRD, 2018 (%) - Nacional",
       y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PRD, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(jalisco_prd_2018_SeQuede_2022_distritos)

ggsave("outputs/jalisco_prd_2018_SeQuede_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
# prd
# Se vaya
jalisco_prd_2018_SeaRevocado_2022_distritos <- jalisco_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_prd_2018, y=p_revoque))+
  geom_point(size = 1.5, color = "gold3")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PRD, 2018 (%) - Nacional",
       y="Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PRD, 2018 (%) - Nacional vs \n Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()


print(jalisco_prd_2018_SeaRevocado_2022_distritos)

ggsave("outputs/jalisco_prd_2018_SeaRevocado_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")

# pvem
# Se quede

jalisco_pvem_2018_SeQuede_2022_distritos <- jalisco_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_pvem_2018, y=p_siga))+
  geom_point(size = 1.5, color = "green")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PVEM, 2018 (%) - Nacional",
       y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PVEM, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(jalisco_pvem_2018_SeQuede_2022_distritos)

ggsave("outputs/jalisco_pvem_2018_SeQuede_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
# pvem
# Se vaya
jalisco_pvem_2018_SeaRevocado_2022_distritos <- jalisco_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_pvem_2018, y=p_revoque))+
  geom_point(size = 1.5, color = "green")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PVEM, 2018 (%) - Nacional",
       y="Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PVEM, 2018 (%) - Nacional vs \n Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(jalisco_pvem_2018_SeaRevocado_2022_distritos)

ggsave("outputs/jalisco_pvem_2018_SeaRevocado_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")

# pt
# Se quede

jalisco_pt_2018_SeQuede_2022_distritos <- jalisco_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_pt_2018, y=p_siga))+
  geom_point(size = 1.5, color = "red3")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PT, 2018 (%) - Nacional",
       y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PT, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(jalisco_pt_2018_SeQuede_2022_distritos)

ggsave("outputs/jalisco_pt_2018_SeQuede_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
# pt
# Se vaya
jalisco_pt_2018_SeaRevocado_2022_distritos <- jalisco_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_pt_2018, y=p_revoque))+
  geom_point(size = 1.5, color = "red3")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PT, 2018 (%) - Nacional",
       y="Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PT, 2018 (%) - Nacional vs \n Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(jalisco_pt_2018_SeaRevocado_2022_distritos)

ggsave("outputs/jalisco_pt_2018_SeaRevocado_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")

# mc
# Se quede

jalisco_mc_2018_SeQuede_2022_distritos <- jalisco_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_mc_2018, y=p_siga))+
  geom_point(size = 1.5, color = "orange")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por MC, 2018 (%) - Nacional",
       y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por MC, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(jalisco_mc_2018_SeQuede_2022_distritos)

ggsave("outputs/jalisco_mc_2018_SeQuede_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
# mc
# Se vaya
jalisco_mc_2018_SeaRevocado_2022_distritos <- jalisco_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_mc_2018, y=p_revoque))+
  geom_point(size = 1.5, color = "orange")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el MC, 2018 (%) - Nacional",
       y="Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el MC, 2018 (%) - Nacional vs \n Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(jalisco_mc_2018_SeaRevocado_2022_distritos)

ggsave("outputs/jalisco_mc_2018_SeaRevocado_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
###############################################################################
###############################################################################
############## NUEVO LEON ####################################################
###############################################################################

nl_presidencia_2018_revocacion_distritos <- presidencia_2018_revocacion_distrito%>%
  clean_names() %>%
  filter(id_estado != 0) %>%
  filter(id_estado == 19)
# Morena
# Se quede

nl_MORENA_2018_SeQuede_2022_distritos <- nl_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_morena_2018, y=p_siga))+
  geom_point(size = 1.5, color = "violetred4")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por Morena, 2018 (%) - Nacional",
       y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por Morena, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(nl_MORENA_2018_SeQuede_2022_distritos)

ggsave("outputs/nl_MORENA_2018_SeQuede_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
# Morena
# Se vaya
nl_MORENA_2018_SeaRevocado_2022_distritos <- nl_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_morena_2018, y=p_revoque))+
  geom_point(size = 1.5, color = "violetred4")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por Morena, 2018 (%) - Nacional",
       y="Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por Morena, 2018 (%) - Nacional vs \n Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(nl_MORENA_2018_SeaRevocado_2022_distritos)

ggsave("outputs/nl_MORENA_2018_SeaRevocado_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")


# PAN
# Se quede

nl_pan_2018_SeQuede_2022_distritos <- nl_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_pan_2018, y=p_siga))+
  geom_point(size = 1.5, color = "blue")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PAN, 2018 (%) - Nacional",
       y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PAN, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(nl_pan_2018_SeQuede_2022_distritos)

ggsave("outputs/nl_PAN_2018_SeQuede_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
# pan
# Se vaya
nl_pan_2018_SeaRevocado_2022_distritos <- nl_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_pan_2018, y=p_revoque))+
  geom_point(size = 1.5, color = "blue")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PAN, 2018 (%) - Nacional",
       y="Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PAN, 2018 (%) - Nacional vs \n Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(nl_pan_2018_SeaRevocado_2022_distritos)

ggsave("outputs/nl_pan_2018_SeaRevocado_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")

# pri
# Se quede

nl_pri_2018_SeQuede_2022_distritos <- nl_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_pri_2018, y=p_siga))+
  geom_point(size = 1.5, color = "red")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PRI, 2018 (%) - Nacional",
       y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PRI, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(nl_pri_2018_SeQuede_2022_distritos)

ggsave("outputs/nl_pri_2018_SeQuede_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
# pri
# Se vaya
nl_pri_2018_SeaRevocado_2022_distritos <- nl_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_pri_2018, y=p_revoque))+
  geom_point(size = 1.5, color = "red")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PRI, 2018 (%) - Nacional",
       y="Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PRI, 2018 (%) - Nacional vs \n Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(nl_pri_2018_SeaRevocado_2022_distritos)

ggsave("outputs/nl_pri_2018_SeaRevocado_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")



# prd
# Se quede

nl_prd_2018_SeQuede_2022_distritos <- nl_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_prd_2018, y=p_siga))+
  geom_point(size = 1.5, color = "gold3")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PRD, 2018 (%) - Nacional",
       y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PRD, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(nl_prd_2018_SeQuede_2022_distritos)

ggsave("outputs/nl_prd_2018_SeQuede_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
# prd
# Se vaya
nl_prd_2018_SeaRevocado_2022_distritos <- nl_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_prd_2018, y=p_revoque))+
  geom_point(size = 1.5, color = "gold3")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PRD, 2018 (%) - Nacional",
       y="Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PRD, 2018 (%) - Nacional vs \n Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()


print(nl_prd_2018_SeaRevocado_2022_distritos)

ggsave("outputs/nl_prd_2018_SeaRevocado_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")

# pvem
# Se quede

nl_pvem_2018_SeQuede_2022_distritos <- nl_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_pvem_2018, y=p_siga))+
  geom_point(size = 1.5, color = "green")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PVEM, 2018 (%) - Nacional",
       y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PVEM, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(nl_pvem_2018_SeQuede_2022_distritos)

ggsave("outputs/nl_pvem_2018_SeQuede_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
# pvem
# Se vaya
nl_pvem_2018_SeaRevocado_2022_distritos <- nl_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_pvem_2018, y=p_revoque))+
  geom_point(size = 1.5, color = "green")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PVEM, 2018 (%) - Nacional",
       y="Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PVEM, 2018 (%) - Nacional vs \n Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(nl_pvem_2018_SeaRevocado_2022_distritos)

ggsave("outputs/nl_pvem_2018_SeaRevocado_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")

# pt
# Se quede

nl_pt_2018_SeQuede_2022_distritos <- nl_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_pt_2018, y=p_siga))+
  geom_point(size = 1.5, color = "red3")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PT, 2018 (%) - Nacional",
       y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PT, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(nl_pt_2018_SeQuede_2022_distritos)

ggsave("outputs/nl_pt_2018_SeQuede_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
# pt
# Se vaya
pt_pt_2018_SeaRevocado_2022_distritos <- nl_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_pt_2018, y=p_revoque))+
  geom_point(size = 1.5, color = "red3")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PT, 2018 (%) - Nacional",
       y="Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PT, 2018 (%) - Nacional vs \n Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(pt_pt_2018_SeaRevocado_2022_distritos)

ggsave("outputs/pt_pt_2018_SeaRevocado_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")

# mc
# Se quede

nl_mc_2018_SeQuede_2022_distritos <- nl_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_mc_2018, y=p_siga))+
  geom_point(size = 1.5, color = "orange")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por MC, 2018 (%) - Nacional",
       y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por MC, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(nl_mc_2018_SeQuede_2022_distritos)

ggsave("outputs/jalisco_mc_2018_SeQuede_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
# mc
# Se vaya
nl_mc_2018_SeaRevocado_2022_distritos <- nl_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_mc_2018, y=p_revoque))+
  geom_point(size = 1.5, color = "orange")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el MC, 2018 (%) - Nacional",
       y="Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el MC, 2018 (%) - Nacional vs \n Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(nl_mc_2018_SeaRevocado_2022_distritos)

ggsave("outputs/nl_mc_2018_SeaRevocado_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")

###############################################################################
###############################################################################
############## yucatan  ####################################################
###############################################################################

y_presidencia_2018_revocacion_distritos <- presidencia_2018_revocacion_distrito%>%
  clean_names() %>%
  filter(id_estado != 0) %>%
  filter(id_estado == 31)

# Morena
# Se quede

y_MORENA_2018_SeQuede_2022_distritos <- y_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_morena_2018, y=p_siga))+
  geom_point(size = 1.5, color = "violetred4")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por Morena, 2018 (%) - Nacional",
       y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por Morena, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(y_MORENA_2018_SeQuede_2022_distritos)

ggsave("outputs/y_MORENA_2018_SeQuede_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
# Morena
# Se vaya
y_MORENA_2018_SeaRevocado_2022_distritos <- y_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_morena_2018, y=p_revoque))+
  geom_point(size = 1.5, color = "violetred4")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por Morena, 2018 (%) - Nacional",
       y="Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por Morena, 2018 (%) - Nacional vs \n Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(y_MORENA_2018_SeaRevocado_2022_distritos)

ggsave("outputs/y_MORENA_2018_SeaRevocado_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")


# PAN
# Se quede

y_pan_2018_SeQuede_2022_distritos <- y_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_pan_2018, y=p_siga))+
  geom_point(size = 1.5, color = "blue")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PAN, 2018 (%) - Nacional",
       y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PAN, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(y_pan_2018_SeQuede_2022_distritos)

ggsave("outputs/y_PAN_2018_SeQuede_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
# pan
# Se vaya
y_pan_2018_SeaRevocado_2022_distritos <- y_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_pan_2018, y=p_revoque))+
  geom_point(size = 1.5, color = "blue")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PAN, 2018 (%) - Nacional",
       y="Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PAN, 2018 (%) - Nacional vs \n Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(y_pan_2018_SeaRevocado_2022_distritos)

ggsave("outputs/y_pan_2018_SeaRevocado_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")

# pri
# Se quede

y_pri_2018_SeQuede_2022_distritos <- y_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_pri_2018, y=p_siga))+
  geom_point(size = 1.5, color = "red")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PRI, 2018 (%) - Nacional",
       y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PRI, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(y_pri_2018_SeQuede_2022_distritos)

ggsave("outputs/y_pri_2018_SeQuede_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
# pri
# Se vaya
y_pri_2018_SeaRevocado_2022_distritos <- y_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_pri_2018, y=p_revoque))+
  geom_point(size = 1.5, color = "red")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PRI, 2018 (%) - Nacional",
       y="Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PRI, 2018 (%) - Nacional vs \n Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(y_pri_2018_SeaRevocado_2022_distritos)

ggsave("outputs/y_pri_2018_SeaRevocado_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")



# prd
# Se quede

y_prd_2018_SeQuede_2022_distritos <- y_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_prd_2018, y=p_siga))+
  geom_point(size = 1.5, color = "gold3")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PRD, 2018 (%) - Nacional",
       y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PRD, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(y_prd_2018_SeQuede_2022_distritos)

ggsave("outputs/y_prd_2018_SeQuede_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
# prd
# Se vaya
y_prd_2018_SeaRevocado_2022_distritos <- y_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_prd_2018, y=p_revoque))+
  geom_point(size = 1.5, color = "gold3")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PRD, 2018 (%) - Nacional",
       y="Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PRD, 2018 (%) - Nacional vs \n Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()


print(y_prd_2018_SeaRevocado_2022_distritos)

ggsave("outputs/y_prd_2018_SeaRevocado_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")

# pvem
# Se quede

y_pvem_2018_SeQuede_2022_distritos <- y_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_pvem_2018, y=p_siga))+
  geom_point(size = 1.5, color = "green")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PVEM, 2018 (%) - Nacional",
       y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PVEM, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(y_pvem_2018_SeQuede_2022_distritos)

ggsave("outputs/y_pvem_2018_SeQuede_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
# pvem
# Se vaya
y_pvem_2018_SeaRevocado_2022_distritos <- y_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_pvem_2018, y=p_revoque))+
  geom_point(size = 1.5, color = "green")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PVEM, 2018 (%) - Nacional",
       y="Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PVEM, 2018 (%) - Nacional vs \n Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(y_pvem_2018_SeaRevocado_2022_distritos)

ggsave("outputs/y_pvem_2018_SeaRevocado_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")

# pt
# Se quede

y_pt_2018_SeQuede_2022_distritos <- y_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_pt_2018, y=p_siga))+
  geom_point(size = 1.5, color = "red3")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PT, 2018 (%) - Nacional",
       y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PT, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(y_pt_2018_SeQuede_2022_distritos)

ggsave("outputs/y_pt_2018_SeQuede_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
# pt
# Se vaya
y_pt_2018_SeaRevocado_2022_distritos <- y_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_pt_2018, y=p_revoque))+
  geom_point(size = 1.5, color = "red3")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PT, 2018 (%) - Nacional",
       y="Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PT, 2018 (%) - Nacional vs \n Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(y_pt_2018_SeaRevocado_2022_distritos)

ggsave("outputs/y_pt_2018_SeaRevocado_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")

# mc
# Se quede

y_mc_2018_SeQuede_2022_distritos <- y_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_mc_2018, y=p_siga))+
  geom_point(size = 1.5, color = "orange")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por MC, 2018 (%) - Nacional",
       y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por MC, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(y_mc_2018_SeQuede_2022_distritos)

ggsave("outputs/y_mc_2018_SeQuede_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
# mc
# Se vaya
y_mc_2018_SeaRevocado_2022_distritos <- y_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_mc_2018, y=p_revoque))+
  geom_point(size = 1.5, color = "orange")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el MC, 2018 (%) - Nacional",
       y="Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el MC, 2018 (%) - Nacional vs \n Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(y_mc_2018_SeaRevocado_2022_distritos)

ggsave("outputs/y_mc_2018_SeaRevocado_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")

###############################################################################
###############################################################################
############## cdmx  ####################################################
###############################################################################

c_presidencia_2018_revocacion_distritos <- presidencia_2018_revocacion_distrito%>%
  clean_names() %>%
  filter(id_estado != 0) %>%
  filter(id_estado == 9)

# Morena
# Se quede

c_MORENA_2018_SeQuede_2022_distritos <- c_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_morena_2018, y=p_siga))+
  geom_point(size = 1.5, color = "violetred4")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por Morena, 2018 (%) - Nacional",
       y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por Morena, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(c_MORENA_2018_SeQuede_2022_distritos)

ggsave("outputs/c_MORENA_2018_SeQuede_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
# Morena
# Se vaya
c_MORENA_2018_SeaRevocado_2022_distritos <- c_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_morena_2018, y=p_revoque))+
  geom_point(size = 1.5, color = "violetred4")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por Morena, 2018 (%) - Nacional",
       y="Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por Morena, 2018 (%) - Nacional vs \n Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(c_MORENA_2018_SeaRevocado_2022_distritos)

ggsave("outputs/c_MORENA_2018_SeaRevocado_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")


# PAN
# Se quede

c_pan_2018_SeQuede_2022_distritos <- c_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_pan_2018, y=p_siga))+
  geom_point(size = 1.5, color = "blue")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PAN, 2018 (%) - Nacional",
       y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PAN, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(c_pan_2018_SeQuede_2022_distritos)

ggsave("outputs/c_PAN_2018_SeQuede_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
# pan
# Se vaya
c_pan_2018_SeaRevocado_2022_distritos <- c_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_pan_2018, y=p_revoque))+
  geom_point(size = 1.5, color = "blue")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PAN, 2018 (%) - Nacional",
       y="Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PAN, 2018 (%) - Nacional vs \n Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(c_pan_2018_SeaRevocado_2022_distritos)

ggsave("outputs/y_pan_2018_SeaRevocado_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")

# pri
# Se quede

c_pri_2018_SeQuede_2022_distritos <- c_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_pri_2018, y=p_siga))+
  geom_point(size = 1.5, color = "red")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PRI, 2018 (%) - Nacional",
       y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PRI, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(c_pri_2018_SeQuede_2022_distritos)

ggsave("outputs/c_pri_2018_SeQuede_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
# pri
# Se vaya
c_pri_2018_SeaRevocado_2022_distritos <- c_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_pri_2018, y=p_revoque))+
  geom_point(size = 1.5, color = "red")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PRI, 2018 (%) - Nacional",
       y="Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PRI, 2018 (%) - Nacional vs \n Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(c_pri_2018_SeaRevocado_2022_distritos)

ggsave("outputs/c_pri_2018_SeaRevocado_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")



# prd
# Se quede

c_prd_2018_SeQuede_2022_distritos <- c_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_prd_2018, y=p_siga))+
  geom_point(size = 1.5, color = "gold3")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PRD, 2018 (%) - Nacional",
       y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PRD, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(c_prd_2018_SeQuede_2022_distritos)

ggsave("outputs/c_prd_2018_SeQuede_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
# prd
# Se vaya
c_prd_2018_SeaRevocado_2022_distritos <- c_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_prd_2018, y=p_revoque))+
  geom_point(size = 1.5, color = "gold3")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PRD, 2018 (%) - Nacional",
       y="Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PRD, 2018 (%) - Nacional vs \n Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()


print(c_prd_2018_SeaRevocado_2022_distritos)

ggsave("outputs/c_prd_2018_SeaRevocado_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")

# pvem
# Se quede

c_pvem_2018_SeQuede_2022_distritos <- c_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_pvem_2018, y=p_siga))+
  geom_point(size = 1.5, color = "green")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PVEM, 2018 (%) - Nacional",
       y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PVEM, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(c_pvem_2018_SeQuede_2022_distritos)

ggsave("outputs/c_pvem_2018_SeQuede_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
# pvem
# Se vaya
c_pvem_2018_SeaRevocado_2022_distritos <- c_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_pvem_2018, y=p_revoque))+
  geom_point(size = 1.5, color = "green")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PVEM, 2018 (%) - Nacional",
       y="Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PVEM, 2018 (%) - Nacional vs \n Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(c_pvem_2018_SeaRevocado_2022_distritos)

ggsave("outputs/c_pvem_2018_SeaRevocado_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")

# pt
# Se quede

c_pt_2018_SeQuede_2022_distritos <- c_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_pt_2018, y=p_siga))+
  geom_point(size = 1.5, color = "red3")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PT, 2018 (%) - Nacional",
       y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PT, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(c_pt_2018_SeQuede_2022_distritos)

ggsave("outputs/c_pt_2018_SeQuede_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
# pt
# Se vaya
c_pt_2018_SeaRevocado_2022_distritos <- c_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_pt_2018, y=p_revoque))+
  geom_point(size = 1.5, color = "red3")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el PT, 2018 (%) - Nacional",
       y="Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el PT, 2018 (%) - Nacional vs \n Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(c_pt_2018_SeaRevocado_2022_distritos)

ggsave("outputs/c_pt_2018_SeaRevocado_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")

# mc
# Se quede

c_mc_2018_SeQuede_2022_distritos <- c_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_mc_2018, y=p_siga))+
  geom_point(size = 1.5, color = "orange")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por MC, 2018 (%) - Nacional",
       y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por MC, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(c_mc_2018_SeQuede_2022_distritos)

ggsave("outputs/y_mc_2018_SeQuede_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
# mc
# Se vaya
c_mc_2018_SeaRevocado_2022_distritos <- c_presidencia_2018_revocacion_distritos %>% ggplot(aes(x= p_mc_2018, y=p_revoque))+
  geom_point(size = 1.5, color = "orange")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Presidencia por el MC, 2018 (%) - Nacional",
       y="Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       title="Votos a Presidencia por el MC, 2018 (%) - Nacional vs \n Votos para que AMLO sea Revocado de la presidencia, 2022 Nacional(%)",
       subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
  theme_economist()

print(c_mc_2018_SeaRevocado_2022_distritos)

ggsave("outputs/y_mc_2018_SeaRevocado_2022_distritos",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")

