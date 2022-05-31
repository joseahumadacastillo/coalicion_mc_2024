#Proyecto: MC=PAN,PRI,SD?
# Electoralmente que es MC? Que tanto la coalicion anti amlo lo necesita?
#_______________________________________________________________________________
#
#_______________________________________________________________________________

rm(list = ls()) #Clears variable environment
cat("\014")     #Clears console
#_______________________________________________________________________________



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
library(janitor)
# install.packages("car")
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
# data y directorios
#-------------------------------------------------------------------------------
#getwd()
# setwd("C:/Users/Jose Ahumada/Desktop/mapa distritos federales/mapas_electorales_secciones/SECCION")

#unidades <- read.csv("CATALOGO_UNIDADES_TERRITORIALES_RM2022.csv")

#unidades <- unidades %>%
#  clean_names() %>%
 # rename(id_estado = id_entidad)


#diputaciones <- readRDS("diputaciones-2021.rds")
#revocacion <- readRDS("revocacion.rds")

unidades <- read.csv("CATALOGO_UNIDADES_TERRITORIALES_RM2022.csv") %>%
  clean_names()%>%
  rename(id_estado = id_entidad)


diputaciones_bueno_2021 <- read.csv("diputaciones2021.csv") %>%
  clean_names() %>%
  filter(seccion!=0) %>%
  rename(votos_nulos_2021 = votos_nulos,
         total_votos_calculados_2021 = total_votos_calculados,
         lista_nominal_2021 = lista_nominal_casilla)

 diputaciones_unidades <- left_join(diputaciones_bueno_2021, unidades)%>%
  group_by(id_estado, unidad_territorial)%>%
  summarize(pan        = sum(pan, na.rm=T),
            pri        = sum(pri, na.rm=T),
            prd        = sum(prd, na.rm=T),
            pvem       = sum(pvem, na.rm=T),
            pt = sum(pt, na.rm=T),
            mc = sum(mc, na.rm=T),
            morena = sum(morena, na.rm=T),
            pes = sum(pes, na.rm=T),
            rsp = sum(rsp, na.rm=T),
            fxm = sum(fxm, na.rm=T),
            ci = sum(ci, na.rm=T),
            pan_pri_prd = sum(pan_pri_prd, na.rm=T),
            pan_pri = sum(pan_pri, na.rm=T),
            pan_prd = sum(pan_prd, na.rm = T),
            pri_prd = sum(pri_prd, na.rm = T),
            pvem_pt_morena = sum(pvem_pt_morena, na.rm=T),
            pvem_pt =  sum(pvem_pt, na.rm=T),
            pvem_morena = sum(pvem_morena, na.rm=T),
            pt_morena = sum(pt_morena, na.rm=T),
            candidato_a_no_registrado_a = sum(candidato_a_no_registrado_a, na.rm=T),
            votos_nulos_2021  = sum(votos_nulos_2021, na.rm=T),
            total_votos_calculados_2021 = sum(total_votos_calculados_2021, na.rm=T),
            lista_nominal_2021 = sum(lista_nominal_2021, na.rm=T))%>%
  ungroup()

 revocacion <- read.csv("20220411_1845_COMPUTOS_RM2022.csv")
 revocacion <- revocacion %>%
   clean_names() %>%
   rename(id_estado = id_entidad)

 revocacion <- revocacion %>%
   filter(id_estado != 0)


 revocacion <- revocacion %>%
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

 revocacion_unidades <- left_join(revocacion, unidades)%>%
   group_by(id_estado, unidad_territorial)%>%
   summarize(revoque=sum(revoque, na.rm=T),
             siga=sum(siga, na.rm=T),
             nulos_2022=sum(nulos_2022, na.rm=T),
             total_votos_calculados_2022=sum(total_votos_calculados_2022, na.rm=T),
             lista_nominal_2022=sum(lista_nominal_2022, na.rm=T))

 diputaciones_revocacion_unidades <- left_join(diputaciones_unidades, revocacion_unidades)

 diputaciones_revocacion_unidades <- diputaciones_revocacion_unidades %>%
   mutate(p_pan = ((pan / total_votos_calculados_2021) * 100),
          p_pri = ((pri / total_votos_calculados_2021)* 100),
          p_prd = ((prd / total_votos_calculados_2021)* 100),
          p_pvem = ((pvem / total_votos_calculados_2021)* 100),
          p_pt = ((pt / total_votos_calculados_2021)* 100),
          p_mc = ((mc / total_votos_calculados_2021)* 100),
          p_morena = ((morena / total_votos_calculados_2021)* 100),
          p_pes = ((pes / total_votos_calculados_2021)* 100),
          p_rsp = ((rsp / total_votos_calculados_2021)* 100),
          p_fxm = ((fxm / total_votos_calculados_2021)* 100),
          p_ci = ((ci / total_votos_calculados_2021)* 100),
          p_pan_pri_prd = ((pan_pri_prd / total_votos_calculados_2021)* 100),
          p_pan_pri = ((pan_pri / total_votos_calculados_2021)* 100),
          p_pan_prd = ((pan_prd / total_votos_calculados_2021)* 100),
          p_pvem_pt_morena = ((pvem_pt_morena / total_votos_calculados_2021)* 100),
          p_pvem_pt = ((pvem_pt / total_votos_calculados_2021)* 100),
          p_pvem_morena = ((pvem_morena / total_votos_calculados_2021)* 100),
          p_candidato_a_no_registrado_a= ((candidato_a_no_registrado_a / total_votos_calculados_2021)* 100),
          p_votos_nulos_2021 = ((votos_nulos_2021 / total_votos_calculados_2021)* 100),
          p_total_votos_calculados_21= ((total_votos_calculados_2021 / total_votos_calculados_2021)* 100),
          p_participacion_2021 = ((total_votos_calculados_2021/ lista_nominal_2021)* 100),
          p_abstencion_2021 = ((100 - p_participacion_2021)),
          p_revoque = ((revoque / total_votos_calculados_2022 )* 100),
          p_siga = ((siga / total_votos_calculados_2022 )* 100),
          p_votos_nulos_2022 = ((nulos_2022 / total_votos_calculados_2022 )* 100),
          p_pan_menos_p_pri = (p_pan - p_pri),
          p_pan_menos_p_morena = (p_pan - p_morena),
          p_pan_menos_p_mc = (p_pan - p_mc),
          p_morena_menos_mc = (p_morena - p_mc),
          p_mc_menos_p_pvem = (p_mc - p_pvem))

 saveRDS(diputaciones_revocacion_unidades, ("outputs/diputaciones_2021_revocacion_2022_unidades.rds"))



  Pan_mc_siga <- revocacion_2022_diputaciones_2021 %>% ggplot(aes(x= p_pan_menos_p_mc, y=p_siga))+
    geom_point(size = .5, color = "blue")+
    geom_smooth(method = lm, se = FALSE)+
    labs(x="PAN - MC",
         y="% de Votos AMLO siga",
         title="PAN - MC (2021) vs \n % de Votos para que AMLO siga (2022)",
         subtitle="Por unidad territorial", caption = "JosE Ahumada Castillo") +
    theme_economist()

  print(Pan_mc_siga)


  Pan_mc_revoque <- revocacion_2022_diputaciones_2021 %>% ggplot(aes(x= p_pan_menos_p_mc, y=p_revoque))+
    geom_point(size = .5, color = "blue")+
    geom_smooth(method = lm, se = FALSE)+
    labs(x="PAN - MC",
         y="% de Votos AMLO Revoque",
         title="PAN - MC (2021) vs \n % de Votos para que AMLO revoque (2022)",
         subtitle="Por unidad territorial", caption = "JosE Ahumada Castillo") +
    theme_economist()

  print(Pan_mc_revoque)

  p_mc_revoque <- revocacion_2022_diputaciones_2021_unidades %>% ggplot(aes(x= p_mc, y=p_revoque))+
    geom_point(size = .5, color = "blue")+
    geom_smooth(method = lm, se = FALSE)+
    labs(x="MC",
         y="% de Votos AMLO Revoque",
         title="MC (2021) vs \n % de Votos para que AMLO revoque (2022)",
         subtitle="Por unidad territorial", caption = "JosE Ahumada Castillo") +
    theme_economist()

  print(p_mc_revoque)

  Morena_SeQuede <- revocacion_2022_diputaciones_2021 %>% ggplot(aes(x= p_morena, y=p_siga))+
    geom_point(size = .5, color = "violetred4")+
    geom_smooth(method = lm, se = FALSE) +
    labs(x=" % de Votos por Diputaciones Federales para Morena",
         y="% de Votos AMLO se quede",
         title="% de Votos por Morena  2021 vs \n 5 de Votos para que AMLO se quede 2022",
         subtitle="Por unidad territorial", caption = "José Ahumada Castillo") +
    theme_economist()

  print(Morena_SeQuede)

  plot (p_mc, p_pan)
