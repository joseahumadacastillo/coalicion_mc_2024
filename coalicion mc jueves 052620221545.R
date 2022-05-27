#Proyecto:
#  Que tanto la coalicion opositora necesita electoralmente a MC? Electoralmente que es MC?
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

library(tidyverse)
library(reshape2)
library(ggrepel)
library(ggthemes)
library(janitor)
library(patchwork)
library(ggforce)
library(ggpubr)
library(sf)


################################################################################
################################################################################
#################################### SECCIÓN I:#################################
################################ Estadística descriptiva #######################
#####################################Caja y brazos##############################
################################################################################

#_______________________________________________________________________________
#  I.1.-Directorios y bases de datos:
#_______________________________________________________________________________

# I.1.1.- Directorio
#getwd()
setwd("C:/Users/Jose Ahumada/Desktop/github/coalicion_mc_2024")

# I.1.2.- Bases



presidencial_2012 <- read.csv("Presidente2012Seccion.csv")                      #load
                                                                                #Elecciones presidenciales 2012 a nivel casilla
presidencial_2012 <- presidencial_2012 %>%                                      #Transform a distritales
  clean_names() %>%
  filter(seccion!=0) %>%
  group_by(id_estado, id_distrito) %>%
  summarize(pan_2012                 = sum(pan, na.rm = T),
            pri_2012                 = sum(pri, na.rm = T),
            prd_2012                 = sum(prd, na.rm = T),
            pvem_2012                = sum(pvem, na.rm = T),
            pt_2012                  = sum(pt, na.rm = T),
            mc_2012                  = sum(mc, na.rm = T),
            nva_a_2012               = sum(nva_alianza, na.rm = T),
            pri_pvem_2012            = sum(pri_pvem, na.rm = T),
            prd_pt_mc_2012           = sum(prd_pt_mc, na.rm = T),
            prd_pt_2012                  = sum(prd_pt, na.rm = T),
            prd_mc_2012              = sum(prd_mc, na.rm = T),
            pt_mc_2012               = sum(pt_mc, na.rm = T),
            num_votos_can_nreg_2012  = sum(num_votos_can_nreg, na.rm = T),
            num_votos_nulos_2012     = sum(num_votos_nulos, na.rm = T),
            total_votos_2012         = sum(total_votos, na.rm = T),
            lista_nominal_2012       = sum(lista_nominal, na.rm = T))

#presidencial_2012 %>% mutate(llave = str_c(id_estado, "_", id_distrito))
#presidencial_2012<- presidencial_2012%>% mutate(llave = str_c(id_estado, "_", id_distrito))


saveRDS(presidencial_2012, ("presidencial_2012_distritos_federales.rds"))       #save
#_______________________________________________________________________________

# Eleccion presidencial 2018 a nivel casilla

presidencial_2018 <- read.csv("2018_presidencia.csv")                           #load

cols <- names(presidencial_2018)[13:39]                                         #Problema de origen de la base, algunas columnas vienen con datos no numerico
presidencial_2018[cols] <- lapply(presidencial_2018[cols], as.numeric)


presidencial_2018 <- presidencial_2018 %>%
  clean_names() %>%
  filter(seccion != 0) %>%
  rename(mc = movimiento_ciudadano, nalz = nueva_alianza, morena = morena, pes = encuentro_social, pri_pvem_na_2018 = pri_pvem_na)


presidencial_2018 <- presidencial_2018 %>%                                      # Transformacion a nivel seccional
  group_by(id_estado, id_distrito) %>%
    summarize(pan_2018                    = sum(pan, na.rm = T),
              pri_2018                    = sum(pri, na.rm = T),
              prd_2018                    = sum(prd, na.rm = T),
              pvem_2018                   = sum(pvem, na.rm = T),
              pt_2018                     = sum(pt, na.rm = T),
              mc_2018                     = sum(mc, na.rm = T),
              nva_a_2018                  = sum(nalz, na.rm = T),
              morena_2018                 = sum(morena, na.rm = T),
              pes_2018                    = sum(pes, na.rm = T),
              pan_prd_mc_2018             = sum(pan_prd_mc, na.rm = T),
              pan_prd_2018                = sum(pan_prd, na.rm = T),
              pan_mc_2018                 =  sum(pan_mc, na.rm = T),
              prd_mc_2018                 = sum(prd_mc, na.rm = T),
              pri_pvem_na_2018            = sum(pri_pvem_na_2018, na.rm = T),
              pri_pvem_2018               = sum(pri_pvem, na.rm = T),
              pri_na_2018                 = sum(pri_na, na.rm = T),
              pvem_na_2018                = sum(pvem_na, na.rm = T),
              pt_morena_pes_2018          = sum(pt_morena_pes, na.rm = T),
              pt_morena_2018              = sum(pt_morena, na.rm = T),
              pt_pes_2018                 = sum(pt_pes, na.rm = T),
              morena_pes_2018             = sum(morena_pes, na.rm = T),
              ci1_2018                    = sum(cand_ind_01, na.rm = T),
              ci2_2018                    = sum(cand_ind_02, na.rm = T),
              cnr_2018                    = sum(cnr, na.rm = T),
              vn_2018                     = sum(vn, na.rm = T),
              total_votos_calculados_2018 = sum(total_votos_calculados, na.rm = T),
              lista_nominal_2018  = sum(lista_nominal_casilla, na.rm = T))

#presidencial_2018 %>% mutate(llave = str_c(id_estado, "_", id_distrito))
#presidencial_2018 <- presidencial_2012%>% mutate(llave2 = str_c(id_estado, "_", id_distrito))

saveRDS(presidencial_2018, ("presidencial_2018_distritos_federales.rds"))       #save
#________________________________________________________________________________
#Uniónde bases 1 y 2


volatilidad_presidencial_2012_2018 <- left_join(presidencial_2012, presidencial_2018, by = c("id_estado", "id_distrito"))

volatilidad_presidencial_2012_2018 <- volatilidad_presidencial_2012_2018 %>%    # Transformaciones
mutate(p_pan_2012 = ((pan_2012 / lista_nominal_2012)*100),
       p_pri_2012 = ((pri_2012 / lista_nominal_2012)*100),
       p_prd_2012 = ((prd_2012 / lista_nominal_2012)*100),
       p_pvem_2012 = ((pvem_2012 / lista_nominal_2012)*100),
       p_pt_2012 = ((pt_2012 / lista_nominal_2012)*100),
       p_mc_2012 = ((mc_2012 / lista_nominal_2012)*100),
       p_nva_a_2012 = ((nva_a_2012 / lista_nominal_2012)*100),
       p_pri_pvem_2012 = ((pri_pvem_2012 / lista_nominal_2012)*100),
       p_prd_pt_mc_2012 = ((prd_pt_mc_2012 / lista_nominal_2012)*100),
       p_prd_pt_2012 = ((prd_pt_2012 / lista_nominal_2012)*100),
       p_prd_mc_2012 = ((prd_mc_2012 / lista_nominal_2012)*100),
       p_pt_mc_2012 = ((pt_mc_2012 / lista_nominal_2012)*100),
       p_votos_can_nreg_2012 =((num_votos_can_nreg_2012 / lista_nominal_2012)*100),
       p_num_votos_nulos_2012 =((num_votos_nulos_2012 / lista_nominal_2012)*100),
       participacion_2012 = ((total_votos_2012 / lista_nominal_2012)*100),
       abstencion_2012 = (100 - participacion_2012),
       p_pan_2018 = ((pan_2018 / lista_nominal_2018)*100),
       p_pri_2018 = ((pri_2018 / lista_nominal_2018)*100),
       p_prd_2018 = ((prd_2018 / lista_nominal_2018)*100),
       p_pvem_2018 = ((pvem_2018 / lista_nominal_2018)*100),
       p_pt_2018 = ((pt_2018 / lista_nominal_2018)*100),
       p_mc_2018 = ((mc_2018 / lista_nominal_2018)*100),
       p_nva_a_2018 = ((nva_a_2018 / lista_nominal_2018)*100),
       p_morena_2018 = ((morena_2018 / lista_nominal_2018)*100),
       p_pes_2018 = ((pes_2018 / lista_nominal_2018)*100),
       p_pan_prd_mc_2018 = ((pan_prd_mc_2018 / lista_nominal_2018)*100),
       p_pan_prd_2018 = ((pan_prd_2018 / lista_nominal_2018)*100),
       p_pan_mc_2018 = ((pan_mc_2018 / lista_nominal_2018)*100),
       p_prd_mc_2018 = ((prd_mc_2018 / lista_nominal_2018)*100),
       p_pri_pvem_na_2018 = ((pri_pvem_na_2018/ lista_nominal_2018)*100),
       p_pri_pvem_2018 = ((pri_pvem_2018/ lista_nominal_2018)*100),
       p_pri_na_2018 = ((pri_na_2018/ lista_nominal_2018)*100),
       p_pvem_na_2018 = ((pvem_na_2018/ lista_nominal_2018)*100),
       p_pt_morena_pes_2018 = ((pt_morena_pes_2018/ lista_nominal_2018)*100),
       p_pt_morena_2018 = ((pt_morena_2018/ lista_nominal_2018)*100),
       p_pt_pes_2018 = ((pt_pes_2018/ lista_nominal_2018)*100),
       p_morena_pes_2018 = ((morena_pes_2018/ lista_nominal_2018)*100),
       p_ci1_2018 = ((ci1_2018/ lista_nominal_2018)*100),
       p_ci2_2018 = ((ci2_2018/ lista_nominal_2018)*100),
       p_cnr_2018 = ((cnr_2018/ lista_nominal_2018)*100),
       por_vn_2018 = ((vn_2018/ lista_nominal_2018)*100),
       p_participacion_2018 = ((total_votos_calculados_2018/ lista_nominal_2018)*100),
       p_abstencion_2018 = (100 - p_participacion_2018),
       variacion_pri_presidencial_2018_2012 = (p_pri_2018 - p_pri_2012),
       variacion_pan_presidencial_2018_2012 = (p_pan_2018 - p_pan_2012),
       variacion_prd_presidencial_2018_2012 = (p_prd_2018 - p_prd_2012),
       variacion_pvem_presidencial_2018_2012 = (p_pvem_2018 - p_pvem_2012),
       variacion_pt_presidencial_2018_2012 = (p_pt_2018 - p_pt_2012),
       variacion_mc_presidencial_2018_2012 = (p_mc_2018 - p_mc_2012),
       variacion_nva_a_presidencial_2018_2012 = (p_nva_a_2018 - p_nva_a_2012),
       pedersen_pri_presidencial_2018_2012 = abs(p_pri_2018 - p_pri_2012),
       pedersen_pan_presidencial_2018_2012 = abs(p_pan_2018 - p_pan_2012),
       pedersen_prd_presidencial_2018_2012 = abs(p_prd_2018 - p_prd_2012),
       pedersen_pvem_presidencial_2018_2012 = abs(p_pvem_2018 - p_pvem_2012),
       pedersen_pt_presidencial_2018_2012 = abs(p_pt_2018 - p_pt_2012),
       pedersen_mc_presidencial_2018_2012 = abs(p_mc_2018 - p_mc_2012),
       pedersen_nva_a_presidencial_2018_2012 = abs(p_nva_a_2018 - p_nva_a_2012))

volatilidad_presidencial_2012_2018 <- volatilidad_presidencial_2012_2018 %>%
  mutate(pedersen_voto_nulo_presidencial_2018_2012 =(por_vn_2018 - p_num_votos_nulos_2012 ))

saveRDS(volatilidad_presidencial_2012_2018, ("volatilidad_presidencial_2012_2018.rds")) #save
#_______________________________________________________________________________
# Variacion entre 2018 menos 2018 por cada partido
variaciones_2018_2012 <- volatilidad_presidencial_2012_2018
variaciones_2018_2012 <- variaciones_2018_2012%>%
  select(id_estado,
         variacion_pri_presidencial_2018_2012,
         variacion_pan_presidencial_2018_2012,
         variacion_prd_presidencial_2018_2012,
         variacion_pvem_presidencial_2018_2012,
         variacion_pt_presidencial_2018_2012,
         variacion_mc_presidencial_2018_2012,
         variacion_nva_a_presidencial_2018_2012) %>%
  rename(PRI = variacion_pri_presidencial_2018_2012,
         PAN = variacion_pan_presidencial_2018_2012,
         PRD = variacion_prd_presidencial_2018_2012,
         VERDE = variacion_pvem_presidencial_2018_2012,
         PT = variacion_pt_presidencial_2018_2012,
         MC = variacion_mc_presidencial_2018_2012,
         NUEVA_ALIANZA = variacion_nva_a_presidencial_2018_2012)

  saveRDS(variaciones_2018_2012, ("variaciones_2018_2012.rds"))        #save

  PRI_variaciones_2018_2012 <- variaciones_2018_2012 %>%              # DF por partido para diagramas
    select(id_estado,
           PRI)

  PAN_variaciones_2018_2012 <- variaciones_2018_2012 %>%
    select(id_estado,
           PAN)

  PRD_variaciones_2018_2012 <- variaciones_2018_2012 %>%
    select(id_estado,
           PRD)

  VERDE_variaciones_2018_2012 <- variaciones_2018_2012 %>%
    select(id_estado,
           VERDE)

  PT_variaciones_2018_2012 <- variaciones_2018_2012 %>%
    select(id_estado,
             PT)

  MC_variaciones_2018_2012 <- variaciones_2018_2012 %>%
    select(id_estado,
           MC)
  NUEVA_ALIANZA_variaciones_2018_2012 <- variaciones_2018_2012 %>%
    select(id_estado,
           NUEVA_ALIANZA)

#_______________________________________________________________________________
# Coeficiente de pedersen para cada partido político.
  #pedersen_partido = (abs(20118-2012))

  pedersen_2018_2012 <- volatilidad_presidencial_2012_2018
  pedersen_2018_2012 <- pedersen_2018_2012%>%
    select(id_estado,
           pedersen_pri_presidencial_2018_2012,
           pedersen_pan_presidencial_2018_2012,
           pedersen_prd_presidencial_2018_2012,
           pedersen_pvem_presidencial_2018_2012,
           pedersen_pt_presidencial_2018_2012,
           pedersen_mc_presidencial_2018_2012,
           pedersen_nva_a_presidencial_2018_2012) %>%
    rename(PRI = pedersen_pri_presidencial_2018_2012,
           PAN = pedersen_pan_presidencial_2018_2012,
           PRD = pedersen_prd_presidencial_2018_2012,
           VERDE = pedersen_pvem_presidencial_2018_2012,
           PT = pedersen_pt_presidencial_2018_2012,
           MC = pedersen_mc_presidencial_2018_2012,
           NUEVA_ALIANZA = pedersen_nva_a_presidencial_2018_2012)

  saveRDS(pedersen_2018_2012, ("pedersen_2018_2012.rds"))                        #save
#_______________________________________________________________________________
# Transformacion de bases a modo long

  long_variaciones_2018_2012 <- melt(variaciones_2018_2012, id.vars="id_estado")

  long_pedersen_2018_2012 <- melt(pedersen_2018_2012, id.vars="id_estado")

  PRI_long_variaciones_2018_2012 <-melt(PRI_variaciones_2018_2012, id.vars="id_estado")
  PAN_long_variaciones_2018_2012 <-melt(PAN_variaciones_2018_2012, id.vars="id_estado")
  PRD_long_variaciones_2018_2012 <-melt(PRD_variaciones_2018_2012, id.vars="id_estado")
  VERDE_long_variaciones_2018_2012 <-melt(VERDE_variaciones_2018_2012, id.vars="id_estado")
  PT_long_variaciones_2018_2012 <-melt(PT_variaciones_2018_2012, id.vars="id_estado")
  MC_long_variaciones_2018_2012 <-melt(MC_variaciones_2018_2012, id.vars="id_estado")
  NUEVA_ALIANZA_long_variaciones_2018_2012 <-melt(NUEVA_ALIANZA_variaciones_2018_2012, id.vars="id_estado")

#________________________________________________________________________________
# Algunos datos de estadistica descriptiva

          mean(variaciones_2018_2012$PRI, na.rm = 1)
          mean(variaciones_2018_2012$PAN, na.rm = 1)
          mean(variaciones_2018_2012$PRD, na.rm = 1)
          mean(variaciones_2018_2012$VERDE, na.rm = 1)
          mean(variaciones_2018_2012$PT, na.rm = 1)
          mean(variaciones_2018_2012$MC, na.rm = 1)
          mean(variaciones_2018_2012$NUEVA_ALIANZA, na.rm = 1)

          sd(variaciones_2018_2012$PRI, na.rm = 1)
          sd(variaciones_2018_2012$PAN, na.rm = 1)
          sd(variaciones_2018_2012$PRD, na.rm = 1)
          sd(variaciones_2018_2012$VERDE, na.rm = 1)
          sd(variaciones_2018_2012$PT, na.rm = 1)
          sd(variaciones_2018_2012$MC, na.rm = 1)
          sd(variaciones_2018_2012$NUEVA_ALIANZA, na.rm = 1)

#_______________________________________________________________________________
# Gráficas
#_______________________________________________________________________________

# Diagrama de caja y brazos

ggboxplot(long_variaciones_2018_2012,                                     # boxplot de todos los partidos variacion 2018-2012
                 x = "variable",
                 y = "value",
                 title = "Variacion en voto a la presidencia por partido 2018-2012",
                 ylab = "Diferencial",
                 xlab = "Partidos",
                 color = "variable",
                 palette = c("red", "blue", "yellow", "green", "red3", "orange", "deepskyblue4"),
                 add = c("jitter", "mean"),
                 rotate = TRUE,
                caption = "Elaborado por: Jose Ahumada Castillo @AhumadaReal \n Fuente: INE ( computos2018.ine.mx/ , https://portalanterior.ine.mx/)",
                ggtheme = theme_pubr())

ggsave("Variacion en voto a la presidencia por partido 2018-2012",
             device = "png",
             height = 20,
             width = 25,
             units = "cm")

ggboxplot(PRI_long_variaciones_2018_2012,                                     # boxplot de todos los partidos variacion 2018-2012
                x = "variable",
                y = "value",
                title = "Variacion en voto a la presidencia por PRI 2018-2012",
                ylab = "Diferencial",
                xlab = "PRI",
                color = "variable",
                palette = c("red"),
                add = c("jitter", "mean"),
                rotate = TRUE,
                caption = "Elaborado por: Jose Ahumada Castillo @AhumadaReal \n Fuente: INE ( computos2018.ine.mx/ , https://portalanterior.ine.mx/)",
                ggtheme = theme_pubr())

ggsave("Variacion en voto a la presidencia por PRI 2018-2012",
             device = "png",
             height = 20,
             width = 25,
             units = "cm")

ggboxplot(PAN_long_variaciones_2018_2012,                                     # boxplot de todos los partidos variacion 2018-2012
                x = "variable",
                y = "value",
                title = "Variacion en voto a la presidencia por PAN 2018-2012",
                ylab = "Diferencial",
                xlab = "PAN",
                color = "variable",
                palette = c("blue"),
                add = c("jitter", "mean"),
                rotate = TRUE,
                caption = "Elaborado por: Jose Ahumada Castillo @AhumadaReal \n Fuente: INE ( computos2018.ine.mx/ , https://portalanterior.ine.mx/)",
                ggtheme = theme_pubr())

ggsave("Variacion en voto a la presidencia por PAN 2018-2012",
             device = "png",
             height = 20,
             width = 25,
             units = "cm")

ggboxplot(PRD_long_variaciones_2018_2012,                                     # boxplot de todos los partidos variacion 2018-2012
                x = "variable",
                y = "value",
                title = "Variacion en voto a la presidencia por PRD 2018-2012",
                ylab = "Diferencial",
                xlab = "PRD",
                color = "variable",
                palette = c("yellow"),
                add = c("jitter", "mean"),
                rotate = TRUE,
                caption = "Elaborado por: Jose Ahumada Castillo @AhumadaReal \n Fuente: INE ( computos2018.ine.mx/ , https://portalanterior.ine.mx/)",
                ggtheme = theme_pubr())

ggsave("Variacion en voto a la presidencia por PRd 2018-2012",
             device = "png",
             height = 20,
             width = 25,
             units = "cm")



ggboxplot(VERDE_long_variaciones_2018_2012,                                     # boxplot de todos los partidos variacion 2018-2012
                x = "variable",
                y = "value",
                title = "Variacion en voto a la presidencia por VERDE 2018-2012",
                ylab = "Diferencial",
                xlab = "VERDE",
                color = "variable",
                palette = c("green"),
                add = c("jitter", "mean"),
                rotate = TRUE,
                caption = "Elaborado por: Jose Ahumada Castillo @AhumadaReal \n Fuente: INE ( computos2018.ine.mx/ , https://portalanterior.ine.mx/)",
                ggtheme = theme_pubr())

ggsave("Variacion en voto a la presidencia por VERDE 2018-2012",
             device = "png",
             height = 20,
             width = 25,
             units = "cm")



ggboxplot(PT_long_variaciones_2018_2012,                                     # boxplot de todos los partidos variacion 2018-2012
                x = "variable",
                y = "value",
                title = "Variacion en voto a la presidencia por PT 2018-2012",
                ylab = "Diferencial",
                xlab = "PT",
                color = "variable",
                palette = c("red3"),
                add = c("jitter", "mean"),
                rotate = TRUE,
                caption = "Elaborado por: Jose Ahumada Castillo @AhumadaReal \n Fuente: INE ( computos2018.ine.mx/ , https://portalanterior.ine.mx/)",
                ggtheme = theme_pubr())

ggsave("Variacion en voto a la presidencia por PT 2018-2012",
             device = "png",
             height = 20,
             width = 25,
             units = "cm")


ggboxplot(MC_long_variaciones_2018_2012,                                     # boxplot de todos los partidos variacion 2018-2012
                x = "variable",
                y = "value",
                title = "Variacion en voto a la presidencia por MC 2018-2012",
                ylab = "Diferencial",
                xlab = "MC",
                color = "variable",
                palette = c("ORANGE"),
                add = c("jitter", "mean"),
                rotate = TRUE,
                caption = "Elaborado por: Jose Ahumada Castillo @AhumadaReal \n Fuente: INE ( computos2018.ine.mx/ , https://portalanterior.ine.mx/)",
                ggtheme = theme_pubr())

ggsave("Variacion en voto a la presidencia por MC 2018-2012",
             device = "png",
             height = 20,
             width = 25,
             units = "cm")

ggboxplot(NUEVA_ALIANZA_long_variaciones_2018_2012,                                     # boxplot de todos los partidos variacion 2018-2012
                x = "variable",
                y = "value",
                title = "Variacion en voto a la presidencia por NUEVA ALIANZA 2018-2012",
                ylab = "Diferencial",
                xlab = "NUEVA ALIANZA",
                color = "variable",
                palette = c("deepskyblue4"),
                add = c("jitter", "mean"),
                rotate = TRUE,
                caption = "Elaborado por: Jose Ahumada Castillo @AhumadaReal \n Fuente: INE ( computos2018.ine.mx/ , https://portalanterior.ine.mx/)",
                ggtheme = theme_pubr())

ggsave("Variacion en voto a la presidencia por NUEVA ALIANZA 2018-2012",
             device = "png",
             height = 20,
             width = 25,
             units = "cm")




ggplot(long_variaciones_2018_2012,                               # theme_light density plot
             aes(x = value,
                 fill = variable)) +
geom_density(alpha = 0.5) +
        scale_fill_manual( values = c("red", "blue", "yellow", "green", "red3", "orange", "deepskyblue4"))
        theme_light()

ggplot(long_pedersen_2018_2012, aes(x = value, fill = variable)) +                       # Draw overlaying histogram
          geom_histogram(position = "identity", alpha = 0.2, bins = 50)
ggboxplot(long_pedersen_2018_2012,
                  x = "variable",
                  y = "value",
                  title = "Indice de Volatilidad de Pedersen para partidosPedersen en voto a la presidencia por partido 2018-2012",
                  ylab = "Indice de Volatilidad de Pedersen para partidos",
                  xlab = "Partidos",
                  color = "variable",
                  palette = c("red", "blue", "yellow", "green", "red3", "orange", "deepskyblue4"),
                  add = c("jitter", "mean"),
                  rotate = TRUE,
                  caption = "Elaborado por: Jose Ahumada Castillo @AhumadaReal \n Fuente: INE ( computos2018.ine.mx/ , https://portalanterior.ine.mx/)",
                  ggtheme = theme_pubr())


variaciones_2018_2012_chicos <- variaciones_2018_2012 %>%
          select(id_estado,VERDE, MC, NUEVA_ALIANZA)

long_variaciones_2018_2012_chicos <- melt(variaciones_2018_2012_chicos, id.vars="id_estado")


ggboxplot(long_variaciones_2018_2012_chicos,
                  x = "variable",
                  y = "value",
                  title = "Variacion en voto a la presidencia por partido chico 2018-2012",
                  ylab = "Diferencial",
                  xlab = "Partidos Chicos",
                  color = "variable",
                  palette = c("green", "orange", "deepskyblue4"),
                  add = c("jitter", "mean"),
                  rotate = TRUE,
                  caption = "Elaborado por: Jose Ahumada Castillo @AhumadaReal \n Fuente: INE ( computos2018.ine.mx/ , https://portalanterior.ine.mx/)",
                  ggtheme = theme_pubr())

ggplot(long_variaciones_2018_2012_chicos,                               # theme_light density plot
               aes(x = value,
                   fill = variable)) +
geom_density(alpha = 0.5) +
          scale_fill_manual( values = c("green", "orange", "deepskyblue4"))
        theme_light()

ggplot(long_variaciones_2018_2012_chicos, aes(x = value, fill = variable)) +                       # Draw overlaying histogram
          geom_histogram(position = "identity", alpha = 0.2, bins = 50)



        boxplot.stats(variaciones_2018_2012$PRI)
        boxplot.stats(variaciones_2018_2012$PAN)
        boxplot.stats(variaciones_2018_2012$PRD)
        boxplot.stats(variaciones_2018_2012$VERDE)
        boxplot.stats(variaciones_2018_2012$PT)
        boxplot.stats(variaciones_2018_2012$MC)
        boxplot.stats(variaciones_2018_2012$NUEVA_ALIANZA)


        boxplot.stats(pedersen_2018_2012$PRI)
        boxplot.stats(pedersen_2018_2012$PAN)
        boxplot.stats(pedersen_2018_2012$PRD)
        boxplot.stats(pedersen_2018_2012$VERDE)
        boxplot.stats(pedersen_2018_2012$PT)
        boxplot.stats(pedersen_2018_2012$MC)
        boxplot.stats(pedersen_2018_2012$NUEVA_ALIANZA)
#________________________________________________________________________________
#________________________________________________________________________________
################################################################################
################################################################################
############################### SECCIÓN II #####################################
#############################  SCATTERPLOTS ####################################
################################################################################
################################################################################
################################################################################
#-------------------------------------------------------------------------------
# Directorios y Bases de Datos
#-------------------------------------------------------------------------------
#getwd()
#setwd("C:/Users/Jose Ahumada/Desktop/github")

presidencial_2018 <- readRDS("presidencial_2018_distritos_federales.rds")


#diputaciones_bueno_2021 <- read.csv("diputaciones2021.csv") %>%
#clean_names() %>%
#filter(seccion!=0) %>%
#rename(votos_nulos_2021 = votos_nulos,
                 #total_votos_calculados_2021 = total_votos_calculados,
                 #lista_nominal_2021 = lista_nominal_casilla)

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
          rename(id_distrito = id_distrito_federal)

revocacion <- revocacion %>%
          select(id_estado,
                 entidad,
                 id_distrito,
                 distrito_federal,
                 seccion,
                 ubicacion_casilla,
                 revoque,
                 siga,
                 nulos_2022,
                 total_votos_calculados_2022,
                 lista_nominal_2022)



revocacion_distrital <- revocacion %>%
group_by(id_estado, id_distrito)%>%
  summarize(revoque=sum(revoque, na.rm=T),
            siga=sum(siga, na.rm=T),
            nulos_2022=sum(nulos_2022, na.rm=T),
            total_votos_calculados_2022=sum(total_votos_calculados_2022, na.rm=T),
            lista_nominal_2022=sum(lista_nominal_2022, na.rm=T))

presidencial_2018_revocacion_distrital <- left_join(presidencial_2018, revocacion_distrital)%>%
  group_by(id_estado, id_distrito)%>%
  summarize(pan_2018=sum(pan_2018, na.rm=T),
            pri_2018=sum(pri_2018, na.rm=T),
            prd_2018=sum(prd_2018, na.rm=T),
            pvem_2018=sum(pvem_2018, na.rm=T),
            pt_2018=sum(pt_2018, na.rm=T),
            mc_2018=sum(mc_2018, na.rm=T),
            nva_a_2018=sum(nva_a_2018, na.rm=T),
            morena_2018=sum(morena_2018, na.rm=T),
            lista_nominal_2018 = sum(lista_nominal_2018, na.rm=T),
            revoque=sum(revoque, na.rm=T),
            siga=sum(siga, na.rm=T),
            nulos_2022=sum(nulos_2022, na.rm=T),
            total_votos_calculados_2022=sum(total_votos_calculados_2022, na.rm=T),
            lista_nominal_2022=sum(lista_nominal_2022, na.rm=T))



presidencial_2018_revocacion_distrital <- presidencial_2018_revocacion_distrital  %>%
  mutate( p_pan_2018 = ((pan_2018 / lista_nominal_2018)*100),
          p_pri_2018 = ((pri_2018 / lista_nominal_2018)*100),
          p_prd_2018 = ((prd_2018 / lista_nominal_2018)*100),
          p_pvem_2018 = ((pvem_2018 / lista_nominal_2018)*100),
          p_pt_2018 = ((pt_2018 / lista_nominal_2018)*100),
          p_mc_2018 = ((mc_2018 / lista_nominal_2018)*100),
          p_nva_a_2018 = ((nva_a_2018 / lista_nominal_2018)*100),
          p_morena_2018 = ((morena_2018 / lista_nominal_2018)*100),
          p_revoque = ((revoque / lista_nominal_2022)*100),
          p_siga=((siga / lista_nominal_2022)*100),
          p_nulos_2022=((nulos_2022 / lista_nominal_2022)*100))



saveRDS(presidencial_2018_revocacion_distrital, ("presidencial_2018_revocacion_distrital"))


# Morena 2018 se queda 2022
        Morena_SeQuede_2018_2022 <- presidencial_2018_revocacion_distrital %>% ggplot(aes(x= p_morena_2018, y=p_siga))+
          geom_point(size = .5, color = "violetred4")+
          geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
          labs(x=" Votos para Presidencia por Morena 2018 (%)",
               y="Votos para que AMLO se quede 2022 (%)",
               title="Votos para Presidencia por Morena 2018 (%) vs \n Votos para que AMLO se quede 2022 (%)",
               subtitle="Por Distrito", caption = "Jose Ahumada Castillo") +
          theme_economist()

        print(Morena_SeQuede_2018_2022)

        ggsave("Morena_SeQuede_2018_2022",
               device = "png",
               height = 20,
               width = 25,
               units = "cm")

# Morena 2018 se revoca 2022
       Morena_SeRevoca_2018_2022 <- presidencial_2018_revocacion_distrital %>% ggplot(aes(x= p_morena_2018, y=p_revoque))+
          geom_point(size = .5, color = "violetred4")+
         geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +          labs(x=" Votos para Presidencia por Morena 2018 (%)",
               y="Votos para que AMLO sea Revocado 2022 (%)",
               title="Votos para Presidencia por Morena 2018 (%) vs \n Votos para que AMLO sea Revocado 2022 (%)",
               subtitle="Por Distrito", caption = "Jose Ahumada Castillo") +
          theme_economist()

        print(Morena_SeRevoca_2018_2022)

        ggsave("Morena_SeRevoca_2018_2022",
               device = "png",
               height = 20,
               width = 25,
               units = "cm")



        # PAN 2018 se revoca 2022
        PAN_SeRevoca_2018_2022 <- presidencial_2018_revocacion_distrital %>% ggplot(aes(x= p_pan_2018, y=p_revoque))+
          geom_point(size = .5, color = "blue")+
          geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
          labs(x=" Votos para Presidencia por el PAN 2018 (%)",
               y="Votos para que AMLO sea Revocado 2022 (%)",
               title="Votos para Presidencia por el PAN 2018 (%) vs \n Votos para que AMLO sea Revocado 2022 (%)",
               subtitle="Por Distrito", caption = "Jose Ahumada Castillo") +
          theme_economist()

        print(PAN_SeRevoca_2018_2022)

        ggsave("PAN_SeRevoca_2018_2022",
               device = "png",
               height = 20,
               width = 25,
               units = "cm")

        # PAN 2018 se queda 2022
PAN_SeQuede_2018_2022 <- presidencial_2018_revocacion_distrital %>% ggplot(aes(x= p_pan_2018, y=p_siga))+
          geom_point(size = .5, color = "blue")+
  geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x=" Votos para Presidencia por el PAN 2018 (%)",
               y="Votos para que AMLO se quede 2022 (%)",
               title="Votos para Presidencia por el PAN 2018 (%) vs \n Votos para que AMLO se quede 2022 (%)",
               subtitle="Por Distrito", caption = "Jose Ahumada Castillo") +
          theme_economist()

        print(PAN_SeQuede_2018_2022)

        ggsave("PAN_SeQuede_2018_2022",
               device = "png",
               height = 20,
               width = 25,
               units = "cm")





# MC 2018 se queda 2022
        MC_SeQuede_2018_2022 <- presidencial_2018_revocacion_distrital %>% ggplot(aes(x= p_mc_2018, y=p_siga))+
          geom_point(size = .5, color = "orange")+
          labs(x=" Votos para Presidencia por MC 2018 (%)",
               y="Votos para que AMLO se quede 2022 (%)",
               title="Votos para Presidencia por MC 2018 (%) vs \n Votos para que AMLO se quede 2022 (%)",
               subtitle="Por Distrito", caption = "Jose Ahumada Castillo") +
          theme_economist()

        print(MC_SeQuede_2018_2022)

        ggsave("MC_SeQuede_2018_2022",
               device = "png",
               height = 20,
               width = 25,
               units = "cm")

        # MC 2018 se revoca 2022
        MC_SeRevoca_2018_2022 <- presidencial_2018_revocacion_distrital %>% ggplot(aes(x= p_mc_2018, y=p_revoque))+
          geom_point(size = .5, color = "orange")+
          labs(x=" Votos para Presidencia por MC 2018 (%)",
               y="Votos para que AMLO sea Revocado 2022 (%)",
               title="Votos para Presidencia por MC 2018 (%) vs \n Votos para que AMLO sea Revocado 2022 (%)",
               subtitle="Por Distrito", caption = "Jose Ahumada Castillo") +
          theme_economist()

        print(MC_SeRevoca_2018_2022)

        ggsave("Morena_SeRevoca_2018_2022",
               device = "png",
               height = 20,
               width = 25,
               units = "cm")


#################################################################################
#################################################################################
############################ SECCION III #######################################
#############################   MAPAS   ########################################
################################################################################


#-------------------------------------------------------------------------------
# (1)Packages:
#-------------------------------------------------------------------------------
library(ggplot2)
library(janitor)
library(sf)
library(tidyverse)
library(ggthemes)

        getwd()

        setwd("C:/Users/Jose Ahumada/Desktop/github/coalicion_mc_2024")

        # mapas_secciones_2021 <- st_read("SECCION.shp")
        #mapas_secciones_2021 <- readRDS("shape.rds")

        #mapas_secciones_2021 <- mapas_secciones_2021 %>%
        # clean_names()
        #mapas_secciones_2021$entidad <- as.numeric(mapas_secciones_2021$entidad)


        presidencial_2018_distritos <- readRDS("presidencial_2018_distritos_federales.rds")
        mapa_300distritos_federales <- st_read("DISTRITO_FEDERAL.shp")
        #plot(mapa_300distritos_federales)


        presidencial_2018_distritos <- presidencial_2018_distritos %>%
          clean_names()

        presidencial_2018_distritos <- presidencial_2018_distritos %>%
          group_by(id_estado, id_distrito) %>%
          summarize(pan_2018                    = sum(pan_2018, na.rm = T),
                    pri_2018                    = sum(pri_2018, na.rm = T),
                    prd_2018                    = sum(prd_2018, na.rm = T),
                    pvem_2018                   = sum(pvem_2018, na.rm = T),
                    pt_2018                     = sum(pt_2018, na.rm = T),
                    mc_2018                     = sum(mc_2018, na.rm = T),
                    nva_a_2018                  = sum(nva_a_2018, na.rm = T),
                    morena_2018                 = sum(morena_2018, na.rm = T),
                    pes_2018                    = sum(pes_2018, na.rm = T),
                    pan_prd_mc_2018             = sum(pan_prd_mc_2018, na.rm = T),
                    pan_prd_2018                = sum(pan_prd_2018, na.rm = T),
                    pan_mc_2018                 =  sum(pan_mc_2018, na.rm = T),
                    prd_mc_2018                 = sum(prd_mc_2018, na.rm = T),
                    pri_pvem_na_2018            = sum(pri_pvem_na_2018, na.rm = T),
                    pri_pvem_2018               = sum(pri_pvem_2018, na.rm = T),
                    pri_na_2018                 = sum(pri_na_2018, na.rm = T),
                    pvem_na_2018                = sum(pvem_na_2018, na.rm = T),
                    pt_morena_pes_2018          = sum(pt_morena_pes_2018, na.rm = T),
                    pt_morena_2018              = sum(pt_morena_2018, na.rm = T),
                    pt_pes_2018                 = sum(pt_pes_2018, na.rm = T),
                    morena_pes_2018             = sum(morena_pes_2018, na.rm = T),
                    cnr_2018                    = sum(cnr_2018, na.rm = T),
                    vn_2018                     = sum(vn_2018, na.rm = T),
                    total_votos_calculados_2018 = sum(total_votos_calculados_2018, na.rm = T),
                    lista_nominal_2018  = sum(lista_nominal_2018, na.rm = T))

        presidencial_2018_distritos <- presidencial_2018_distritos %>%
          mutate(p_pan_2018 = ((pan_2018 / lista_nominal_2018)*100),
                 p_pri_2018 = ((pri_2018 / lista_nominal_2018)*100),
                 p_prd_2018 = ((prd_2018 / lista_nominal_2018)*100),
                 p_pvem_2018 = ((pvem_2018 / lista_nominal_2018)*100),
                 p_pt_2018 = ((pt_2018 / lista_nominal_2018)*100),
                 p_mc_2018 = ((mc_2018 / lista_nominal_2018)*100),
                 p_nva_a_2018 = ((nva_a_2018 / lista_nominal_2018)*100),
                 p_morena_2018 = ((morena_2018 / lista_nominal_2018)*100),
                 por_vn_2018 = ((vn_2018/ lista_nominal_2018)*100),
                 p_participacion_2018 = ((total_votos_calculados_2018/ lista_nominal_2018)*100),
                 p_abstencion_2018 = (100 - p_participacion_2018))

        #saveRDS(presidencial_2018_distritos, ("presidencial_2018_distritos.rds")) #save



        # View(diputaciones_2021_300distritos)
        # View(mapa_300distritos_federales)

        mapa_300distritos_federales %>% mutate(llave = str_c(entidad, "_", distrito_f))
        mapa_300distritos_federales <- mapa_300distritos_federales%>% mutate(llave = str_c(entidad, "_", distrito_f))


        presidencial_2018_distritos%>% mutate(llave = str_c(id_estado, "_", id_distrito))
        presidencial_2018_distritos <- presidencial_2018_distritos%>% mutate(llave = str_c(id_estado, "_", id_distrito))



        left_join(mapa_300distritos_federales, presidencial_2018_distritos, by = "llave", na.rm = TRUE )
        mapa_presidencial_2018_distritos <- left_join(mapa_300distritos_federales, presidencial_2018_distritos, by = "llave" )
        #mapa_diputaciones_2021_distritos <- na.omit(mapa_diputaciones_2021_distritos)


        mapa_presidencial_2018_distritos %>% ggplot(aes(fill = p_morena_2018)) +
          geom_sf(color = "transparent") +
          scale_fill_gradientn(colors = c("white", "violetred4")) +
          labs(title = "Voto por MORENA 2018 (%)", x = "Latitud", y= "MORENA", caption = "Elaborado por: Jose Ahumada Castillo @AhumadaReal \n Fuente: INE ( computos2018.ine.mx/)")


        ggsave("mapa_distritos_p_MORENA_20_diputados_MR",
               device = "png",
               height = 20,
               width = 25,
               units = "cm")



        mapa_presidencial_2018_distritos %>% ggplot(aes(fill = p_pan_2018)) +
          geom_sf(color = "transparent") +
          scale_fill_gradientn(colors = c("white", "blue")) +
          labs(title = "Voto por el PAN 2018 (%)", x = "Latitud", y= "PAN", caption = "Elaborado por: Jose Ahumada Castillo @AhumadaReal \n Fuente: INE ( computos2018.ine.mx/")

        ggsave("mapa_secciones_p_PAN_2021_diputados_MR",
               device = "png",
               height = 20,
               width = 25,
               units = "cm")

        mapa_presidencial_2018_distritos %>% ggplot(aes(fill = p_pri_2018)) +
          geom_sf(color = "transparent") +
          scale_fill_gradientn(colors = c("white", "red")) +
          labs(title = "Voto por el PRI 2018 (%)", x = "Latitud", y= "PRI", caption = "Elaborado por: Jose Ahumada Castillo @AhumadaReal \n Fuente: INE ( computos2018.ine.mx/")

        ggsave("mapa_secciones_p_PRI_2021_diputados_MR",
               device = "png",
               height = 20,
               width = 25,
               units = "cm")

        mapa_presidencial_2018_distritos %>% ggplot(aes(fill = p_prd_2018)) +
          geom_sf(color = "transparent") +
          scale_fill_gradientn(colors = c("white", "YELLOW")) +
          labs(title = "Voto por el PRD 2018 (%)", x = "Latitud", y= "PRD", caption = "Elaborado por: Jose Ahumada Castillo @AhumadaReal \n Fuente: INE ( computos2018.ine.mx/")

        ggsave("mapa_secciones_p_PRD_2021_diputados_MR",
               device = "png",
               height = 20,
               width = 25,
               units = "cm")

        mapa_presidencial_2018_distritos %>% ggplot(aes(fill = p_pvem_2018)) +
          geom_sf(color = "transparent") +
          scale_fill_gradientn(colors = c("white", "green")) +
          labs(title = "Voto por el PVEM 2018 (%)", x = "Latitud", y= "PVEM", caption = "Elaborado por: Jose Ahumada Castillo @AhumadaReal \n Fuente: INE ( computos2018.ine.mx/")

        ggsave("mapa_secciones_p_PVEM_2021_diputados_MR",
               device = "png",
               height = 20,
               width = 25,
               units = "cm")

        mapa_presidencial_2018_distritos %>% ggplot(aes(fill = p_pt_2018)) +
          geom_sf(color = "transparent") +
          scale_fill_gradientn(colors = c("white", "red3")) +
          labs(title = "Voto por el PT 2018 (%)", x = "Latitud", y= "PT", caption = "Elaborado por: Jose Ahumada Castillo @AhumadaReal \n Fuente: INE ( computos2018.ine.mx/")

        ggsave("mapa_secciones_p_PT_2021_diputados_MR",
               device = "png",
               height = 20,
               width = 25,
               units = "cm")

        mapa_presidencial_2018_distritos %>% ggplot(aes(fill = p_nva_a_2018)) +
          geom_sf(color = "transparent") +
          scale_fill_gradientn(colors = c("white", "deepskyblue4")) +
          labs(title = "Voto por el NUEVA ALIANZA 2018 (%)", x = "Latitud", y= "NUEVA ALIANZA", caption = "Elaborado por: Jose Ahumada Castillo @AhumadaReal \n Fuente: INE ( computos2018.ine.mx/")

        ggsave("mapa_secciones_p_NUEVA_ALIANZA_2021_diputados_MR",
               device = "png",
               height = 20,
               width = 25,
               units = "cm")




        mapa_presidencial_2018_distritos %>% ggplot(aes(fill = p_mc_2018)) +
          geom_sf(color = "transparent") +
          scale_fill_gradientn(colors = c("white", "orange")) +
          labs(title = "Voto por MC 2018 (%)", x = "Latitud", y= "MC", caption = "Elaborado por: Jose Ahumada Castillo @AhumadaReal \n Fuente: INE ( computos2018.ine.mx/")

        ggsave("mapa_secciones_p_MC_2021_diputados_MR",
               device = "png",
               height = 20,
               width = 25,
               units = "cm")

################################################################################
################################################################################
################################ fin ###########################################
############################## The End #########################################
################################################################################
################################################################################









