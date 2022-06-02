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
################################################################################
################################# SECCION 0 ####################################
########################## BARPLOTS ELECTORALES 2000-2018 ######################
################################################################################
################################################################################
#_______________________________________________________________________________
#
#_______________________________________________________________________________


#_______________________________________________________________________________
# data, directorios y graficas de barras
#_______________________________________________________________________________
#getwd()

setwd("C:/Users/Jose Ahumada/Desktop/github/coalicion_mc_2024")
#_______________________________________________________________________________
#_______________________________________________________________________________

# 2000
# https://portalanterior.ine.mx/documentos/RESELEC/esta2000/inipres.htm
barplot_2000 <- read.csv("inputs/barplot_2000.csv")                                     #load
barplot_2000$Partido <- factor(barplot_2000$Partido, levels = c('PAN', 'PRI', 'PRD', "PCD", "PARM", "PDS"))

primero_2000 <- 42.52

segundo_2000 <- 36.11

tercero_2000 <- 16.64



primero_2000 + segundo_2000 + tercero_2000

primero_2000 - segundo_2000
segundo_2000 - tercero_2000

T_2000 <- (((primero_2000 + segundo_2000)*(segundo_2000 - tercero_2000))/(primero_2000)) # Taagapera index
T_2000


barplot_2000 <- ggplot(barplot_2000, aes(Partido, dosmil, fill = Partido)) +
  geom_bar(stat="identity", width= .5) +
  scale_fill_manual(values=c("blue", "red", "gold2", "purple", "orange", "magenta3")) +
  labs(x=" Partidos/Cabeza de Coalición para Presidencia 2000 (%)",
       y="Votos para Presidencial 2000 (%)",
       title="Votos para Presidencia 2000 (%)",
       subtitle="Nacional\nDiferencia entre primero y segundo = 6.41\nDiferencia entre segundo y tercero = 19.47  ", caption = "José Ahumada Castillo   tw: @AhumadaReal\n Por simplificación se pone el partido más grande de la coalición \n Los votos nulos y por candidaturas no registradas complementan la suma a 100%") +
  geom_text(aes(label = dosmil), position = position_dodge(.9), size=5, vjust = 1.5, size = 2, color = "black")

barplot_2000

ggsave("outputs/barplot_2000.png",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")


# 2006
#https://portalanterior.ine.mx/documentos/Estadisticas2006/presidente/gra_nac.html

barplot_2006 <- read.csv("inputs/barplot_2006.csv")


barplot_2006$Partido <- factor(barplot_2006$Partido, levels = c("PAN", "PRD", "PRI", "NVA", "ASD"))

primero_2006 <- 35.89

segundo_2006 <- 35.31

tercero_2006 <- 22.26

primero_2006 + segundo_2006 + tercero_2006

primero_2006 - segundo_2006
segundo_2006 - tercero_2006

                                                                                # Taagapera index

#T_2006 <- ((primero_2006 + segundo_2006)*(segundo_2006 - tercero_2006)/(primero_2006))
#T_2006

barplot_2006 <- ggplot(barplot_2006, aes(Partido, dosmilseis, fill = Partido)) +
  geom_bar(stat="identity", width= .5) +
  scale_fill_manual(values=c("blue", "gold2", "red", "deepskyblue4", "navyblue")) +
  labs(x=" Partidos/Cabeza de Coalición para Presidencia 2006 (%)",
       y="Votos para Presidencial 2006 (%)",
       title="Votos para Presidencia 2006 (%)",
       subtitle="Nacional\nDiferencia entre primero y segundo = .58\nDiferencia entre segundo y tercero = 13.05  ", caption = "José Ahumada Castillo   tw: @AhumadaReal\n Por simplificación se pone el partido más grande de la coalición \n Los votos nulos y por candidaturas no registradas complementan la suma a 100%") +
  geom_text(aes(label = dosmilseis), position = position_dodge(.9), size=5, vjust = 1.5, size = 2, color = "black")

barplot_2006

ggsave("outputs/barplot_2006.png",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")


# 2012
# https://prep2012.ife.org.mx/prep/NACIONAL/PresidenteNacionalVPC.html

barplot_2012 <- read.csv("inputs/barplot_2012.csv")    #load
barplot_2012$Partido <- factor(barplot_2012$Partido, levels = c("PRI", "PRD", "PAN", "NVA"))


primero_2012 <- 38.2

segundo_2012 <- 32.6

tercero_2012 <- 25.39

primero_2012 + segundo_2012 + tercero_2012

primero_2012 - segundo_2012
segundo_2012 - tercero_2012

                                                                                # Taagapera index

#T_20012 <- ((primero_2012 + segundo_2012)*(segundo_2012 - tercero_2012)/(primero_2012))
#T_2012

barplot_2012 <- ggplot(barplot_2012, aes(Partido, dosmildoce, fill = Partido)) +
  geom_bar(stat="identity", width= .5) +
  scale_fill_manual(values=c("red", "gold2", "blue", "deepskyblue4")) +
  labs(x=" Partidos/Cabeza de Coalición para Presidencia 2012 (%)",
       y="Votos para Presidencial 2012 (%)",
       title="Votos para Presidencia 2012 (%)",
       subtitle="Nacional\nDiferencia entre primero y segundo = 5.6\nDiferencia entre segundo y tercero = 7.21  ", caption = "José Ahumada Castillo   tw: @AhumadaReal\n Por simplificación se pone el partido más grande de la coalición \n Los votos nulos y por candidaturas no registradas complementan la suma a 100%") +
  geom_text(aes(label = dosmildoce), position = position_dodge(.9), size=5, vjust = 1.5, size = 2, color = "black")

barplot_2012

ggsave("outputs/barplot_2012.png",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")


# 2018
# https://computos2018.ine.mx/#/presidencia/nacional/1/1/1/1
barplot_2018 <- read.csv("inputs/barplot_2018.csv")    #load
barplot_2018$Partido <- factor(barplot_2018$Partido, levels = c("MORENA", "PAN", "PRI", "CI"))


primero_2018 <- 53.19

segundo_2018 <- 22.27

tercero_2018 <- 16.40

primero_2018 + segundo_2018 + tercero_2018

primero_2018 - segundo_2018
segundo_2018 - tercero_2018

                                                                                  # Taagapera index

#T_20012 <- ((primero_2012 + segundo_2012)*(segundo_2012 - tercero_2012)/(primero_2012))
#T_2012

barplot_2018 <- ggplot(barplot_2018, aes(Partido, dosmildieciocho, fill = Partido)) +
  geom_bar(stat="identity", width= .5) +
  scale_fill_manual(values=c("violetred4", "blue", "red", "purple")) +
  labs(x=" Partidos/Cabeza de Coalición para Presidencia 2018 (%)",
       y="Votos para Presidencial 2018 (%)",
       title="Votos para Presidencia 2018 (%)",
       subtitle="Nacional\nDiferencia entre primero y segundo = 30.92\nDiferencia entre segundo y tercero = 5.87  ", caption = "José Ahumada Castillo   tw: @AhumadaReal\n Por simplificación se pone el partido más grande de la coalición \n Los votos nulos y por candidaturas no registradas complementan la suma a 100%") +
  geom_text(aes(label = dosmildieciocho), position = position_dodge(.9), size=5, vjust = 1.5, size = 2, color = "black")

barplot_2018

ggsave("outputs/barplot_2018.png",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")

################################################################################
################################################################################
#################################### SECCIÓN I:#################################
################################ diagramas de #######################
################################## Caja y brazos##############################
################################################################################

#_______________________________________________________________________________
#  I.1.-Directorios y bases de datos:
#_______________________________________________________________________________

# I.1.1.- Directorio
#getwd()
#setwd("C:/Users/Jose Ahumada/Desktop/github/coalicion_mc_2024")

# I.1.2.- Bases



presidencial_2012 <- read.csv("inputs/Presidente2012Seccion.csv")                      #load
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


saveRDS(presidencial_2012, ("outputs/presidencial_2012_distritos_federales.rds"))       #save
#_______________________________________________________________________________

# Eleccion presidencial 2018 a nivel casilla

presidencial_2018 <- read.csv("inputs/2018_presidencia.csv")                           #load

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

saveRDS(presidencial_2018, ("outputs/presidencial_2018_distritos_federales.rds"))       #save
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

saveRDS(volatilidad_presidencial_2012_2018, ("outputs/volatilidad_presidencial_2012_2018.rds")) #save
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
         variacion_mc_presidencial_2018_2012) %>%
  rename(PRI = variacion_pri_presidencial_2018_2012,
         PAN = variacion_pan_presidencial_2018_2012,
         PRD = variacion_prd_presidencial_2018_2012,
         VERDE = variacion_pvem_presidencial_2018_2012,
         PT = variacion_pt_presidencial_2018_2012,
         MC = variacion_mc_presidencial_2018_2012)

  saveRDS(variaciones_2018_2012, ("outputs/variaciones_2018_2012.rds"))        #save

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

  saveRDS(pedersen_2018_2012, ("outputs/pedersen_2018_2012.rds"))                        #save
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
                 palette = c("red", "blue", "gold2", "green", "red3", "orange", "deepskyblue4"),
                 add = c("jitter", "mean"),
                 rotate = TRUE,
                caption = "Elaborado por: Jose Ahumada Castillo @AhumadaReal \n Fuente: INE ( computos2018.ine.mx/ , https://portalanterior.ine.mx/)",
                ggtheme = theme_pubr())

ggsave("outputs/Variacion en voto a la presidencia por partido 2018-2012.png",
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
                ggtheme = theme_pubr())+
  geom_boxplot(outlier.colour = "blue", outlier.shape = 4, outlier.size = 2)

ggsave("outputs/Variacion en voto a la presidencia por PRI 2018-2012.png",
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
                ggtheme = theme_pubr())  +
  geom_boxplot(outlier.colour = "red", outlier.shape = 4, outlier.size = 2)

ggsave("outputs/Variacion en voto a la presidencia por PAN 2018-2012.png",
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
                palette = c("gold2"),
                add = c("jitter", "mean"),
                rotate = TRUE,
                caption = "Elaborado por: Jose Ahumada Castillo @AhumadaReal \n Fuente: INE ( computos2018.ine.mx/ , https://portalanterior.ine.mx/)",
                ggtheme = theme_pubr()) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 4, outlier.size = 2)

ggsave("outputs/Variacion en voto a la presidencia por PRd 2018-2012.png",
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
                ggtheme = theme_pubr()) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 4, outlier.size = 2)

ggsave("outputs/Variacion en voto a la presidencia por VERDE 2018-2012.png",
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
                ggtheme = theme_pubr()) +
  geom_boxplot(outlier.colour = "blue", outlier.shape = 4, outlier.size = 2)

ggsave("outputs/Variacion en voto a la presidencia por PT 2018-2012.png",
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
                ggtheme = theme_pubr()) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 4, outlier.size = 2)


ggsave("outputs/Variacion en voto a la presidencia por MC 2018-2012.png",
             device = "png",
             height = 20,
             width = 25,
             units = "cm")




        boxplot.stats(variaciones_2018_2012$PRI)
        outliers_pri = (0 / 300) * 100
        outliers_pri


        boxplot.stats(variaciones_2018_2012$PAN)
        outliers_pan = (1 / 300) * 100
        outliers_pan


        boxplot.stats(variaciones_2018_2012$PRD)
        outliers_prd = (4 / 300) * 100
        outliers_prd

        boxplot.stats(variaciones_2018_2012$VERDE)
        outliers_pvem = (38 / 300) * 100
        outliers_pvem

      boxplot.stats(variaciones_2018_2012$PT)
        outliers_pt = (8 / 300) * 100
        outliers_pt

        boxplot.stats(variaciones_2018_2012$MC)
        outliers_mc = (15 / 300) * 100
        outliers_mc



        sd(variaciones_2018_2012$PRI, na.rm = 1)
        sd(variaciones_2018_2012$PAN, na.rm = 1)
        sd(variaciones_2018_2012$PRD, na.rm = 1)
        sd(variaciones_2018_2012$VERDE, na.rm = 1)
        sd(variaciones_2018_2012$PT, na.rm = 1)
        sd(variaciones_2018_2012$MC, na.rm = 1)


        boxplot.stats(pedersen_2018_2012$PRI)
        boxplot.stats(pedersen_2018_2012$PAN)
        boxplot.stats(pedersen_2018_2012$PRD)
        boxplot.stats(pedersen_2018_2012$VERDE)
        boxplot.stats(pedersen_2018_2012$PT)
        boxplot.stats(pedersen_2018_2012$MC)
#________________________________________________________________________________
#________________________________________________________________________________
################################################################################
################################################################################
############################### SECCIÓN II #####################################
#############################  SCATTERPLOTS ####################################
################################################################################
################################################################################
################################################################################
        ################################################################################
        ################################# scatterplots #################################
        ################################## distritales, 2018 -2022######
        ################################################################################

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

        diputaciones_2021_revocacion_distrito <- diputaciones_2021_revocacion_distrito %>%
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

        ggsave("outputs/MORENA_2018_SeQuede_2022_distritos.png",
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

        ggsave("outputs/MORENA_2018_SeaRevocado_2022_distritos.png",
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

        ggsave("outputs/PAN_2018_SeQuede_2022_distritos.png",
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

        ggsave("outputs/pan_2018_SeaRevocado_2022_distritos.png",
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

        ggsave("outputs/pri_2018_SeQuede_2022_distritos.png",
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

        ggsave("outputs/pri_2018_SeaRevocado_2022_distritos.png",
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

        ggsave("outputs/prd_2018_SeQuede_2022_distritos.png",
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

        ggsave("outputs/prd_2018_SeaRevocado_2022_distritos.png",
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

        ggsave("outputs/pvem_2018_SeQuede_2022_distritos.png",
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

        ggsave("outputs/pvem_2018_SeaRevocado_2022_distritos.png",
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

        ggsave("outputs/pt_2018_SeQuede_2022_distritos.png",
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

        ggsave("outputs/pt_2018_SeaRevocado_2022_distritos.png",
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

        ggsave("outputs/mc_2018_SeQuede_2022_distritos.png",
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

        ggsave("outputs/mc_2018_SeaRevocado_2022_distritos.png",
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

        ggsave("outputs/jalisco_MORENA_2018_SeQuede_2022_distritos.png",
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

        ggsave("outputs/jalisco_MORENA_2018_SeaRevocado_2022_distritos.png",
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

        ggsave("outputs/jalisco_PAN_2018_SeQuede_2022_distritos.png",
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

        ggsave("outputs/jalisco_pan_2018_SeaRevocado_2022_distritos.png",
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

        ggsave("outputs/jalisco_pri_2018_SeQuede_2022_distritos.png",
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

        ggsave("outputs/jalisco_pri_2018_SeaRevocado_2022_distritos.png",
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

        ggsave("outputs/jalisco_prd_2018_SeQuede_2022_distritos.png",
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

        ggsave("outputs/jalisco_prd_2018_SeaRevocado_2022_distritos.png",
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

        ggsave("outputs/jalisco_pvem_2018_SeQuede_2022_distritos.png",
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

        ggsave("outputs/jalisco_pvem_2018_SeaRevocado_2022_distritos.png",
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

        ggsave("outputs/jalisco_pt_2018_SeQuede_2022_distritos.png",
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

        ggsave("outputs/jalisco_pt_2018_SeaRevocado_2022_distritos.png",
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

        ggsave("outputs/jalisco_mc_2018_SeQuede_2022_distritos.png",
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

        ggsave("outputs/jalisco_mc_2018_SeaRevocado_2022_distritos.png",
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

        ggsave("outputs/nl_MORENA_2018_SeQuede_2022_distritos.png",
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

        ggsave("outputs/nl_MORENA_2018_SeaRevocado_2022_distritos.png",
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

        ggsave("outputs/nl_PAN_2018_SeQuede_2022_distritos.png",
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

        ggsave("outputs/nl_pan_2018_SeaRevocado_2022_distritos.png",
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

        ggsave("outputs/nl_pri_2018_SeQuede_2022_distritos.png",
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

        ggsave("outputs/nl_pri_2018_SeaRevocado_2022_distritos.png",
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

        ggsave("outputs/nl_prd_2018_SeQuede_2022_distritos.png",
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

        ggsave("outputs/nl_prd_2018_SeaRevocado_2022_distritos.png",
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

        ggsave("outputs/nl_pvem_2018_SeQuede_2022_distritos.png",
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

        ggsave("outputs/nl_pvem_2018_SeaRevocado_2022_distritos.png",
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

        ggsave("outputs/pt_pt_2018_SeaRevocado_2022_distritos.png",
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

        ggsave("outputs/jalisco_mc_2018_SeQuede_2022_distritos.png",
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

        ggsave("outputs/nl_mc_2018_SeaRevocado_2022_distritos.png",
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

        ggsave("outputs/y_MORENA_2018_SeQuede_2022_distritos.png",
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

        ggsave("outputs/y_MORENA_2018_SeaRevocado_2022_distritos.png",
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

        ggsave("outputs/y_PAN_2018_SeQuede_2022_distritos.png",
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

        ggsave("outputs/y_pan_2018_SeaRevocado_2022_distritos.png",
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

        ggsave("outputs/y_pri_2018_SeQuede_2022_distritos.png",
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

        ggsave("outputs/y_pri_2018_SeaRevocado_2022_distritos.png",
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

        ggsave("outputs/y_prd_2018_SeQuede_2022_distritos.png",
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

        ggsave("outputs/y_prd_2018_SeaRevocado_2022_distritos.png",
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

        ggsave("outputs/y_pvem_2018_SeQuede_2022_distritos.png",
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

        ggsave("outputs/y_pvem_2018_SeaRevocado_2022_distritos.png",
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

        ggsave("outputs/y_pt_2018_SeQuede_2022_distritos.png",
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

        ggsave("outputs/y_pt_2018_SeaRevocado_2022_distritos.png",
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

        ggsave("outputs/y_mc_2018_SeQuede_2022_distritos.png",
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

        ggsave("outputs/y_mc_2018_SeaRevocado_2022_distritos.png",
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

        ggsave("outputs/c_MORENA_2018_SeQuede_2022_distritos.png",
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

        ggsave("outputs/c_MORENA_2018_SeaRevocado_2022_distritos.png",
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

        ggsave("outputs/c_PAN_2018_SeQuede_2022_distritos.png",
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

        ggsave("outputs/y_pan_2018_SeaRevocado_2022_distritos.png",
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

        ggsave("outputs/c_pri_2018_SeQuede_2022_distritos.png",
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

        ggsave("outputs/c_pri_2018_SeaRevocado_2022_distritos.png",
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

        ggsave("outputs/c_prd_2018_SeQuede_2022_distritos.png",
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

        ggsave("outputs/c_prd_2018_SeaRevocado_2022_distritos.png",
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

        ggsave("outputs/c_pvem_2018_SeQuede_2022_distritos.png",
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

        ggsave("outputs/c_pvem_2018_SeaRevocado_2022_distritos.png",
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

        ggsave("outputs/c_pt_2018_SeQuede_2022_distritos.png",
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

        ggsave("outputs/c_pt_2018_SeaRevocado_2022_distritos.png",
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

        ggsave("outputs/y_mc_2018_SeQuede_2022_distritos.png",
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

        ggsave("outputs/y_mc_2018_SeaRevocado_2022_distritos.png",
               device = "png",
               height = 20,
               width = 25,
               units = "cm")



        ###############################################################################



        # diputaciones 2021 revocacion nuevo leon

        ###############################################################################
        ###############################################################################
        ###############################################################################
        ############## NUEVO LEON ####################################################
        ###############################################################################



        nl_diputaciones_2021_revocacion_distrito <- diputaciones_2021_revocacion_distrito%>%
          clean_names() %>%
          filter(id_estado != 0) %>%
          filter(id_estado == 19)
        # Morena
        # Se quede

        nl_MORENA_2021_SeQuede_2022_distritos <- nl_diputaciones_2021_revocacion_distrito %>% ggplot(aes(x= p_morena_2021, y=p_siga))+
          geom_point(size = 1.5, color = "violetred4")+
          geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
          labs(x="Votos a Presidencia por Morena, 2018 (%) - Nacional",
               y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
               title="Votos a Presidencia por Morena, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
               subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
          theme_economist()

        print(nl_MORENA_2021_SeQuede_2022_distritos)

        ggsave("outputs/nl_MORENA_2021_SeQuede_2022_distritos.png",
               device = "png",
               height = 20,
               width = 25,
               units = "cm")
        # Morena
        # Se vaya

        nl_MORENA_2021_SeRevoque_2022_distritos <- nl_diputaciones_2021_revocacion_distrito %>% ggplot(aes(x= p_morena_2021, y=p_revoque))+
          geom_point(size = 1.5, color = "violetred4")+
          geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
          labs(x="Votos a Presidencia por Morena, 2018 (%) - Nacional",
               y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
               title="Votos a Presidencia por Morena, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
               subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
          theme_economist()

        print(nl_MORENA_2021_SeRevoque_2022_distritos)

        ggsave("outputs/nl_MORENA_2021_SeRevoque_2022_distritos.png",
               device = "png",
               height = 20,
               width = 25,
               units = "cm")


        # pan
        # Se quede

        nl_pan_2021_SeRevoque_2022_distritos <- nl_diputaciones_2021_revocacion_distrito %>% ggplot(aes(x= p_pan_2021, y=p_revoque))+
          geom_point(size = 1.5, color = "blue")+
          geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
          labs(x="Votos a Presidencia por Morena, 2018 (%) - Nacional",
               y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
               title="Votos a Presidencia por Morena, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
               subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
          theme_economist()

        print(nl_pan_2021_SeRevoque_2022_distritos)

        ggsave("outputs/nl_pan_2021_SeRevoque_2022_distritos.png",
               device = "png",
               height = 20,
               width = 25,
               units = "cm")


        # pan
        # Se quede

        nl_pan_2021_SeQuede_2022_distritos <- nl_diputaciones_2021_revocacion_distrito %>% ggplot(aes(x= p_pan_2021, y=p_siga))+
          geom_point(size = 1.5, color = "blue")+
          geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
          labs(x="Votos a Presidencia por Morena, 2018 (%) - Nacional",
               y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
               title="Votos a Presidencia por Morena, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
               subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
          theme_economist()

        print(nl_pan_2021_SeQuede_2022_distritos)

        ggsave("outputs/nl_blue_2021_SeQuede_2022_distritos.png",
               device = "png",
               height = 20,
               width = 25,
               units = "cm")


        # MC
        # Se quede

        nl_MC_2021_SeQuede_2022_distritos <- nl_diputaciones_2021_revocacion_distrito %>% ggplot(aes(x= p_mc_2021, y=p_siga))+
          geom_point(size = 1.5, color = "ORANGE")+
          geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
          labs(x="Votos a Presidencia por Morena, 2018 (%) - Nacional",
               y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
               title="Votos a Presidencia por Morena, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
               subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
          theme_economist()

        print(nl_MC_2021_SeQuede_2022_distritos)

        ggsave("outputs/nl_MC_2021_SeRevoque_2022_distritos.png",
               device = "png",
               height = 20,
               width = 25,
               units = "cm")
        # MC
        # Se quede

        nl_MC_2021_SeRevoque_2022_distritos <- nl_diputaciones_2021_revocacion_distrito %>% ggplot(aes(x= p_mc_2021, y=p_revoque))+
          geom_point(size = 1.5, color = "ORANGE")+
          geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
          labs(x="Votos a Presidencia por Morena, 2018 (%) - Nacional",
               y="Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
               title="Votos a Presidencia por Morena, 2018 (%) - Nacional vs \n Votos para que AMLO Siga en la presidencia, 2022 Nacional(%)",
               subtitle="Por Distrito", caption = "José Ahumada Castillo @AhumadaReal \n  Fuente: (https://computos2018.ine.mx, https://computosrm2022.ine.mx/)") +
          theme_economist()

        print(nl_MC_2021_SeRevoque_2022_distritos)

        ggsave("outputs/nl_MC_2021_SeRevoque_2022_distritos.png",
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
# (2) presidencial 2012 distritos
#-------------------------------------------------------------------------------



setwd("C:/Users/Jose Ahumada/Desktop/github/coalicion_mc_2024")


presidencial_2012_distritos <- readRDS("outputs/presidencial_2012_distritos_federales.rds")
mapa_300distritos_federales <- st_read("inputs/DISTRITO_FEDERAL.shp")




presidencial_2012_distritos <- presidencial_2012_distritos %>%
 mutate(p_pan_2012 = ((pan_2012 / lista_nominal_2012)*100),
p_pri_2012 = ((pri_2012 / lista_nominal_2012)*100),
p_prd_2012 = ((prd_2012 / lista_nominal_2012)*100),
p_pvem_2012 = ((pvem_2012 / lista_nominal_2012)*100),
p_pt_2012 = ((pt_2012 / lista_nominal_2012)*100),
p_mc_2012 = ((mc_2012 / lista_nominal_2012)*100),
p_nva_a_2012 = ((nva_a_2012 / lista_nominal_2012)*100),
p_votos_nulos_2012 = ((num_votos_nulos_2012/ lista_nominal_2012)*100))

 #saveRDS(presidencial_2018_distritos, ("presidencial_2018_distritos.rds")) #save


mapa_300distritos_federales %>% mutate(llave = str_c(entidad, "_", distrito_f))
mapa_300distritos_federales <- mapa_300distritos_federales%>% mutate(llave = str_c(entidad, "_", distrito_f))


presidencial_2012_distritos%>% mutate(llave = str_c(id_estado, "_", id_distrito))
presidencial_2012_distritos <- presidencial_2012_distritos%>% mutate(llave = str_c(id_estado, "_", id_distrito))



left_join(mapa_300distritos_federales, presidencial_2012_distritos, by = "llave", na.rm = TRUE )
mapa_presidencial_2012_distritos <- left_join(mapa_300distritos_federales, presidencial_2012_distritos, by = "llave" )

saveRDS(mapa_presidencial_2012_distritos, ("outputs/presidencial_2018_distritos.rds")) #save

# MORENA NO EXISTIA

        # PAN

mapa_presidencial_2012_distritos %>% ggplot(aes(fill = p_pan_2012)) +
geom_sf(color = "transparent") +
scale_fill_gradientn(colors = c("white", "blue3"), na.value = "white") + # 7 secciones na de origen convertidas a blanco) ) +
labs(title = "Voto por el PAN 2012 (%) - Nacional", x = "Latitud", y= "PAN", caption = "Elaborado por: José Ahumada Castillo @AhumadaReal \n Fuente: INE ( https://prep2012.ife.org.mx/prep/)") +
guides(fill=guide_legend(title="Voto por el PAN (%)"))


ggsave("outputs/2012_presidencial_mapa_distritos_pan.png",
               device = "png",
               height = 20,
               width = 25,
               units = "cm")

mapa_presidencial_2012_distritos %>% ggplot(aes(fill = p_pri_2012)) +
          geom_sf(color = "transparent") +
          scale_fill_gradientn(colors = c("white", "red"), na.value = "white") + # 7 secciones na de origen convertidas a blanco)
          labs(title = "Voto por el PRI 2012 (%) - Nacional ", x = "Latitud", y= "PRI", caption = "Elaborado por: José Ahumada Castillo @AhumadaReal \n Fuente: INE ( https://prep2012.ife.org.mx/prep/)") +
          guides(fill=guide_legend(title="Voto por el PRI (%)"))


ggsave("outputs/2012_presidencial_mapa_distritos_pri.png",
               device = "png",
               height = 20,
               width = 25,
               units = "cm")

mapa_presidencial_2012_distritos %>% ggplot(aes(fill = p_prd_2012)) +
          geom_sf(color = "transparent") +
          scale_fill_gradientn(colors = c("white", "gold2"), na.value = "white" ) +# 7 secciones na de origen convertidas a blanco
          labs(title = "Voto por el PRD 2012 (%) - Nacional", x = "Latitud", y= "PRD", caption = "Elaborado por: José Ahumada Castillo @AhumadaReal \n Fuente: INE ( https://prep2012.ife.org.mx/prep/)") +
          guides(fill=guide_legend(title="Voto por el PRD (%)"))

ggsave("outputs/2012_presidencial_mapa_distritos_prd.png",
               device = "png",
               height = 20,
               width = 25,
               units = "cm")

mapa_presidencial_2012_distritos %>% ggplot(aes(fill = p_pvem_2012)) +
          geom_sf(color = "transparent") +
          scale_fill_gradientn(colors = c("white", "green"), na.value = "white") + # 7 secciones na de origen convertidas a blanco
          labs(title = "Voto por el PVEM 2012 (%) - Nacional", x = "Latitud", y= "PVEM", caption = "Elaborado por: José Ahumada Castillo @AhumadaReal \n Fuente: INE ( https://prep2012.ife.org.mx/prep/)") +
          guides(fill=guide_legend(title="Voto por el PVEM (%)"))


ggsave("outputs/2012_presidencial_mapa_distritos_pvem.png",
               device = "png",
               height = 20,
               width = 25,
               units = "cm")

mapa_presidencial_2012_distritos %>% ggplot(aes(fill = p_pt_2012)) +
          geom_sf(color = "transparent") +
          scale_fill_gradientn(colors = c("white", "red3"), na.value = "white") + # 7 secciones na de origen convertidas a blanco) +
          labs(title = "Voto por el PT 2012 (%) - Nacional ", x = "Latitud", y= "PT", caption = "Elaborado por: José Ahumada Castillo @AhumadaReal \n Fuente: INE ( https://prep2012.ife.org.mx/prep/)") +
          guides(fill=guide_legend(title="Voto por el PVEM (%) - Nacional "))
        guides(fill=guide_legend(title="Voto por el PT (%)"))

ggsave("outputs/2012_presidencial_mapa_distritos_pt.png",
               device = "png",
               height = 20,
               width = 25,
               units = "cm")


mapa_presidencial_2012_distritos %>% ggplot(aes(fill = p_mc_2012)) +
          geom_sf(color = "transparent") +
          scale_fill_gradientn(colors = c("white", "orange"), na.value = "white") + # 7 secciones na de origen convertidas a blanco) +) +
          labs(title = "Voto por MC 2012 (%) - Nacional ", x = "Latitud", y= "MC", caption = "Elaborado por: José Ahumada Castillo @AhumadaReal \n Fuente: INE ( https://prep2012.ife.org.mx/prep/)") +
          guides(fill=guide_legend(title="Voto por MC (%)"))

ggsave("outputs/2012_presidencial_mapa_distritos_mc.png",
               device = "png",
               height = 20,
               width = 25,
               units = "cm")

################################################################################
################################################################################

#-------------------------------------------------------------------------------
# (2) presidencia 2018 distritos
#-------------------------------------------------------------------------------

       # getwd()
        #setwd("C:/Users/Jose Ahumada/Desktop/github/coalicion_mc_2024")

        # mapas_secciones_2021 <- st_read("SECCION.shp")
        #mapas_secciones_2021 <- readRDS("shape.rds")

        #mapas_secciones_2021 <- mapas_secciones_2021 %>%
        # clean_names()
        #mapas_secciones_2021$entidad <- as.numeric(mapas_secciones_2021$entidad)


presidencial_2018_distritos <- readRDS("outputs/presidencial_2018_distritos_federales.rds")
mapa_300distritos_federales <- st_read("inputs/DISTRITO_FEDERAL.shp")
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


        mapa_300distritos_federales %>% mutate(llave = str_c(entidad, "_", distrito_f))
        mapa_300distritos_federales <- mapa_300distritos_federales%>% mutate(llave = str_c(entidad, "_", distrito_f))


        presidencial_2018_distritos%>% mutate(llave = str_c(id_estado, "_", id_distrito))
        presidencial_2018_distritos <- presidencial_2018_distritos%>% mutate(llave = str_c(id_estado, "_", id_distrito))



        left_join(mapa_300distritos_federales, presidencial_2018_distritos, by = "llave", na.rm = TRUE )
        mapa_presidencial_2018_distritos <- left_join(mapa_300distritos_federales, presidencial_2018_distritos, by = "llave" )

        saveRDS(mapa_presidencial_2018_distritos, ("outputs/presidencial_2018_distritos.rds")) #save


mapa_presidencial_2018_distritos %>% ggplot(aes(fill = p_morena_2018)) +
geom_sf(color = "transparent") +
scale_fill_gradientn(colors = c("white", "violetred4")) +
labs(title = "Voto por MORENA 2018 (%) - Nacional", x = "Latitud", y= "MORENA", caption = "Elaborado por: Jose Ahumada Castillo @AhumadaReal \n Fuente: INE ( computos2018.ine.mx/)") +
guides(fill=guide_legend(title="Voto por Morena (%)"))



ggsave("outputs/2018_presidencial_mapa_distritos_morena.png",
               device = "png",
               height = 20,
               width = 25,
               units = "cm")

mapa_presidencial_2018_distritos %>% ggplot(aes(fill = p_pan_2018)) +
          geom_sf(color = "transparent") +
          scale_fill_gradientn(colors = c("white", "blue3")) +
          labs(title = "Voto por el PAN 2018 (%) - Nacional", x = "Latitud", y= "PAN", caption = "Elaborado por: Jose Ahumada Castillo @AhumadaReal \n Fuente: INE ( computos2018.ine.mx/)") +
          guides(fill=guide_legend(title="Voto por el PAN (%)"))


ggsave("outputs/2018_presidencial_mapa_distritos_pan.png",
               device = "png",
               height = 20,
               width = 25,
               units = "cm")

mapa_presidencial_2018_distritos %>% ggplot(aes(fill = p_pri_2018)) +
          geom_sf(color = "transparent") +
          scale_fill_gradientn(colors = c("white", "red")) +
          labs(title = "Voto por el PRI 2018 (%) - Nacional", x = "Latitud", y= "PRI", caption = "Elaborado por: Jose Ahumada Castillo @AhumadaReal \n Fuente: INE ( computos2018.ine.mx/)") +
          guides(fill=guide_legend(title="Voto por el PRI (%)"))


ggsave("outputs/2018_presidencial_mapa_distritos_pri.png",
               device = "png",
               height = 20,
               width = 25,
               units = "cm")

mapa_presidencial_2018_distritos %>% ggplot(aes(fill = p_prd_2018)) +
          geom_sf(color = "transparent") +
          scale_fill_gradientn(colors = c("white", "gold2")) +
          labs(title = "Voto por el PRD 2018 (%) - Nacional ", x = "Latitud", y= "PRD", caption = "Elaborado por: Jose Ahumada Castillo @AhumadaReal \n Fuente: INE ( computos2018.ine.mx/)") +
          guides(fill=guide_legend(title="Voto por el PRD (%)"))

ggsave("outputs/2018_presidencial_mapa_distritos_prd.png",
               device = "png",
               height = 20,
               width = 25,
               units = "cm")

mapa_presidencial_2018_distritos %>% ggplot(aes(fill = p_pvem_2018)) +
          geom_sf(color = "transparent") +
          scale_fill_gradientn(colors = c("white", "green")) +
          labs(title = "Voto por el PVEM 2018 (%) - Nacional", x = "Latitud", y= "PVEM", caption = "Elaborado por: Jose Ahumada Castillo @AhumadaReal \n Fuente: INE ( computos2018.ine.mx/)") +
          guides(fill=guide_legend(title="Voto por el PVEM (%)"))


ggsave("outputs/2018_presidencial_mapa_distritos_pvem.png",
               device = "png",
               height = 20,
               width = 25,
               units = "cm")

mapa_presidencial_2018_distritos %>% ggplot(aes(fill = p_pt_2018)) +
          geom_sf(color = "transparent") +
          scale_fill_gradientn(colors = c("white", "red3")) +
          labs(title = "Voto por el PT 2018 (%) - Nacional", x = "Latitud", y= "PT", caption = "Elaborado por: Jose Ahumada Castillo @AhumadaReal \n Fuente: INE ( computos2018.ine.mx/)") +
        guides(fill=guide_legend(title="Voto por el PT (%)"))

ggsave("outputs/2018_presidencial_mapa_distritos_pt.png",
               device = "png",
               height = 20,
               width = 25,
               units = "cm")


mapa_presidencial_2018_distritos %>% ggplot(aes(fill = p_mc_2018)) +
          geom_sf(color = "transparent") +
          scale_fill_gradientn(colors = c("white", "orange")) +
          labs(title = "Voto por MC 2018 (%) - Nacional ", x = "Latitud", y= "MC", caption = "Elaborado por: Jose Ahumada Castillo @AhumadaReal \n Fuente: INE ( computos2018.ine.mx/")
          guides(fill=guide_legend(title="Voto por MC (%)"))

ggsave("outputs/2018_presidencial_mapa_distritos_mc.png",
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









