#Autor: Jose Ahumada Castillo
#Proyecto: Mapas
#Fecha: 11 mayo 2021 3:15pm
#-------------------------------------------------------------------------------
# (1)Packages:
#-------------------------------------------------------------------------------
library(ggplot2)
library(janitor)
library(sf)
library(tidyverse)
library(ggthemes)



getwd()

setwd("C:/Users/Jose Ahumada/Desktop/mapa distritos federales/mapas_electorales_secciones/SECCION")

# mapas_secciones_2021 <- st_read("SECCION.shp")
mapas_secciones_2021 <- readRDS("shape.rds")

mapas_secciones_2021 <- mapas_secciones_2021 %>% 
  clean_names()
mapas_secciones_2021$entidad <- as.numeric(mapas_secciones_2021$entidad)
  


diputaciones_2021_secciones <- read.csv("diputaciones2021.csv")
# mapa_300distritos_federales <- st_read("shapefiles_electorales_2021_distrirales/DISTRITO_FEDERAL.shp")


diputaciones_2021_secciones <- diputaciones_2021_secciones %>%
  clean_names() 

diputaciones_2021_secciones <- diputaciones_2021_secciones %>%
  filter(seccion != 0) %>%
  group_by(id_estado, seccion) %>%
  dplyr::summarize(pan= sum(pan, na.rm = T),
                   pri= sum(pri, na.rm = T),
                   prd= sum(prd, na.rm = T),
                   pvem= sum(pvem, na.rm = T),
                   pt= sum(pt, na.rm = T),
                   mc= sum(pri, na.rm = T),
                   morena= sum(pri, na.rm = T),
                   pes= sum(pri, na.rm = T),
                   rsp= sum(pri, na.rm = T),
                   fxm= sum(pri, na.rm = T),
                   ci= sum(pri, na.rm = T),
                   pan_pri_prd= sum(pan_pri_prd, na.rm = T),
                   pan_pri= sum(pan_pri, na.rm = T),
                   pan_prd= sum(pan_prd, na.rm = T),
                   pvem_pt_morena= sum(pvem_pt_morena, na.rm = T),
                   pvem_pt= sum(pvem_pt, na.rm = T),
                   pvem_morena= sum(pvem_morena, na.rm = T),
                   candidato_a_no_registrado_a= sum(candidato_a_no_registrado_a, na.rm = T),
                   votos_nulos= sum(votos_nulos, na.rm = T),
                   total_votos_calculados= sum(total_votos_calculados, na.rm = T),
                   lista_nominal_casilla= sum(lista_nominal_casilla, na.rm = T)) %>%
                   dplyr::select(id_estado, seccion, pan, pri, prd, pvem, pt, mc, morena, pes, rsp, fxm, ci, pan_pri_prd, pan_pri, pan_prd, pvem_pt_morena, pvem_pt, pvem_morena, candidato_a_no_registrado_a, votos_nulos, total_votos_calculados, lista_nominal_casilla)

diputaciones_2021_secciones <- diputaciones_2021_secciones %>%
  mutate(p_pan = ((pan / total_votos_calculados) * 100),
         p_pri = ((pri / total_votos_calculados)* 100),
         p_prd = ((prd / total_votos_calculados)* 100),
         p_pvem = ((pvem / total_votos_calculados)* 100),
         p_pt = ((pt / total_votos_calculados)* 100),
         p_mc = ((mc / total_votos_calculados)* 100),
         p_morena = ((morena / total_votos_calculados)* 100),
         p_pes = ((pes / total_votos_calculados)* 100),
         p_rsp = ((rsp / total_votos_calculados)* 100),
         p_fxm = ((fxm / total_votos_calculados)* 100),
         p_ci = ((ci / total_votos_calculados)* 100),
         p_pan_pri_prd = ((pan_pri_prd / total_votos_calculados)* 100),
         p_pan_pri = ((pan_pri / total_votos_calculados)* 100),
         p_pan_prd = ((pan_prd / total_votos_calculados)* 100),
         p_pvem_pt_morena = ((pvem_pt_morena / total_votos_calculados)* 100),
         p_pvem_pt = ((pvem_pt / total_votos_calculados)* 100),
         p_pvem_morena = ((pvem_morena / total_votos_calculados)* 100),
         p_candidato_a_no_registrado_a= ((candidato_a_no_registrado_a / total_votos_calculados)* 100),
         p_votos_nulos = ((votos_nulos / total_votos_calculados)* 100),
         p_total_votos_calculados= ((total_votos_calculados / total_votos_calculados)* 100),
         p_lista_nominal_casilla = ((lista_nominal_casilla / total_votos_calculados)* 100))

# View(diputaciones_2021_300distritos)
# View(mapa_300distritos_federales)

mapas_secciones_2021 %>% mutate(llave = str_c(entidad, "_", seccion))
mapas_secciones_2021 <- mapas_secciones_2021%>% mutate(llave = str_c(entidad, "_", seccion))


diputaciones_2021_secciones%>% mutate(llave = str_c(id_estado, "_", seccion))
diputaciones_2021_secciones <- diputaciones_2021_secciones%>% mutate(llave = str_c(id_estado, "_", seccion))



left_join(mapas_secciones_2021, diputaciones_2021_secciones, by = "llave", na.rm = TRUE )
mapa_diputaciones_2021_secciones <- left_join(mapas_secciones_2021, diputaciones_2021_secciones, by = "llave" )
mapa_diputaciones_2021_secciones <- na.omit(mapa_diputaciones_2021_secciones)


mapa_diputaciones_2021_secciones %>% ggplot(aes(fill = p_morena)) +
  geom_sf(color = "transparent") +
  scale_fill_gradientn(colors = c("white", "violetred4")) +
  labs(title = "Voto por MORENA (%)", x = "Latitud", y= "MORENA", caption = "Elaborado por: Jose Ahumada Castillo @AhumadaReal \n Fuente: INE ( computos2018.ine.mx/ , https://portalanterior.ine.mx/)")

ggsave("mapa_secciones_p_MORENA_2021_diputados_MR",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")





mapa_diputaciones_2021_secciones %>% ggplot(aes(fill = p_mc)) +
  geom_sf(color = "transparent") +
  scale_fill_gradientn(colors = c("white", "orange")) +
  labs(title = "Voto por MC (%)", x = "Latitud", y= "MC", caption = "Elaborado por: Jose Ahumada Castillo @AhumadaReal \n Fuente: INE ( computos2018.ine.mx/ , https://portalanterior.ine.mx/)")

ggsave("mapa_secciones_p_MC_2021_diputados_MR",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")
#-------------------------------------------------------------------------------
# Estados
# ------------------------------------------------------------------------------

aguascalientes <- mapa_diputaciones_2021_secciones %>% 
  filter(entidad == 1)

aguascalientes %>% ggplot(aes(fill = p_mc)) +
  geom_sf(color = "transparent") +
  scale_fill_gradientn(colors = c("white", "orange")) +
  labs(title = "Aguascalientes Voto por MC (%)", x = "Latitud", y= "MC", caption = "Jose Ahumada Castillo")

yucatan  <- mapa_diputaciones_2021_secciones %>% 
  filter(entidad == 31)

yucatan %>% ggplot(aes(fill = p_morena)) +
  geom_sf(color = "transparent") +
  scale_fill_gradientn(colors = c("white", "red")) +
  labs(title = "Yucatan Voto por Morena (%)", x = "Latitud", y= "Morena", caption = "Jose Ahumada Castillo")

ggsave("mapa_secciones_p_morena_2021_diputados_MR",
       device = "png",
       height = 16,
       width = 14,
       units = "cm")




yucatan %>% ggplot(aes(fill = p_pan)) +
  geom_sf(color = "transparent") +
  scale_fill_gradientn(colors = c("white", "blue")) +
  labs(title = "Yucatan Voto por PAN (%)", x = "Latitud", y= "PAN", caption = "Jose Ahumada Castillo")

ggsave("mapa_secciones_p_PAN_2021_diputados_MR",
       device = "png",
       height = 16,
       width = 14,
       units = "cm")

yucatan %>% ggplot(aes(fill = p_mc)) +
  geom_sf(color = "transparent") +
  scale_fill_gradientn(colors = c("white", "orange")) +
  labs(title = "Yucatan Voto por MC (%)", x = "Latitud", y= "MC", caption = "Jose Ahumada Castillo")

ggsave("mapa_secciones_p_MC_2021_diputados_MR",
       device = "png",
       height = 16,
       width = 14,
       units = "cm")

