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

max(diputaciones_2021$morena_2021)

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




revocacion_unidades <- left_join(revocacion, unidades)%>%
  group_by(id_estado, unidad_territorial)%>%
  summarize(revoque=sum(revoque, na.rm=T),
            siga=sum(siga, na.rm=T),
            nulos_2022=sum(nulos_2022, na.rm=T),
            total_votos_calculados_2022=sum(total_votos_calculados_2022, na.rm=T),
            lista_nominal_2022=sum(lista_nominal_2022, na.rm=T))




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


diputaciones_2021_revocacion <- left_join(diputaciones_2021, revocacion)%>%
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

diputaciones_revocacion_unidades <- na.omit(diputaciones_revocacion_unidades)















MORENA_2021_SeQuede_2022_UNIDADES <- diputaciones_revocacion_unidades %>% ggplot(aes(x= p_morena_2021, y=p_siga))+
  geom_point(size = .5, color = "violetred4")+
  # geom_smooth(method = lm, se = FALSE, color= "black", linetype = "dashed") +
  labs(x="Votos a Diputaciones Federales por Morena 2021 Nacional (%)",
       y="Votos para que AMLO Siga 2022 Nacional(%)",
       title="Votos a Diputaciones Federales por TOTAL Morena 2021 Nacional (%) vs \n Votos para que AMLO  Siga 2022 Nacional(%)",
       subtitle="Por unidad territorial", caption = "José Ahumada Castillo @AhumadaReal") +
  theme_economist()

print(MORENA_2021_SeQuede_2022_UNIDADES)


ggsave("outputs/MORENA_2021_SeQuede_2022_UNIDADES.png",
       device = "png",
       height = 20,
       width = 25,
       units = "cm")

