#Proyecto: mc
# Electoralmente que es MC? Que tanto la coalicion anti amlo lo necesita?
# Autor: Jose Ahumada Castillo Tw @AhumadaReal
#_______________________________________________________________________________
#
#_______________________________________________________________________________

rm(list = ls())#Clears variable environment
cat("\014")     #Clears console
#_______________________________________________________________________________
#----------------------------------
# (1)Packages:
#-----------------------------------
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
library(car)
library(ggpubr)


#-------------------------------------------------------------------------------
# data y directorios
#-------------------------------------------------------------------------------
 #getwd()
 setwd("C:/Users/Jose Ahumada/Desktop/github/coalicion_mc_2024")
#_______________________________________________________________________________
# Primera base de origen
# Elecciones presidenciales 2012 a nivel casilla
#_______________________________________________________________________________

# 2000
primero <- 42.52

segundo <- 36.11

tercero <- 16.64

primero + segundo + tercero

primero - segundo
segundo - tercero

barplot_2000 <- read.csv("barplot.csv")                      #load


barplot_2000 <- ggplot(barplot, aes(Partido, dosmil, fill = Partido)) +
  geom_bar(stat="identity", width= .5) +
  scale_fill_manual(values=c("blue", "yellow", "red")) +
  labs(x=" Partidos para Presidencia 2000 (%)",
       y="Votos para Candidatura Presidencial 2000 (%)",
       title="Votos para Presidencia 2000 (%)",
       subtitle="Nacional \n Diferencia entre primero y segundo = 6.41 \n Diferencia entre segundo y tercero = 19.47  ", caption = "Jose Ahumada Castillo \n Por simplificacion se pone el partido mas grande de la coalicion \n Solo se presentan los primeros tres lugares \n los restantes completan la suma de porcentajes a 100") +
  geom_text(aes(label = dosmil), position = position_dodge(.9), size=5, vjust = 1.5, size = 2, color = "black")
barplot_2000

labs(x=" Votos para Presidencia por MC 2018 (%)",
     y="Votos para que AMLO sea Revocado 2022 (%)",
     title="Votos para Presidencia por MC 2018 (%) vs \n Votos para que AMLO sea Revocado 2022 (%)",
     subtitle="Por Distrito", caption = "Jose Ahumada Castillo")



%>%
aes(barplot, Partido, dosmil)) + geom_bar(stat="identity"width=0.5) +


library(ggplot2)
# Basic barplot
p<-ggplot(data=df, aes(x=dose, y=len)) +
  geom_bar(stat="identity")
p

# Horizontal bar plot
p + coord_flip()

# Change the width of bars
ggplot(data=df, aes(x=dose, y=len)) +
  geom_bar(stat="identity", width=0.5)
# Change colors
ggplot(data=df, aes(x=dose, y=len)) +
  geom_bar(stat="identity", color="blue", fill="white")
# Minimal theme + blue fill color
p<-ggplot(data=df, aes(x=dose, y=len)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()
p








+
  scale_fill_manual("legend", values = c("PAN" = "blue", "PRD" = "yellow", "PRI" = "red"))


ggplot(memefin, aes(x = Year, fill = Year))+
  geom_bar()+

