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

barplot_2000 <- read.csv("barplot_2000.csv")                      #load
barplot_2000$Partido <- factor(barplot_2000$Partido, levels = c('PAN', 'PRI', 'PRD'))

primero_2000 <- 42.52

segundo_2000 <- 36.11

tercero_2000 <- 16.64

primero_2000 + segundo_2000 + tercero_2000

primero_2000 - segundo_2000
segundo_2000 - tercero_2000


barplot_2000 <- ggplot(barplot_2000, aes(Partido, dosmil, fill = Partido)) +
  geom_bar(stat="identity", width= .5) +
  scale_fill_manual(values=c("blue", "red", "yellow")) +
  labs(x=" Partidos/Cabeza de Coalición para Presidencia 2000 (%)",
       y="Votos para Presidencial 2000 (%)",
       title="Votos para Presidencia 2000 (%)",
       subtitle="Nacional\nDiferencia entre primero y segundo = 6.41\nDiferencia entre segundo y tercero = 19.47  ", caption = "José Ahumada Castillo   tw: @AhumadaReal\n Por simplificación se pone el partido más grande de la coalición \n Solo se presentan los primeros tres lugares, \n los restantes completan la suma de % a 100") +
  geom_text(aes(label = dosmil), position = position_dodge(.9), size=5, vjust = 1.5, size = 2, color = "black")

barplot_2000


# 2006

barplot_2006 <- read.csv("barplot_2006.csv")                      #load

primero_2006 <- 35.89

segundo_2006 <- 35.31

tercero_2006 <- 22.26

primero_2006 + segundo_2006 + tercero_2006

primero_2006 - segundo_2006
segundo_2006 - tercero_2006

barplot_2006 <- ggplot(barplot_2006, aes(Partido, dosmilseis, fill = Partido)) +
  geom_bar(stat="identity", width= .5) +
  scale_fill_manual(values=c("blue", "yellow", "red")) +
  labs(x=" Partidos/Cabeza de Coalición para Presidencia 2006 (%)",
       y="Votos para Presidencial 2006 (%)",
       title="Votos para Presidencia 2006 (%)",
       subtitle="Nacional\nDiferencia entre primero y segundo = .58\nDiferencia entre segundo y tercero = 13.05  ", caption = "José Ahumada Castillo   tw: @AhumadaReal\n Por simplificación se pone el partido más grande de la coalición \n Solo se presentan los primeros tres lugares, \n los restantes completan la suma de % a 100") +
  geom_text(aes(label = dosmilseis), position = position_dodge(.9), size=5, vjust = 1.5, size = 2, color = "black")

barplot_2006



# 2012

barplot_2012 <- read.csv("barplot_2012.csv")    #load
barplot_2012$Partido <- factor(barplot_2012$Partido, levels = c('PRI', 'PRD', 'PAN'))


primero_2012 <- 38.2

segundo_2012 <- 32.6

tercero_2012 <- 25.39

primero_2012 + segundo_2012 + tercero_2012

primero_2012 - segundo_2012
segundo_2012 - tercero_2012

barplot_2012 <- ggplot(barplot_2012, aes(Partido, dosmildoce, fill = Partido)) +
  geom_bar(stat="identity", width= .5) +
  scale_fill_manual(values=c("red", "yellow", "blue")) +
  labs(x=" Partidos/Cabeza de Coalición para Presidencia 2012 (%)",
       y="Votos para Presidencial 2012 (%)",
       title="Votos para Presidencia 2012 (%)",
       subtitle="Nacional\nDiferencia entre primero y segundo = 5.6\nDiferencia entre segundo y tercero = 7.21  ", caption = "José Ahumada Castillo   tw: @AhumadaReal\n Por simplificación se pone el partido más grande de la coalición \n Solo se presentan los primeros tres lugares, \n los restantes completan la suma de % a 100") +
  geom_text(aes(label = dosmildoce), position = position_dodge(.9), size=5, vjust = 1.5, size = 2, color = "black")

barplot_2012


# 2018

barplot_2018 <- read.csv("barplot_2018.csv")    #load
barplot_2018$Partido <- factor(barplot_2018$Partido, levels = c('MORENA', 'PAN', 'PRI'))


primero_2018 <- 53.19

segundo_2018 <- 22.27

tercero_2018 <- 16.40

primero_2018 + segundo_2018 + tercero_2018

primero_2018 - segundo_2018
segundo_2018 - tercero_2018

barplot_2018 <- ggplot(barplot_2018, aes(Partido, dosmildieciocho, fill = Partido)) +
  geom_bar(stat="identity", width= .5) +
  scale_fill_manual(values=c("violetred4", "blue", "red")) +
  labs(x=" Partidos/Cabeza de Coalición para Presidencia 2018 (%)",
       y="Votos para Presidencial 2018 (%)",
       title="Votos para Presidencia 2018 (%)",
       subtitle="Nacional\nDiferencia entre primero y segundo = 30.92\nDiferencia entre segundo y tercero = 5.87  ", caption = "José Ahumada Castillo   tw: @AhumadaReal\n Por simplificación se pone el partido más grande de la coalición \n Solo se presentan los primeros tres lugares, \n los restantes completan la suma de % a 100") +
  geom_text(aes(label = dosmildieciocho), position = position_dodge(.9), size=5, vjust = 1.5, size = 2, color = "black")

barplot_2018









