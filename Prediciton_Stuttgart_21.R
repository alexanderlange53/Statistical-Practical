###########################
# Prediction Stuttgart 21 #
###########################

rm(list=ls())
# loading packages
require('ROCR')
require('mgcv')
require('splines')
require(rgdal);require(rgeos)
require(ggplot2)
require(maptools);require(rvest);require(dplyr)

# Laden von Populationen
Umfrage <- read.csv2('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Daten_Kneib/Stick/buergerumfrage/population_aufbereitet.txt')
Zensus <- read.csv2('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Daten_Kneib/Stick/zensus/population_aufbereitet.txt')

#--------------------------------------------------#
# Populationen in die Form der Schätzungen bringen #
#--------------------------------------------------#

# Auswahl der passenden Variablen
u.p <- select(Umfrage, Haushaltsgroesse,Altersklasse,Geschlecht,Familienstand, Nationalitaet,GaussX, GaussY )
names(u.p) <- c("Personenzahl.im.Haushalt","Altersklasse.Befragter","Geschlecht","Familienstand","Nationalität","X" ,"Y")

# Anpassen der Akltersklassen
u.p2 <- u.p
u.p2$Altersklasse.Befragter <- ""
u.p2$Altersklasse.Befragter[u.p$Altersklasse.Befragter == 1] <- 1
u.p2$Altersklasse.Befragter[u.p$Altersklasse.Befragter == 2 | u.p$Altersklasse.Befragter == 3] <- 2
u.p2$Altersklasse.Befragter[u.p$Altersklasse.Befragter == 4 | u.p$Altersklasse.Befragter == 5] <- 3
u.p2$Altersklasse.Befragter[u.p$Altersklasse.Befragter == 6 | u.p$Altersklasse.Befragter == 7] <- 4
u.p2$Altersklasse.Befragter[u.p$Altersklasse.Befragter == 8 | u.p$Altersklasse.Befragter == 9] <- 5
u.p2$Altersklasse.Befragter[u.p$Altersklasse.Befragter == 10 | u.p$Altersklasse.Befragter == 11 
                            | u.p$Altersklasse.Befragter == 12 | u.p$Altersklasse.Befragter == 13
                            | u.p$Altersklasse.Befragter == 14] <- 6

# Anpassen der Nationalität
u.p2$Nationalität <- ''
u.p2$Nationalität[u.p$Nationalität == 'Deutsch'] <- 'Deutsch'
u.p2$Nationalität[u.p$Nationalität == 'Nicht-Deutsch'] <- 'Nicht Deutsch'

# Anpassen der Geschlechter
u.p2$Geschlecht <- ''
u.p2$Geschlecht[u.p$Geschlecht == 'Mann'] <- 'Männlich'
u.p2$Geschlecht[u.p$Geschlecht == 'Frau'] <- 'Weiblich'

# Klasse der Variablen festlegen
u.p2$Altersklasse.Befragter <- as.integer(u.p$Altersklasse.Befragter)
u.p2$Geschlecht <- as.factor(u.p2$Geschlecht)
u.p2$Nationalität <- as.factor(u.p2$Nationalität)


#-----------------------------------------# Model mit stetigen räumlichen Informationen #------------------------------------#

# Laden der GAM Schätzungen
model1 <- readRDS('model1.rds') # Gauss Krüger Informationen mit 5 Kategorien
model2 <- readRDS('model2.rds') # Gauss Krüger Informationen mit 3 Kategorien

# Predicten der Umfrage population
pred.pop.u.5 <- predict.gam(model1, newdata = u.p2, type = 'response')
pred.pop.u.3 <- predict.gam(model2, newdata = u.p2, type = 'response')

# Räumliche Informationen dazu fügen
pred.pop.u.5 <- cbind(pred.pop.u.5, u.p2$X, u.p2$Y)
pred.pop.u.3 <- cbind(pred.pop.u.3, u.p2$X, u.p2$Y)

colnames(pred.pop.u.5) <- c('1', '2', '3', '4', '5', 'X', 'Y')
colnames(pred.pop.u.3) <- c('1', '2', '3', 'X', 'Y')

# Speichern der Geschätzten Grundgesamtheit
write.table(pred.pop.u.5, file="Pop_geschätzt_u_5.csv", sep=";", col.names=TRUE, row.names=FALSE, quote=FALSE)
write.table(pred.pop.u.3, file="Pop_geschätzt_u_3.csv", sep=";", col.names=TRUE, row.names=FALSE, quote=FALSE)
