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

bearbeiter = 'Kai@Home'
# loading data
if(bearbeiter == 'Alex'){
  Umfrage <- read.csv2('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Daten_Kneib/Stick/buergerumfrage/population_aufbereitet.txt')
  Zensus <- read.csv2('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Daten_Kneib/Stick/zensus/population_aufbereitet.txt')
}
if(bearbeiter == 'Kai@Home'){
  Umfrage <- read.csv2('/home/kai/Dokumente/Master/Stat_Practical/Statistical-Practical/Rohdaten/buergerumfrage/population_aufbereitet.txt')
  Zensus <- read.csv2('/home/kai/Dokumente/Master/Stat_Practical/Statistical-Practical/Rohdaten/zensus/population_aufbereitet.txt')
}
if(bearbeiter == 'Kai@Work') {

}


#--------------------------------------------------#
# Populationen in die Form der Schätzungen bringen #
#--------------------------------------------------#

# Auswahl der passenden Variablen
u.p <- select(Umfrage, Altersklasse,Haushaltsgroesse, Geschlecht,Familienstand, Nationalitaet,GaussX, GaussY, Stadtteil )
names(u.p) <- c("Altersklasse.Befragter","Personenzahl.im.Haushalt", "Geschlecht","Familienstand","Nationalität","X" ,"Y", 'Stadtteil')

z.p <- select(Zensus, alter,haushaltsgroesse, geschlecht, familienstand, staatsangehoerigkeit, xcoord, ycoord, stadtteil )
names(z.p) <- c("Altersklasse.Befragter","Personenzahl.im.Haushalt","Geschlecht","Familienstand","Nationalität","X" ,"Y", 'Stadtteil')

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
z.p2 <- z.p
z.p2$Altersklasse.Befragter <- ""
z.p2$Altersklasse.Befragter[z.p$Altersklasse.Befragter == 1] <- 1
z.p2$Altersklasse.Befragter[z.p$Altersklasse.Befragter == 2 | z.p$Altersklasse.Befragter == 3] <- 2
z.p2$Altersklasse.Befragter[z.p$Altersklasse.Befragter == 4 | z.p$Altersklasse.Befragter == 5] <- 3
z.p2$Altersklasse.Befragter[z.p$Altersklasse.Befragter == 6 | z.p$Altersklasse.Befragter == 7] <- 4
z.p2$Altersklasse.Befragter[z.p$Altersklasse.Befragter == 8 | z.p$Altersklasse.Befragter == 9] <- 5

# Anpassen der Nationalität
u.p2$Nationalität <- ''
u.p2$Nationalität[u.p$Nationalität == 'Deutsch'] <- 'Deutsch'
u.p2$Nationalität[u.p$Nationalität == 'Nicht-Deutsch'] <- 'Nicht Deutsch'

z.p2$Nationalität <- ''
z.p2$Nationalität[z.p$Nationalität == 'Deutschland'] <- 'Deutsch'
z.p2$Nationalität[z.p$Nationalität == 'Ausland'] <- 'Nicht Deutsch'

# Anpassen der Geschlechter
u.p2$Geschlecht <- ''
u.p2$Geschlecht[u.p$Geschlecht == 'Mann'] <- 'Männlich'
u.p2$Geschlecht[u.p$Geschlecht == 'Frau'] <- 'Weiblich'

z.p2$Geschlecht <- ''
z.p2$Geschlecht[z.p$Geschlecht == 'maennlich'] <- 'Männlich'
z.p2$Geschlecht[z.p$Geschlecht == 'weiblich'] <- 'Weiblich'

# Klasse der Variablen festlegen
u.p2$Altersklasse.Befragter <- as.integer(u.p$Altersklasse.Befragter)
u.p2$Geschlecht <- as.factor(u.p2$Geschlecht)
u.p2$Nationalität <- as.factor(u.p2$Nationalität)

z.p2$Altersklasse.Befragter <- as.integer(z.p$Altersklasse.Befragter)
z.p2$Geschlecht <- as.factor(z.p2$Geschlecht)
z.p2$Nationalität <- as.factor(z.p2$Nationalität)

# Familienstand
z.p2$Familienstand <- ''
z.p2$Familienstand[z.p$Familienstand == 'geschieden'] <- 'geschieden'
z.p2$Familienstand[z.p$Familienstand == 'ledig'] <- 'ledig'
z.p2$Familienstand[z.p$Familienstand == 'verheiratet/eingetragene Partnerschaft'] <- 'verheiratet'
z.p2$Familienstand[z.p$Familienstand == 'verwitwet'] <- 'verwitwet'
z.p2$Familienstand <- as.factor(z.p2$Familienstand)

# Personzahl

z.p2$Personenzahl.im.Haushalt <- ''
z.p2$Personenzahl.im.Haushalt[z.p$Personenzahl.im.Haushalt == '1'] <- 1
z.p2$Personenzahl.im.Haushalt[z.p$Personenzahl.im.Haushalt == '2'] <- 2
z.p2$Personenzahl.im.Haushalt[z.p$Personenzahl.im.Haushalt == '3'] <- 3
z.p2$Personenzahl.im.Haushalt[z.p$Personenzahl.im.Haushalt == '4'] <- 4
z.p2$Personenzahl.im.Haushalt[z.p$Personenzahl.im.Haushalt == '5'] <- 5
z.p2$Personenzahl.im.Haushalt[z.p$Personenzahl.im.Haushalt == '6'] <- 5

z.p2$Personenzahl.im.Haushalt <- as.numeric(z.p2$Personenzahl.im.Haushalt)

#-----------------------------------------# Model mit stetigen räumlichen Informationen #------------------------------------#

# Laden der GAM Schätzungen
model2 <- readRDS('step.model.rds') # Gauss Krüger Informationen mit 3 Kategorien

# Predicten der Umfrage population
pred.pop.u.3 <- predict.gam(model2, newdata = u.p2, type = 'response')
pred.pop.z.3 <- predict.gam(model2, newdata = z.p2, type = 'response')

# Räumliche Informationen dazu fügen
pred.pop.u <- cbind(pred.pop.u.3, u.p2$X, u.p2$Y, u.p2$Stadtteil)
pred.pop.z <- cbind(pred.pop.z.3, z.p2$X, z.p2$Y, z.p2$Stadtteil)

colnames(pred.pop.u) <- c('1', '2', '3', 'X', 'Y', 'Stadtteil')
colnames(pred.pop.z) <- c('1', '2', '3', 'X', 'Y', 'Stadtteil')

# ermitterln der Gruppe mir höchster Wahrscheinlichkeit
Meinung <- rep(0, nrow(pred.pop.u))
for(i in 1:nrow(pred.pop.u)){
  if(pred.pop.u[i,1] > pred.pop.u[i,2] & pred.pop.u[i,1] > pred.pop.u[i,3]){
    Meinung[i] <- 1
  }
  if(pred.pop.u[i,2] > pred.pop.u[i,1] & pred.pop.u[i,2] > pred.pop.u[i,3]){
    Meinung[i] <- 2
  } 
  if(pred.pop.u[i,3] > pred.pop.u[i,1] & pred.pop.u[i,3] > pred.pop.u[i,2]){
    Meinung[i] <- 3
  }
}
pred.pop.u <- cbind(pred.pop.u, Meinung)

Meinung <- rep(0, nrow(pred.pop.z))
for(i in 1:nrow(pred.pop.z)){
  if(pred.pop.z[i,1] > pred.pop.z[i,2] & pred.pop.z[i,1] > pred.pop.z[i,3]){
    Meinung[i] <- 1
  }
  if(pred.pop.z[i,2] > pred.pop.z[i,1] & pred.pop.z[i,2] > pred.pop.z[i,3]){
    Meinung[i] <- 2
  } 
  if(pred.pop.z[i,3] > pred.pop.z[i,1] & pred.pop.z[i,3] > pred.pop.z[i,2]){
    Meinung[i] <- 3
  }
}
pred.pop.z <- cbind(pred.pop.z, Meinung)

# Speichern der Geschätzten Grundgesamtheit
write.table(pred.pop.u, file="Pop_geschätzt_u_3.csv", sep=";", col.names=TRUE, row.names=FALSE, quote=FALSE)
write.table(pred.pop.z, file="Pop_geschätzt_u_3.csv", sep=";", col.names=TRUE, row.names=FALSE, quote=FALSE)

# Häufigkeiten der geschätzten Klassen
count_u_cs <- as.data.frame(table(pred.pop.u[,7]))
write.table(count_u_cs, file = 'count_u_cs.csv', sep = ';', col.names = T, row.names = F)

count_z_cs <- as.data.frame(table(pred.pop.z[,7]))
write.table(count_z_cs, file = 'count_z_cs.csv', sep = ';', col.names = T, row.names = F)
