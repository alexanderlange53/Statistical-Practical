#####################################
# Estimation of Stuttgart 21 survey #
#####################################
rm(list=ls())
# loading packages
require('ROCR')
require('mgcv')
require('splines')
require(rgdal);require(rgeos)
require(VGAM)

bearbeiter = 'Alex'
# loading data
if(bearbeiter == 'Alex'){
  dataS <- read.csv2('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Auswertung/Neue_Daten/Stuttgart21_aufbereitet.csv',
                     dec = '.')
  bezirke <- readOGR(dsn = "/home/alex/Schreibtisch/Uni/statistisches_praktikum/Auswertung/Geodaten/bezirke", layer = "bezirke")
} else {
  dataS <- read.csv2('/home/khusmann/mnt/U/Promotion/Kurse/Stat_Praktikum/Auswertung/Neue_Daten//Stuttgart21_aufbereitet.csv',
                     dec = '.')
  bezirke <- readOGR(dsn = "/home/khusmann/mnt/U/Promotion/Kurse/Stat_Praktikum/Auswertung/Geodaten/bezirke/", layer = "bezirke")
  
}

# ordering ordinal data
dataS$Altersklasse.Befragter <- ordered(dataS$Altersklasse.Befragter, levels = c('15-<25', '25-<35', '35-<45', '45-<55', 
                                              '55-<65', '>65'))
dataS$Personenzahl.im.Haushalt <- ordered(dataS$Personenzahl.im.Haushalt , levels = c('1', '2', '3', '4', 
                                                                                        '>5'))
dataS$Monatliches.Netto.Haushaltseinkommen <- ordered(dataS$Monatliches.Netto.Haushaltseinkommen , levels = c('<900', '900-<2000', '2000-<2900', '2900-<4000', 
                                                                                                                '4000-<5000', '>5000'))

model1 <- vgam(Meinung.zu.Stuttgart.21 ~ Geschlecht + Familienstand + s(Altersklasse.Befragter), 
               data = dataS, family=multinomial(refLevel=1))
summary(model1)
