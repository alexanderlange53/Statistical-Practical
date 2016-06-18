#####################################
# Estimation of Stuttgart 21 survey #
#####################################

rm(list=ls())
# loading packages
require('ROCR')
require('mgcv')
require('splines')
require(rgdal);require(rgeos)

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
# Hilfsfunktionen
source('./erster_test_functions.R')

# Da die Kategorie 'Keine Angabe' nicht in das Schema der geordneten Kategorien passt und keine Informationen
# enthält wirde sie entfernt.
for(i in 1:nrow(dataS)){
if(dataS$Meinung.zu.Stuttgart.21[i] == 6){
  dataS$Meinung.zu.Stuttgart.21[i] <- NA
}}
dataS <- na.omit(dataS)

## gam parameter ##
# Zielgroesse & Verteilungsannahme
response <- "Meinung.zu.Stuttgart.21"
verteilung <- ocat(R=5)

# raeumlicher Effekt
seff <- "s(X, Y, bs=\"tp\")"

# Parametrisch zu modellierende Kovariablen
pars <- c("Familienstand", "Nationalität", "Geschlecht")

# Potenziell nichtparametrisch zu modellierende Kovariablen
nonpars <- c("Altersklasse.Befragter","Personenzahl.im.Haushalt","Monatliches.Netto.Haushaltseinkommen")

# Erstellen der Schätzfunktion
formel <- make.formula(response = response, fixed = seff, pars = pars, nonpars = nonpars)

# GAM Schätzung
model1 <- gam(formel, data = dataS, family= verteilung, method = 'REML')
summary(model1)
plot(model1, pages = 1)
gam.check(model1)
model1$family$getTheta(TRUE)
