#---------------------------------------------------------------#
#### Erster Versuch unterschiedliche gam zu parameterisieren ####
#---------------------------------------------------------------#

# Zuletzt bearbeitet: 17.06.2016
# von: Kai

#---------------------#
## Datenvorbereitung ##
#---------------------#

# Workspace loeschen
rm(list=ls())

## Pakete laden und Arbeitsordner setzen ##
bearbeiter = 'Alex'
if(bearbeiter == 'Alex') {
  setwd('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Presi/Statistical-Practical')
} else {
  setwd('/home/khusmann/mnt/U/Promotion/Kurse/Stat_Praktikum/Praesentation1_06062016/Statistical-Practical/')
}

library("ROCR")
library("mgcv")
library("splines")

# Hilfsfunktionen
source('./erster_test_functions.R')

## Einlesen ##
if(bearbeiter == 'Alex') {
  zensus_sample <- read.table("/home/alex/Schreibtisch/Uni/statistisches_praktikum/Auswertung/Rohdaten/zensus/sample_aufbereitet.txt", header=TRUE, sep=";")
} else {
  zensus_sample <- read.table("/home/khusmann/mnt/U/Promotion/Kurse/Stat_Praktikum/Auswertung/Rohdaten/zensus/sample_aufbereitet.txt", header=TRUE, sep=";")
}

#-------------------#
## Datenauswertung ##
#-------------------#

#### Beispiel 1: Nachbildung Kneib Modell Zensus ####

## gam parameter ##
# Zielgroesse & Verteilungsannahme
response <- "beamte"
verteilung <- binomial()

# Gewichte
gewichte <- "gewicht"

# raeumlicher Effekt
seff <- "s(xcoord, ycoord, bs=\"tp\")"

# Parametrisch zu modellierende Kovariablen
pars <- c("familienstand", "nutzung", "gebaeudetyp", "staatsangehoerigkeit", "geschlecht")

# Potenziell nichtparametrisch zu modellierende Kovariablen
nonpars <- c("alter","wohnflaeche","haushaltsgroesse")

formel <- make.formula(response = response, fixed = seff, pars = pars, nonpars = nonpars)

## gam Aufruf ##
gam(formel , weights = zensus_sample[, gewichte], family = verteilung, link = logit, method = "REML", data = zensus_sample)

## gam Analyse ##



#### Beispiel 2: Geordneter Response Zensus ####
response <- "alter"
verteilung <- ocat(R = 9)

# Gewichte
gewichte <- "gewicht"

# raeumlicher Effekt
seff <- "s(xcoord, ycoord, bs=\"tp\")"

# Parametrisch zu modellierende Kovariablen
pars <- c("familienstand", "nutzung", "gebaeudetyp", "staatsangehoerigkeit", "geschlecht")

# Potenziell nichtparametrisch zu modellierende Kovariablen
nonpars <- c("wohnflaeche","haushaltsgroesse")

formel <- make.formula(response = response, fixed = seff, pars = pars, nonpars = nonpars)

## gam Aufruf ##
t2 <- gam(formel , weights = zensus_sample[, gewichte], family = verteilung, method = "REML", data = zensus_sample)
# Mit Warnung: Aber Kneib meinte die kommt immer, weiss nicht warum. Ma nachfragen. Evtl weiss Matthias das auch

## gam Analyse ##
t2
plot(t2, pages = 1)
gam.check(t2) # Sieht nicht so gut aus. Ist ja aber auch erst der erste Test
summary(t2)
t2$family$getTheta(TRUE)



#### Beispiel 3: Wie 2 mit weniger Kovar. (nur signifikanten) ####
response <- "alter"
verteilung <- ocat(R = 9)

# Gewichte
gewichte <- "gewicht"

# raeumlicher Effekt
seff <- "s(xcoord, ycoord, bs=\"tp\")"

# Parametrisch zu modellierende Kovariablen
pars <- c("familienstand")

# Potenziell nichtparametrisch zu modellierende Kovariablen
nonpars <- c("wohnflaeche")

formel <- make.formula(response = response, fixed = seff, pars = pars, nonpars = nonpars)

## gam Aufruf ##
t3 <- gam(formel , weights = zensus_sample[, gewichte], family = verteilung, method = "REML", data = zensus_sample)
# Diesmal sogar ohne Warnung

## gam Analyse ##
t3
plot(t3, pages = 1)
gam.check(t3) # Naja, s. o.
summary(t3)
t3$family$getTheta(TRUE) # Grenzen = Threshold
predict(t3, zensus_sample[,c(2,10,11,3,6)])
# So wie ich das sehe muss man jetzt gucken, wann diese Grenzen ueberschritten werden, um ueber die predicts auf die Altersklasse zu kommen. 
# DAfuer hab ich jetzt aber keine Zeit mehr. Muss los.

