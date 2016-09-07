#------------------------------------------------#
#### Laden der Daten und Aufrufen der Skripte ####
#------------------------------------------------#

rm(list = ls())
library("ROCR")
library("mgcv")
library("splines")
library(MASS)
require(rgdal);require(rgeos)
require(ggplot2)
require(maptools);require(rvest);require(dplyr)
library(ggplot2)
library(reshape2)
## Working directory ##

bearbeiter <- 'Alex'
pred = T

if(bearbeiter == 'Alex') {
  setwd('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Presi/Statistical-Practical')
  sample <- read.table("/home/alex/Schreibtisch/Uni/statistisches_praktikum/Auswertung/Neue_Daten/Stuttgart21_aufbereitet.csv", header=TRUE, sep=";")
  bezirke <- readOGR(dsn = "/home/alex/Schreibtisch/Uni/statistisches_praktikum/Auswertung/Geodaten/bezirke", layer = "bezirke")
  stadtteile <- readOGR(dsn = "/home/alex/Schreibtisch/Uni/statistisches_praktikum/Daten_Kneib/Stick/Daten_Kneib/Stadtteile_netto", layer = "Stadtteile_netto")
  if(pred == T){
    Umfrage <- read.csv2('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Daten_Kneib/Stick/buergerumfrage/population_aufbereitet_stadtteile.txt')
    Zensus <- read.csv2('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Daten_Kneib/Stick/zensus/population_aufbereitet_stadtteile.txt')
  }
} 
if(bearbeiter == 'Kai@Work') {
  setwd('/home/khusmann/mnt/U/Promotion/Kurse/Stat_Praktikum/Praesentation1_06062016/Statistical-Practical/')
  sample <- read.table("./Rohdaten/buergerumfrage_neu/Stuttgart21_aufbereitet.csv", header=TRUE, sep=";")}
if(bearbeiter == 'Kai@Home') {
  setwd('/home/kai/Dokumente/Master/Stat_Practical/Statistical-Practical/')
  sample <- read.table("./Rohdaten/buergerumfrage_neu/Stuttgart21_aufbereitet.csv", header=TRUE, sep=";")
  bezirke <- readOGR(dsn = "/home/kai/Dokumente/Master/Stat_Practical/Statistical-Practical/Rohdaten/Geodaten/bezirke/", layer = "bezirke")
  stadtteile <- readOGR(dsn = "/home/kai/Dokumente/Master/Stat_Practical/Statistical-Practical/Rohdaten/Geodaten/Stadtteile_netto/", layer = "Stadtteile_netto")
  if(pred == T){
    Umfrage <- read.csv2('/home/kai/Dokumente/Master/Stat_Practical/Statistical-Practical/Rohdaten/buergerumfrage/population_aufbereitet.txt')
    Zensus <- read.csv2('/home/kai/Dokumente/Master/Stat_Practical/Statistical-Practical/Rohdaten/zensus/population_aufbereitet.txt')
  }
}

source("stepAIC.R")
source("evaluation.R")
source('DataPrep.R')
source('MarkovRandomField.R')
source('PseudoB.R')
source("prediction_function.R")
source('PredBarPlot.R')

#--------------------------------#
# Daten einlesen und vorbereiten #
#--------------------------------#

# Wenn binom = F:
# erstellt aus Gruppen 1 und 2 = 1
# erstellt aus Gruppen 3 = 2
# erstellt aus Grppen 4 und 5 = 3
# löscht Gruppe 6
#sample <- DataPrep(sample, binom = F)

for(i in 1:nrow(sample)){
  if(sample$Bewertung.Wohngegend[i] == 6){
    sample$Bewertung.Wohngegend[i] <- NA
  }
}
sample <- na.omit(sample)

#------------------#
# Eingabeparameter #
#------------------#

# Schätzdatensatz (Stichprobe)
# reine Textdatei mit
# - Variablennamen in der ersten Zeile
# - Semikolon-getrenten Spalten
# - Kategoriale Variablen mit passenden Labeln versehen (als Text)
# - "." als Dezimaltrennzeichen

# Zielgröße & Verteilungsannahme
response <- "Bewertung.Wohngegend"
verteilung <- ocat(R=5)

# Gewichte
sample$Gewicht <- 1
gewichte <- "Gewicht"

# Feste Modellbestandteile, die nicht in die Variablenselektion mit aufgenommen
# werden sollen (typischerweise der r?umliche Effekt)
fixed <- "s(X, Y, bs=\"tp\") + s(Personenzahl.im.Haushalt, Altersklasse.Befragter, bs= \"tp\")"

# Parametrisch zu modellierende Kovariablen
pars <- c("Familienstand", "Nationalität", "Geschlecht")

# Potenziell nichtparametrisch zu modellierende Kovariablen
nonpars <- c("Altersklasse.Befragter","Personenzahl.im.Haushalt")

# Modellwahl ja/nein?
modellwahl <- TRUE

# Vorhersageintervalle ja/nein und Eigenschaften
# nboot = Anzahl Bootstrap Stichproben
# coverage = ?berdeckungswahrscheinlichkeit der Vorhersageintervalle
# parallel = Soll parallel mit mehreren Kernen gerechnet werden?
#            dazu wird das Paket multicore ben?tigt (nur unter Linux)
# ncore = Anzahl der zu verwendenden Kerne
# seed = Startwert f?r den Zufallszahlengenerator
intervalle <- TRUE
nboot <- 10
coverage <- 0.95
parallel <- FALSE
ncore <- 20
seed <- 123

#--------------------#
## Modellerstellung ##
#--------------------#

load_model <- F
## Step AIC ##
if(!load_model){
  step.model.Bewertung.5 <- stepAIC()
  saveRDS(step.model.Bewertung.5, file="step.mode.Bewertungl.5_all.rds")
} else {
  step.model.Bewertung.5 <- readRDS(file = "step.mode.Bewertungl.5_all.rds")
}

#--------------------------------#
## Modelleffekte interpretieren ##
#--------------------------------#
## GAM Plots ##
m1 <- step.model.Bewertung.5$model.spat
plot(m1, select = 1, all = TRUE, ylab = "GK Hochwert", xlab = "GK Rechtswert") # Cont. spat. effect
plot(m1, select = 3, all = TRUE, ylab = "s(Altersklasse)", xlab = "Altersklasse") # Alter

x11()
par(mfrow = c(2, 2))
plot(m1, select = 4, all = TRUE, ann = F) # Geschlecht
mtext(side = 1, line = 3, "Geschlecht"); mtext(side = 2, line = 3, "Einfluss des Geschlechts")
plot(m1, select = 5, all = TRUE, ann = F) # Nationalität
mtext(side = 1, line = 3, "Nationalität"); mtext(side = 2, line = 3, "Einfluss der Nationalität")
plot(m1, select = 6, all = TRUE, ann = F) # Familienstand
mtext(side = 1, line = 3, "Familienstand"); mtext(side = 2, line = 3, "Einfluss des Familienstands")
plot(m1, select = 7, all = TRUE, ann = F) # Personenzahl
mtext(side = 1, line = 3, "Personenzahl im Haushalt"); mtext(side = 2, line = 3, "Einfluss der Personenzahl im Haushalt")
dev.off()

AIC(step.model.Bewertung.5$model.spat)
AIC(step.model.Bewertung.5$model.nospat)
AIC(step.model.Bewertung.5$model.spatonly)

#--------------------#
## Model Evaluation ##
#--------------------#
evaluate(step.model.Bewertung.5$model.spat, data = sample)
evaluateAll(step.model.Bewertung.5, data = sample)

# Modell mit 3 Klassen ----------------------------------------------------------------------------

sample <- DataPrep(sample, binom = F, Stuttgart21 = F)

#------------------#
# Eingabeparameter #
#------------------#

# Schätzdatensatz (Stichprobe)
# reine Textdatei mit
# - Variablennamen in der ersten Zeile
# - Semikolon-getrenten Spalten
# - Kategoriale Variablen mit passenden Labeln versehen (als Text)
# - "." als Dezimaltrennzeichen

# Zielgröße & Verteilungsannahme
response <- "Bewertung.Wohngegend"
verteilung <- ocat(R=3)

#--------------------#
## Modellerstellung ##
#--------------------#

load_model <- T
## Step AIC ##
if(!load_model){
  step.model.Bewertung.3 <- stepAIC()
  saveRDS(step.model.Bewertung.3, file="step.model.Bewertungl.3_all.rds")
} else {
  step.model.Bewertung.3 <- readRDS(file = "step.model.Bewertungl.3_all.rds")
}

#--------------------------------#
## Modelleffekte interpretieren ##
#--------------------------------#
## GAM Plots ##
m1 <- step.model.Bewertung.3$model.spat
plot(m1, select = 1, all = TRUE, ylab = "GK Hochwert", xlab = "GK Rechtswert") # Cont. spat. effect
plot(m1, select = 3, all = TRUE, ylab = "s(Altersklasse)", xlab = "Altersklasse") # Alter

x11()
par(mfrow = c(2, 2))
plot(m1, select = 4, all = TRUE, ann = F) # Geschlecht
mtext(side = 1, line = 3, "Geschlecht"); mtext(side = 2, line = 3, "Einfluss des Geschlechts")
plot(m1, select = 5, all = TRUE, ann = F) # Nationalität
mtext(side = 1, line = 3, "Nationalität"); mtext(side = 2, line = 3, "Einfluss der Nationalität")
plot(m1, select = 6, all = TRUE, ann = F) # Familienstand
mtext(side = 1, line = 3, "Familienstand"); mtext(side = 2, line = 3, "Einfluss des Familienstands")
plot(m1, select = 7, all = TRUE, ann = F) # Personenzahl
mtext(side = 1, line = 3, "Personenzahl im Haushalt"); mtext(side = 2, line = 3, "Einfluss der Personenzahl im Haushalt")
dev.off()

AIC(step.model.Bewertung.3$model.spat)
AIC(step.model.Bewertung.3$model.nospat)
AIC(step.model.Bewertung.3$model.spatonly)

#--------------------#
## Model Evaluation ##
#--------------------#
evaluate(step.model.Bewertung.3$model.nospat, data = sample)
evaluateAll(step.model.Bewertung.3, data = sample)
