#------------------------------------------------#
#### Laden der Daten und Aufrufen der Skripte ####
#------------------------------------------------#

require(rgdal);require(rgeos)
require(ggplot2)
require(maptools);require(rvest);require(dplyr)

rm(list = ls())

## Working directory ##

bearbeiter = 'Alex'

if(bearbeiter == 'Alex') {
  setwd('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Presi/Statistical-Practical')
  sample <- read.table("/home/alex/Schreibtisch/Uni/statistisches_praktikum/Auswertung/Neue_Daten/Stuttgart21_aufbereitet.csv", header=TRUE, sep=";")
  bezirke <- readOGR(dsn = "/home/alex/Schreibtisch/Uni/statistisches_praktikum/Auswertung/Geodaten/bezirke", layer = "bezirke")
  stadtteile <- readOGR(dsn = "/home/alex/Schreibtisch/Uni/statistisches_praktikum/Daten_Kneib/Stick/Daten_Kneib/Stadtteile_netto", layer = "Stadtteile_netto")
} 
if(bearbeiter == 'Kai@Work') {
  setwd('/home/khusmann/mnt/U/Promotion/Kurse/Stat_Praktikum/Praesentation1_06062016/Statistical-Practical/')
  sample <- read.table("./Rohdaten/buergerumfrage_neu/Stuttgart21_aufbereitet.csv", header=TRUE, sep=";")}
if(bearbeiter == 'Kai@Home') {
  setwd('/home/kai/Dokumente/Master/Stat_Practical/Statistical-Practical/')
  sample <- read.table("./Rohdaten/buergerumfrage_neu/Stuttgart21_aufbereitet.csv", header=TRUE, sep=";")
}

source("stepAIC.R")
source("evaluation.R")
source('DataPrep.R')
source('MarkovRandomField.R')
source('PseudoB.R')
#source("prediction.R")

library("ROCR")
library("mgcv")
library("splines")

#--------------------------------#
# Daten einlesen und vorbereiten #
#--------------------------------#

# Wenn binom = T:
# Fasst die Gruppen 1 und 2 zu == 1 zusammen und
# 4 und 5 == 0
# Gruppen 6 und 3 werden gelöscht
sample <- DataPrep(sample, binom = T)

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
response <- "Meinung.zu.Stuttgart.21"
verteilung <- binomial()

# Gewichte
sample$Gewicht <- 1
gewichte <- "Gewicht"

# Feste Modellbestandteile, die nicht in die Variablenselektion mit aufgenommen
# werden sollen (typischerweise der raeumliche Effekt)
fixed <- "s(X, Y, bs=\"tp\") + s(Personenzahl.im.Haushalt, Altersklasse.Befragter, bs= \"tp\")"

# Parametrisch zu modellierende Kovariablen
pars <- c("Familienstand", "Nationalität", "Geschlecht")

# Potenziell nichtparametrisch zu modellierende Kovariablen
nonpars <- c("Altersklasse.Befragter","Personenzahl.im.Haushalt")

# Modellwahl ja/nein?
modellwahl <- TRUE

# Vorhersageintervalle ja/nein und Eigenschaften
# nboot = Anzahl Bootstrap Stichproben
# coverage = Ueberdeckungswahrscheinlichkeit der Vorhersageintervalle
# parallel = Soll parallel mit mehreren Kernen gerechnet werden?
#            dazu wird das Paket multicore benoetigt (nur unter Linux)
# ncore = Anzahl der zu verwendenden Kerne
# seed = Startwert fuer den Zufallszahlengenerator
intervalle <- TRUE
nboot <- 10
coverage <- 0.95
parallel <- FALSE
ncore <- 20
seed <- 123

#--------------------#
## Modellerstellung ##
#--------------------#

load_model <- T
## Step AIC ##
if(!load_model){
  step.model.binom <- stepAIC()
  saveRDS(step.model.binom$model.spat, file="step.model_binom.rds")
  saveRDS(step.model.binom, file="step.model_all_binom.rds")
} else {
  step.model.binom <- readRDS(file = "step.model_all_binom.rds")
}

#--------------------------------#
## Modelleffekte interpretieren ##
#--------------------------------#
## GAM Plots ##
m1 <- step.model.binom$model.spat
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


AIC(step.model.binom$model.spat)
AIC(step.model.binom$model.nospat)
AIC(step.model.binom$model.spatonly)

summary(step.model.binom$model.spat)
#plot(step.model.binom$model.spat, all = T)

evaluate(step.model.binom$model.spat, data = sample)

#--------------------------------------#
# Bezirke als Räumliche Informationen  #-----------------------------------------------------------------
#--------------------------------------#

# Erstellen des Markov-Random fields
zt <- MarkovRandomField(bezirke, Bezirke = T)

# Neue raeumliche Information, der rest bleibt gleich
fixed <- "s(Stadtbezirk, bs=\"mrf\", xt = zt) + s(Personenzahl.im.Haushalt, Altersklasse.Befragter, bs= \"tp\")"

#--------------------#
## Modellerstellung ##
#--------------------#

load_model <- TRUE
## Step AIC ##
if(!load_model){
  step.model.binom.B <- stepAIC()
  saveRDS(step.model.binom.B$model.spat, file="step.model_binomB.rds")
  saveRDS(step.model.binom.B, file="step.model_all_binomB.rds")
} else {
  step.model.binom.B <- readRDS(file = "step.model_all_binomB.rds")
}

#--------------------------------#
## Modelleffekte interpretieren ##
#--------------------------------#
## GAM Plots ##
m1 <- step.model.binom.B$model.spat
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

AIC(step.model.binom.B$model.spat)
AIC(step.model.binom.B$model.nospat)
AIC(step.model.binom.B$model.spatonly)

summary(step.model.binom.B$model.spat)
plot(step.model.binom.B$model.spat, all = T)

#-----------------------------------------#
# Stadtteile als Räumliche Informationen  #--------------------------------------------------------------
#-----------------------------------------#

# Erstellen des Markov Random fields
zt <- MarkovRandomField(stadtteile, Bezirke = F)

# Erstellen der Pseudo Beobachtungen und in Datensatz integrieren
sample <- PseudoB(sample, stadtteile, binom = T)

# Neue raeumliche Information, der rest bleibt gleich
fixed <- "s(Stadtteil, bs=\"mrf\", xt = zt) + s(Personenzahl.im.Haushalt, Altersklasse.Befragter, bs= \"tp\")"

#--------------------#
## Modellerstellung ##
#--------------------#

load_model <- TRUE
## Step AIC ##
if(!load_model){
  step.model.binom.S <- stepAIC()
  saveRDS(step.model.binom.S$model.spat, file="step.model_binomS.rds")
  saveRDS(step.model.binom.S, file="step.model_all_binomS.rds")
} else {
  step.model.binom.S <- readRDS(file = "step.model_all_binomS.rds")
}
#--------------------------------#
## Modelleffekte interpretieren ##
#--------------------------------#
## GAM Plots ##
m1 <- step.model.binom.S$model.spat
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

AIC(step.model.binom.S$model.spat)
AIC(step.model.binom.S$model.nospat)
AIC(step.model.binom.S$model.spatonly)

summary(step.model.binom.S$model.spat)
plot(step.model.binom.S$model.spat, all = T)

