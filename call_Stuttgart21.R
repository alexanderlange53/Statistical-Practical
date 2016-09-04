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

bearbeiter <- 'Kai@Home'
loadGeo <- TRUE # Getrennt, damit die Geodaten geladen werden koennen ohne Prediction aufzurufen
pred = FALSE
load_model <- TRUE

if(bearbeiter == 'Alex') {
  setwd('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Presi/Statistical-Practical')
  sample <- read.table("/home/alex/Schreibtisch/Uni/statistisches_praktikum/Auswertung/Neue_Daten/Stuttgart21_aufbereitet.csv", header=TRUE, sep=";")
  bezirke <- readOGR(dsn = "/home/alex/Schreibtisch/Uni/statistisches_praktikum/Auswertung/Geodaten/bezirke", layer = "bezirke")
  stadtteile <- readOGR(dsn = "/home/alex/Schreibtisch/Uni/statistisches_praktikum/Daten_Kneib/Stick/Daten_Kneib/Stadtteile_netto", layer = "Stadtteile_netto")
  if(loadGeo){
    Umfrage <- read.csv2('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Daten_Kneib/Stick/buergerumfrage/population_aufbereitet_stadtteile.txt')
    Zensus <- read.csv2('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Daten_Kneib/Stick/zensus/population_aufbereitet_stadtteile.txt')
  }
} 
if(bearbeiter == 'Kai@Work') {
  setwd('/home/khusmann/mnt/U/Promotion/Kurse/Stat_Praktikum/Praesentation1_06062016/Statistical-Practical/')
  sample <- read.table("./Rohdaten/buergerumfrage_neu/Stuttgart21_aufbereitet_stadtteile.csv", header=TRUE, sep=";")
  bezirke <- readOGR(dsn = "./Rohdaten/Geodaten/bezirke/", layer = "bezirke")
  stadtteile <- readOGR(dsn = "./Rohdaten/Geodaten/Stadtteile_Shapefile/", layer = "Stadtteile_netto")
  if(loadGeo){
    Umfrage <- read.csv2('./Rohdaten/buergerumfrage/population_aufbereitet_stadtteile.txt')
    Zensus <- read.csv2('./Rohdaten/zensus/population_aufbereitet_stadtteile.txt')
  }
}
if(bearbeiter == 'Kai@Home') {
  setwd('/home/kai/Dokumente/Master/Stat_Practical/Statistical-Practical/')
  sample <- read.table("./Rohdaten/buergerumfrage_neu/Stuttgart21_aufbereitet_stadtteile.csv", header=TRUE, sep=";")
  bezirke <- readOGR(dsn = "/home/kai/Dokumente/Master/Stat_Practical/Statistical-Practical/Rohdaten/Geodaten/bezirke/", layer = "bezirke")
  stadtteile <- readOGR(dsn = "/home/kai/Dokumente/Master/Stat_Practical/Statistical-Practical/Rohdaten/Geodaten/Stadtteile_netto/", layer = "Stadtteile_netto")
  if(loadGeo){
    Umfrage <- read.csv2('/home/kai/Dokumente/Master/Stat_Practical/Statistical-Practical/Rohdaten/buergerumfrage/population_aufbereitet_stadtteile.txt', as.is = TRUE)
    Zensus <- read.csv2('/home/kai/Dokumente/Master/Stat_Practical/Statistical-Practical/Rohdaten/zensus/population_aufbereitet_stadtteile.txt', as.is = TRUE)
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
sample <- DataPrep(sample, binom = F)

#---------------------------------------------#
#### Kontinuierliche räumliche Information ####
#---------------------------------------------#

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
verteilung <- ocat(R=3)

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
parallel <- TRUE
ncore <- 8
seed <- 123

#--------------------#
## Modellerstellung ##
#--------------------#

## Step AIC ##
if(!load_model){
  step.model <- stepAIC()
  saveRDS(step.model, file="step.model_all.rds")
} else {
  step.model <- readRDS(file = "step.model_all.rds")
}

#--------------------------------#
## Modelleffekte interpretieren ##
#--------------------------------#
## GAM Plots ##
m1 <- step.model$model.spat
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


AIC(step.model$model.spat)
AIC(step.model$model.nospat)
AIC(step.model$model.spatonly)

#--------------------#
## Model Evaluation ##
#--------------------#

evaluate(step.model$model.spat, data = sample)
evaluateAll(step.model, data = sample)

## Cross Evaluation ##
repeatitions = 2
model <- step.model$model.spat

leave_out <- sample.int(n = dim(sample)[1], size = repeatitions)
crosseval <- data.frame(Observation.No = integer(), Observed.y = integer(), Predicted.y = integer())

for (i in c(1 : repeatitions)) {
  all <- c(1 : dim(sample)[1])
  subset_i <- all[-leave_out]
  print(paste('Model', i, 'of', repeatitions))
  gam_i <- gam(model$formula, family = model$family, method="REML", data = sample, weights = as.vector(sample[, "Gewicht"]), subset = as.vector(subset_i)) # Fit a GAM
  ret_i <- cbind(leave_out[i], sample$Meinung.zu.Stuttgart.21[leave_out[i]], apply(predict(model, newdata = sample[leave_out[i],], type = "response"), 1, which.max)) # Compare true and estiamted y.
  crosseval <- rbind(crosseval, ret_i)
}
names(crosseval) = c("Observation.No", "Observed.y", "Predicted.y")
rm(list = c("all", "subset_i", "gam_i", "ret_i"))
crosseval

#---------------#
## Prediction  ##
#---------------#

## Vorhersage der individuellen Ausprägung ##
if(pred == T){
  pred.U.k <- Prediction(Umfrage, step.model$model.spat, IFUmfrage = T, binom = F)
  pred.Z.k <- Prediction(Zensus, step.model$model.spat, IFUmfrage = F, binom = F)
  write.csv2(pred.U.k, file = 'pred_S21_U_kont.csv', row.names=FALSE, quote=FALSE)
  write.csv2(pred.Z.k, file = 'pred_S21_Z_kont.csv', row.names=FALSE, quote=FALSE)
}else{
  pred.U.k <- read.csv2('pred_S21_U_kont.csv')
  pred.Z.k <- read.csv2('pred_S21_Z_kont.csv')
}

## Aggregation auf Bezirksebene ##

AggPredU <- Prediction.Aggregation(pred = pred.U.k[, c(1 : 3, 6)], agg = 'Stadtteil')
AggPredZ <- Prediction.Aggregation(pred = pred.Z.k[, c(1 : 3, 6)], agg = 'Stadtteil')

PredBarPlot(sample, pred.U.k, Variable = 'Meinung zu Stuttgart 21', 
            x = c('Zustimmung', 'Neutral', 'Ablehnung'))
PredBarPlot(sample, pred.Z.k, Variable = 'Meinung zu Stuttgart 21',
            x = c('Zustimmung', 'Neutral', 'Ablehnung'))

## Konfidenzintervalle der Schätzung ##


#--------------------------------------------#
#### Bezirke als Räumliche Informationen #####                        -----------------------------------------------------------------
#--------------------------------------------#

# Erstellen des Markov-Random fields
zt <- MarkovRandomField(bezirke, Bezirke = T)

# Neue raeumliche Information, der Rest bleibt gleich
fixed <- "s(Stadtbezirk, bs=\"mrf\", xt = zt) + s(Personenzahl.im.Haushalt, Altersklasse.Befragter, bs= \"tp\")"


#--------------------#
## Modellerstellung ##
#--------------------#

## Step AIC ##
if(!load_model){
  step.model.B <- stepAIC()
  saveRDS(step.model.B, file="step.model_all_B.rds")
} else {
  step.model.B <- readRDS(file = "step.model_all_B.rds")
}


#--------------------#
## Model Evaluation ##
#--------------------#

evaluate(step.model.B$model.spat, data = sample)
evaluateAll(step.model.B, data = sample)

## Cross Evaluation ##
repeatitions = 2
model <- step.model.B$model.spat

leave_out <- sample.int(n = dim(sample)[1], size = repeatitions)
crosseval <- data.frame(Observation.No = integer(), Observed.y = integer(), Predicted.y = integer())

for (i in c(1 : repeatitions)) {
  all <- c(1 : dim(sample)[1])
  subset_i <- all[-leave_out]
  print(paste('Model', i, 'of', repeatitions))
  gam_i <- gam(model$formula, family = model$family, method="REML", data = sample, weights = as.vector(sample[, "Gewicht"]), subset = as.vector(subset_i)) # Fit a GAM
  ret_i <- cbind(leave_out[i], sample$Meinung.zu.Stuttgart.21[leave_out[i]], apply(predict(model, newdata = sample[leave_out[i],], type = "response"), 1, which.max)) # Compare true and estiamted y.
  crosseval <- rbind(crosseval, ret_i)
}
names(crosseval) = c("Observation.No", "Observed.y", "Predicted.y")
rm(list = c("all", "subset_i", "gam_i", "ret_i"))
crosseval


#--------------------------------#
## Modelleffekte interpretieren ##
#--------------------------------#
## GAM Plots ##
m1 <- step.model.B$model.spat
plot(m1, select = 1, all = TRUE, ylab = "GK Hochwert", xlab = "GK Rechtswert") # Cont. spat. effect
plot(m1, select = 3, all = TRUE, ylab = "s(Altersklasse)", xlab = "Altersklasse") # Alter

x11()
par(mfrow = c(2, 2))
plot(m1, select = 4, all = TRUE, ann = F) # Geschlecht
mtext(side = 1, line = 3, "Geschlecht"); mtext(side = 2, line = 3, "Einfluss des Geschlechts")
plot(m1, select = 5, all = TRUE, ann = F) # Nationalität
mtext(side = 1, line = 3, "Nationalität"); mtext(side = 2, line = 3, "Einfluss der Nationalität")
#plot(m1, select = 6, all = TRUE, ann = F) # Familienstand
#mtext(side = 1, line = 3, "Familienstand"); mtext(side = 2, line = 3, "Einfluss des Familienstands")
plot(m1, select = 7, all = TRUE, ann = F) # Personenzahl
mtext(side = 1, line = 3, "Personenzahl im Haushalt"); mtext(side = 2, line = 3, "Einfluss der Personenzahl im Haushalt")

dev.off()

AIC(step.model.B$model.spat)
AIC(step.model.B$model.nospat)
AIC(step.model.B$model.spatonly)

summary(step.model.B$model.spat)
#plot(step.model.B$model.spat, all = T)


#---------------#
## Prediction  ##
#---------------#

## Vorhersage der individuellen Ausprägung ##
if(pred){
  pred.U.B <- Prediction(Umfrage, step.model.B$model.spat, IFUmfrage = T, binom = F)
  pred.Z.B <- Prediction(Zensus, step.model.B$model.spat, IFUmfrage = F, binom = F)
  write.csv2(pred.U.B, file = 'pred_S21_U_bezirk.csv', row.names=FALSE, quote=FALSE)
  write.csv2(pred.Z.B, file = 'pred_S21_Z_bezirk.csv', row.names=FALSE, quote=FALSE)
}else{
  pred.U.B <- read.csv2('pred_S21_U_bezirk.csv')
  pred.Z.B <- read.csv2('pred_S21_Z_bezirk.csv')
}

## Aggregation auf Bezirksebene ##

Prediction.Aggregation(pred = pred.U.B[, c(1 : 3, 7)], agg = "Stadtbezirk")

PredBarPlot(sample, pred.U.B, x = c('Zustimmung', 'Neutral', 'Ablehnung'))
PredBarPlot(sample, pred.Z.B, x = c('Zustimmung', 'Neutral', 'Ablehnung'))



#-----------------------------------------------#
#### Stadtteile als Räumliche Informationen #####                        -----------------------------------------------------------------
#-----------------------------------------------#

# Erstellen des Markov Random fields
zt <- MarkovRandomField(stadtteile, Bezirke = F)

# Erstellen der Pseudo Beobachtungen und in Datensatz integrieren
sample <- PseudoB(sample, stadtteile, binom = F)

# Neue raeumliche Information, der rest bleibt gleich
fixed <- "s(Stadtteil, bs=\"mrf\", xt = zt) + s(Personenzahl.im.Haushalt, Altersklasse.Befragter, bs= \"tp\")"

#--------------------#
## Modellerstellung ##
#--------------------#


## Step AIC ##
if(!load_model){
  step.model.S <- stepAIC()
  saveRDS(step.model.S, file="step.model_all_S.rds")
} else {
  step.model.S <- readRDS(file = "step.model_all_S.rds")
}

#--------------------#
## Model Evaluation ##
#--------------------#

evaluate(step.model.S$model.spat, data = sample)
evaluateAll(step.model.S, data = sample)


## Cross Evaluation ##
repeatitions = 1
model <- step.model.S$model.spat

leave_out <- sample.int(n = dim(sample)[1], size = repeatitions)
crosseval <- data.frame(Observation.No = integer(), Observed.y = integer(), Predicted.y = integer())

for (i in c(1 : repeatitions)) {
  all <- c(1 : dim(sample)[1])
  subset_i <- all[-leave_out]
  print(paste('Model', i, 'of', repeatitions))
  gam_i <- gam(model$formula, family = model$family, method="REML", data = sample, weights = as.vector(sample[, "Gewicht"]), subset = as.vector(subset_i)) # Fit a GAM
  ret_i <- cbind(leave_out[i], sample$Meinung.zu.Stuttgart.21[leave_out[i]], apply(predict(model, newdata = sample[leave_out[i],], type = "response"), 1, which.max)) # Compare true and estiamted y.
  crosseval <- rbind(crosseval, ret_i)
}
names(crosseval) = c("Observation.No", "Observed.y", "Predicted.y")
rm(list = c("all", "subset_i", "gam_i", "ret_i"))
crosseval


#--------------------------------#
## Modelleffekte interpretieren ##
#--------------------------------#
## GAM Plots ##
m1 <- step.model.S$model.spat
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

AIC(step.model.S$model.spat)
AIC(step.model.S$model.nospat)
AIC(step.model.S$model.spatonly)

summary(step.model.S$model.spat)
# plot(step.model.S$model.spat, all = T)

#---------------#
## Prediction  ##
#---------------#

## Vorhersage der individuellen Ausprägung ##
if(pred){
  pred.U.S <- Prediction(Umfrage, step.model.S$model.spat, IFUmfrage = T, binom = F)
  pred.Z.S <- Prediction(Zensus, step.model.S$model.spat, IFUmfrage = F, binom = F)
  write.csv2(pred.U.S, file = 'pred_S21_U_stadtteil.csv', row.names=FALSE, quote=FALSE)
  write.csv2(pred.Z.S, file = 'pred_S21_Z_stadtteil.csv', row.names=FALSE, quote=FALSE)
}else{
  pred.U.S <- read.csv2('pred_S21_U_bezirk.csv')
  pred.Z.S <- read.csv2('pred_S21_Z_bezirk.csv')
}

## Aggregation auf Bezirksebene ##

Prediction.Aggregation(pred = pred.U.S[, c(1 : 3, 6)], agg = "Stadtteil")

PredBarPlot(sample, pred.U.S, x = c('Zustimmung', 'Neutral', 'Ablehnung'))
PredBarPlot(sample, pred.Z.S, x = c('Zustimmung', 'Neutral', 'Ablehnung'))


