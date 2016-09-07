#------------------------------------------------#
#### Laden der Daten und Aufrufen der Skripte ####
#------------------------------------------------#

rm(list = ls())

## Pakete und Funktionen laden ##
library("ROCR")
library("mgcv")
library("splines")
library(MASS)
require(rgdal);require(rgeos)
require(ggplot2)
require(maptools);require(rvest);require(dplyr)
library(ggplot2)
library(reshape2)

## Einstellungen ##

bearbeiter <- 'Kai@Home'
loadGeo <- TRUE # Geodaten laden?
calculate_model <- TRUE # Modelle erstellen und als RDS speichern? Oder als RDS laden
pred = TRUE # Vorhersage berechnen und als CSV speichern? Oder CSV laden
calc_CI <- TRUE # Konfidenzintervalle berechnen und als CSV speichern? Dauert sehr lange, je nach Bootstrap-Wiederholungen bis zu mehreren Stunden!!

## Laden der Daten ##
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
    Umfrage <- read.csv2('./Rohdaten/buergerumfrage/population_aufbereitet_stadtteile.txt', as.is = TRUE)
    Zensus <- read.csv2('./Rohdaten/zensus/population_aufbereitet_stadtteile.txt', as.is = TRUE)
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
if(bearbeiter == 'Cluster') {
  cat('Auf dem Cluster gibt es keinen GIT Ordner. Die Dateien müssen manuell aktualisiert werden. Es sollte keine Datei verändert werden.')
  setwd('/home/khusmann/Statistical-Practical/')
  sample <- read.table("./Rohdaten/buergerumfrage_neu/Stuttgart21_aufbereitet_stadtteile.csv", header=TRUE, sep=";")
  Umfrage <- read.csv2('./Rohdaten/buergerumfrage/population_aufbereitet_stadtteile.txt', as.is = TRUE)
  Zensus <- read.csv2('./Rohdaten/zensus/population_aufbereitet_stadtteile.txt', as.is = TRUE)
}

source("stepAIC.R")
source("evaluation.R")
source('DataPrep.R')
source('MarkovRandomField.R')
source('PseudoB.R')
source("prediction_function.R")
source('PredBarPlot.R')
source('validation.R')

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
fixed <- "s(X, Y, bs=\"tp\")" # Bei diesen Modell ist die WW s(Personenzahl.im.Haushalt,Altersklasse.Befragter) nicht signifikant 

# Parametrisch zu modellierende Kovariablen
pars <- c("Familienstand", "Nationalität", "Geschlecht")

# Potenziell nichtparametrisch zu modellierende Kovariablen
nonpars <- c("Altersklasse.Befragter", "Personenzahl.im.Haushalt")

# Modellwahl ja/nein?
modellwahl <- TRUE

#--------------------#
## Modellerstellung ##
#--------------------#

## Step AIC ##
if(calculate_model) {
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

## Cross Evaluation ##
repeatitions = 2
model <- step.model.Bewertung.5$model.spat

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
rm(list = c("all", "subset_i", "gam_i", "ret_i", "repeatitions", "model"))
crosseval

#---------------#
## Prediction  ##
#---------------#

if(pred) {
  ## Vorhersage der individuellen Ausprägung ##
  pred.U.k <- Prediction(Umfrage, step.model.Bewertung.5$model.spat, IFUmfrage = T, binom = F)
  pred.Z.k <- Prediction(Zensus, step.model.Bewertung.5$model.spat, IFUmfrage = F, binom = F)
  write.csv2(pred.U.k, file = './Prediction_Results/W_5_U_Ko_Einzel.csv', row.names=FALSE, quote=FALSE)
  write.csv2(pred.Z.k, file = './Prediction_Results/W_5_Z_Ko_Einzel.csv', row.names=FALSE, quote=FALSE)
  
  ## Aggregation = Räumliche Extrapolation ##
  AggPred.U.ST <- Prediction.Aggregation(pred = pred.U.k[, c(1 : 5, 8)], agg = 'Stadtteil')
  AggPred.Z.ST <- Prediction.Aggregation(pred = pred.Z.k[, c(1 : 5, 8)], agg = 'Stadtteil')
  AggPred.U.SB <- Prediction.Aggregation(pred = pred.U.k[, c(1 : 5, 9)], agg = 'Stadtbezirk')
  AggPred.Z.SB <- Prediction.Aggregation(pred = pred.Z.k[, c(1 : 5, 9)], agg = 'Stadtbezirk')
  write.csv2(AggPred.U.ST, file = './Prediction_Results/W_5_U_Ko_AggST.csv', row.names = FALSE, quote = FALSE)
  write.csv2(AggPred.Z.ST, file = './Prediction_Results/W_5_Z_Ko_AggST.csv', row.names = FALSE, quote = FALSE)
  write.csv2(AggPred.U.SB, file = './Prediction_Results/W_5_U_Ko_AggSB.csv', row.names = FALSE, quote = FALSE)
  write.csv2(AggPred.Z.SB, file = './Prediction_Results/W_5_Z_Ko_AggSB.csv', row.names = FALSE, quote = FALSE)
  
} else {
  pred.U.k <- read.csv2('./Prediction_Results/S21_3_U_Ko_Einzel.csv', as.is = TRUE)
  pred.Z.k <- read.csv2('./Prediction_Results/S21_3_Z_Ko_Einzel.csv', as.is = TRUE)
  
  AggPred.U.ST <- read.csv2('./Prediction_Results/S21_3_U_Ko_AggST.csv', as.is = TRUE)
  AggPred.Z.ST <- read.csv2('./Prediction_Results/S21_3_Z_Ko_AggST.csv', as.is = TRUE)
  AggPred.U.SB <- read.csv2('./Prediction_Results/S21_3_U_Ko_AggSB.csv', as.is = TRUE)
  AggPred.Z.SB <- read.csv2('./Prediction_Results/S21_3_Z_Ko_AggSB.csv', as.is = TRUE)
}


# Muss noch an K = 5 angepasst werden
PredBarPlot(sample, pred.U.k, Variable = 'Meinung zu Stuttgart 21', 
            x = c('Zustimmung', 'Neutral', 'Ablehnung'))
PredBarPlot(sample, pred.Z.k, Variable = 'Meinung zu Stuttgart 21',
            x = c('Zustimmung', 'Neutral', 'Ablehnung'))

## Konfidenzintervalle ##
if(calc_CI) {
  ## Allg. Einstellungen
  model <- step.model.Bewertung.5$model.spat
  sample <- sample
  ncores <- 8
  nboot <- 4
  coverage <- 0.95
  seed <- 123
  
  ## Konfidenzintervalle: Umfrage, Stadtteile ##
  population <- Umfrage
  aggregation <- "Stadtteil"
  pred.sum <- AggPred.U.ST
  IFUmfrage <- TRUE
  source('./prediction_interval.R')
  UInt.U.ST <- pred.interval$u_intervall
  OInt.U.ST <- pred.interval$o_intervall
  temp_mean <- pred.interval$mean
  temp_median <- pred.interval$median
  write.csv2(cbind(UInt.U.ST, OInt.U.ST[, c(2 : 4)], temp_mean[, c(2 : 4)], temp_median[, c(2 : 4)]), file = './Prediction_Results/W_5_U_Ko_IntST.csv', row.names = FALSE)
  W.5.U.Ko.IntST <- cbind(UInt.U.ST, OInt.U.ST[, c(2 : 4)], temp_mean[, c(2 : 4)], temp_median[, c(2 : 4)])
  rm(list = c('UInt.U.ST', 'OInt.U.ST', 'temp_mean', 'temp_median'))
  
  ## Konfidenzintervalle: Umfrage, Stadtbezirke ##
  pred.sum <- AggPred.U.SB
  aggregation <- "Stadtbezirk"
  
  source('./prediction_interval.R')
  UInt.U.SB <- pred.interval$u_intervall
  OInt.U.SB <- pred.interval$o_intervall
  temp_mean <- pred.interval$mean
  temp_median <- pred.interval$median
  write.csv2(cbind(UInt.U.SB, OInt.U.SB[, c(2 : 4)], temp_mean[, c(2 : 4)], temp_median[, c(2 : 4)]), file = './Prediction_Results/W_5_U_Ko_IntSB.csv', row.names = FALSE)
  W.5.U.Ko.IntSB <- cbind(UInt.U.SB, OInt.U.SB[, c(2 : 4)], temp_mean[, c(2 : 4)], temp_median[, c(2 : 4)])
  
  ## Konfidenzintervalle: Zensus, Stadtteile
  population <- Zensus
  aggregation <- "Stadtteil"
  pred.sum <- AggPred.Z.ST
  IFUmfrage <- FALSE
  
  source('./prediction_interval.R')
  UInt.Z.ST <- pred.interval$u_intervall
  OInt.Z.ST <- pred.interval$o_intervall
  temp_mean <- pred.interval$mean
  temp_median <- pred.interval$median
  write.csv2(cbind(UInt.Z.ST, OInt.Z.ST[, c(2 : 4)], temp_mean[, c(2 : 4)], temp_median[, c(2 : 4)]), file = './Prediction_Results/W_5_Z_Ko_IntST.csv', row.names = FALSE)
  W.5.Z.Ko.IntST <- cbind(UInt.Z.ST, OInt.Z.ST[, c(2 : 4)], temp_mean[, c(2 : 4)], temp_median[, c(2 : 4)])
  
  ## Konfidenzintervalle: Zensus, Stadtbezirke
  pred.sum <- AggPred.Z.SB
  aggregation <- "Stadtbezirk"
  source('./prediction_interval.R')
  UInt.Z.SB <- pred.interval$u_intervall
  OInt.Z.SB <- pred.interval$o_intervall
  temp_mean <- pred.interval$mean
  temp_median <- pred.interval$median
  write.csv2(cbind(UInt.Z.SB, OInt.Z.SB[, c(2 : 4)], temp_mean[, c(2 : 4)], temp_median[, c(2 : 4)]), file = './Prediction_Results/W_5_Z_Ko_IntSB.csv', row.names = FALSE)
  W.5.Z.Ko.IntSB <- cbind(UInt.Z.SB, OInt.Z.SB[, c(2 : 4)], temp_mean[, c(2 : 4)], temp_median[, c(2 : 4)])
  
} else {
  S21.3.U.Ko.IntST <- read.csv2('./Prediction_Results/W_5_U_Ko_IntST.csv', as.is = TRUE)
  S21.3.U.Ko.IntSB <- read.csv2('./Prediction_Results/W_5_U_Ko_IntSB.csv', as.is = TRUE)
  S21.3.Z.Ko.IntST <- read.csv2('./Prediction_Results/W_5_Z_Ko_IntST.csv', as.is = TRUE)
  S21.3.Z.Ko.IntSB <- read.csv2('./Prediction_Results/W_5_Z_Ko_IntSB.csv', as.is = TRUE)
}





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
