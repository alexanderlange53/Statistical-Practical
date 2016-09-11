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
calc_CI <- FALSE # Konfidenzintervalle berechnen und als CSV speichern? Dauert sehr lange, je nach Bootstrap-Wiederholungen bis zu mehreren Stunden!!

## Laden der Daten ##
if(bearbeiter == 'Alex') {
  setwd('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Presi/Statistical-Practical')
  sample <- read.table("/home/alex/Schreibtisch/Uni/statistisches_praktikum/Auswertung/Neue_Daten/Stuttgart21_aufbereitet.csv", header=TRUE, sep=";")
  bezirke <- readOGR(dsn = "/home/alex/Schreibtisch/Uni/statistisches_praktikum/Auswertung/Geodaten/bezirke", layer = "bezirke")
  stadtteile <- readOGR(dsn = "/home/alex/Schreibtisch/Uni/statistisches_praktikum/Daten_Kneib/Stick/Daten_Kneib/Stadtteile_netto", layer = "Stadtteile_netto")
  Bezirke.Val <- read.csv2('Bezirke_True.csv', as.is = TRUE)
  Stadtteile.Val <- read.csv2('Stadtteile_True.csv', as.is = T)
  if(loadGeo){
    Umfrage <- read.csv2('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Daten_Kneib/Stick/buergerumfrage/population_aufbereitet_stadtteile.txt', as.is = TRUE)
    Zensus <- read.csv2('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Daten_Kneib/Stick/zensus/population_aufbereitet_stadtteile.txt', as.is = TRUE)
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
  Bezirke.Val <- read.csv2('Bezirke_True.csv', as.is = TRUE)
  Stadtteile.Val <- read.csv2('Stadtteile_True.csv', as.is = T)
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
  Bezirke.Val <- read.csv2('Bezirke_True.csv', as.is = TRUE)
  Stadtteile.Val <- read.csv2('Stadtteile_True.csv', as.is = T)
}
if(bearbeiter == 'Cluster') {
  cat('Auf dem Cluster gibt es keinen GIT Ordner. Die Dateien müssen manuell aktualisiert werden. Es sollte keine Datei verändert werden. 
      Es ist eine alte, nicht kompatible Version von RCpp installiert. Die aktuelle RCpp Version muss händisch geladen werden, sonst funktioniert die subset Funktion nicht.')
  setwd('/home/khusmann/Statistical-Practical/')
  sample <- read.table("./Rohdaten/buergerumfrage_neu/Stuttgart21_aufbereitet_stadtteile.csv", header=TRUE, sep=";")
  Umfrage <- read.csv2('./Rohdaten/buergerumfrage/population_aufbereitet_stadtteile.txt', as.is = TRUE)
  Zensus <- read.csv2('./Rohdaten/zensus/population_aufbereitet_stadtteile.txt', as.is = TRUE)
  bezirke <- readOGR(dsn = "./Rohdaten/Geodaten/bezirke/", layer = "bezirke")
  stadtteile <- readOGR(dsn = "./Rohdaten/Geodaten/Stadtteile_Shapefile/", layer = "Stadtteile_netto")
}
# Funktionen
source("stepAIC.R")
source("evaluation.R")
source('DataPrep.R')
source('MarkovRandomField.R')
source('PseudoB.R')
source("prediction_function.R")
source('PredBarPlot.R')
source('validation.R')

#-------------------#
# Daten vorbereiten #
#-------------------#

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




#--------------------#
## Modellerstellung ##
#--------------------#

## Step AIC ##
if(calculate_model){
  step.model <- stepAIC()
  saveRDS(step.model, file="./Model_Results/step.model_all.rds")
} else {
  step.model <- readRDS(file = "./Model_Results/step.model_all.rds")
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

if(pred) {
  ## Vorhersage der individuellen Ausprägung ##
  pred.U.k <- Prediction(Umfrage, step.model$model.spat, IFUmfrage = T, binom = F)
  pred.Z.k <- Prediction(Zensus, step.model$model.spat, IFUmfrage = F, binom = F)
  write.csv2(pred.U.k, file = './Prediction_Results/S21_3_U_Ko_Einzel.csv', row.names=FALSE, quote=FALSE)
  write.csv2(pred.Z.k, file = './Prediction_Results/S21_3_Z_Ko_Einzel.csv', row.names=FALSE, quote=FALSE)
  
  ## Aggregation = Räumliche Extrapolation ##
  AggPred.U.ST <- Prediction.Aggregation(pred = pred.U.k[, c(1 : 3, 6)], agg = 'Stadtteil')
  AggPred.Z.ST <- Prediction.Aggregation(pred = pred.Z.k[, c(1 : 3, 6)], agg = 'Stadtteil')
  AggPred.U.SB <- Prediction.Aggregation(pred = pred.U.k[, c(1 : 3, 7)], agg = 'Stadtbezirk')
  AggPred.Z.SB <- Prediction.Aggregation(pred = pred.Z.k[, c(1 : 3, 7)], agg = 'Stadtbezirk')
  write.csv2(AggPred.U.ST, file = './Prediction_Results/S21_3_U_Ko_AggST.csv', row.names = FALSE, quote = FALSE)
  write.csv2(AggPred.Z.ST, file = './Prediction_Results/S21_3_Z_Ko_AggST.csv', row.names = FALSE, quote = FALSE)
  write.csv2(AggPred.U.SB, file = './Prediction_Results/S21_3_U_Ko_AggSB.csv', row.names = FALSE, quote = FALSE)
  write.csv2(AggPred.Z.SB, file = './Prediction_Results/S21_3_Z_Ko_AggSB.csv', row.names = FALSE, quote = FALSE)
} else {
  pred.U.k <- read.csv2('./Prediction_Results/S21_3_U_Ko_Einzel.csv', as.is = TRUE)
  pred.Z.k <- read.csv2('./Prediction_Results/S21_3_Z_Ko_Einzel.csv', as.is = TRUE)
  
  AggPred.U.ST <- read.csv2('./Prediction_Results/S21_3_U_Ko_AggST.csv', as.is = TRUE)
  AggPred.Z.ST <- read.csv2('./Prediction_Results/S21_3_Z_Ko_AggST.csv', as.is = TRUE)
  AggPred.U.SB <- read.csv2('./Prediction_Results/S21_3_U_Ko_AggSB.csv', as.is = TRUE)
  AggPred.Z.SB <- read.csv2('./Prediction_Results/S21_3_Z_Ko_AggSB.csv', as.is = TRUE)
}



PredBarPlot(sample, pred.U.k, Variable = 'Meinung zu Stuttgart 21', 
            x = c('Zustimmung', 'Neutral', 'Ablehnung'))
PredBarPlot(sample, pred.Z.k, Variable = 'Meinung zu Stuttgart 21',
            x = c('Zustimmung', 'Neutral', 'Ablehnung'))

## Konfidenzintervalle ##
if(calc_CI) {
  ## Allg. Einstellungen
  model <- step.model$model.spat
  sample <- sample
  ncores <- 4
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
  write.csv2(cbind(UInt.U.ST, OInt.U.ST[, c(2 : 4)], temp_mean[, c(2 : 4)], temp_median[, c(2 : 4)]), file = './Prediction_Results/S21_3_U_Ko_IntST.csv', row.names = FALSE)
  S21.3.U.Ko.IntST <- cbind(UInt.U.ST, OInt.U.ST[, c(2 : 4)], temp_mean[, c(2 : 4)], temp_median[, c(2 : 4)])
  rm(list = c('UInt.U.ST', 'OInt.U.ST', 'temp_mean', 'temp_median', 'pred.interval'))
  
  ## Konfidenzintervalle: Umfrage, Stadtbezirke ##
  pred.sum <- AggPred.U.SB
  aggregation <- "Stadtbezirk"
  
  source('./prediction_interval.R')
  UInt.U.SB <- pred.interval$u_intervall
  OInt.U.SB <- pred.interval$o_intervall
  temp_mean <- pred.interval$mean
  temp_median <- pred.interval$median
  write.csv2(cbind(UInt.U.SB, OInt.U.SB[, c(2 : 4)], temp_mean[, c(2 : 4)], temp_median[, c(2 : 4)]), file = './Prediction_Results/S21_3_U_Ko_IntSB.csv', row.names = FALSE)
  S21.3.U.Ko.IntSB <- cbind(UInt.U.SB, OInt.U.SB[, c(2 : 4)], temp_mean[, c(2 : 4)], temp_median[, c(2 : 4)])
  rm(list = c('UInt.U.SB', 'OInt.U.SB', 'temp_mean', 'temp_median', 'pred.interval'))
  
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
  write.csv2(cbind(UInt.Z.ST, OInt.Z.ST[, c(2 : 4)], temp_mean[, c(2 : 4)], temp_median[, c(2 : 4)]), file = './Prediction_Results/S21_3_Z_Ko_IntST.csv', row.names = FALSE)
  S21.3.Z.Ko.IntST <- cbind(UInt.Z.ST, OInt.Z.ST[, c(2 : 4)], temp_mean[, c(2 : 4)], temp_median[, c(2 : 4)])
  rm(list = c('UInt.Z.ST', 'OInt.Z.ST', 'temp_mean', 'temp_median', 'pred.interval'))
  
  ## Konfidenzintervalle: Zensus, Stadtbezirke
  pred.sum <- AggPred.Z.SB
  aggregation <- "Stadtbezirk"
  
  source('./prediction_interval.R')
  UInt.Z.SB <- pred.interval$u_intervall
  OInt.Z.SB <- pred.interval$o_intervall
  temp_mean <- pred.interval$mean
  temp_median <- pred.interval$median
  write.csv2(cbind(UInt.Z.SB, OInt.Z.SB[, c(2 : 4)], temp_mean[, c(2 : 4)], temp_median[, c(2 : 4)]), file = './Prediction_Results/S21_3_Z_Ko_IntSB.csv', row.names = FALSE)
  S21.3.Z.Ko.IntSB <- cbind(UInt.Z.SB, OInt.Z.SB[, c(2 : 4)], temp_mean[, c(2 : 4)], temp_median[, c(2 : 4)])
  rm(list = c('UInt.Z.SB', 'OInt.Z.SB', 'temp_mean', 'temp_median', 'pred.interval'))
  
} else {
  S21.3.U.Ko.IntST <- read.csv2('./Boot_Results/S21_3_U_Ko_IntST.csv', as.is = TRUE)
  S21.3.U.Ko.IntSB <- read.csv2('./Boot_Results/S21_3_U_Ko_IntSB.csv', as.is = TRUE)
  S21.3.Z.Ko.IntST <- read.csv2('./Boot_Results/S21_3_Z_Ko_IntST.csv', as.is = TRUE)
  S21.3.Z.Ko.IntSB <- read.csv2('./Boot_Results/S21_3_Z_Ko_IntSB.csv', as.is = TRUE)
}

#-------------#
# Validierung #
#-------------#

# Validierung auf Bezirksebene
validation(pred = S21.3.U.Ko.IntSB, valid = Bezirke.Val)
validation(pred = S21.3.Z.Ko.IntSB, valid = Bezirke.Val)

# Validierung auf Stadtteilebene (Ohne Briefwahl)
validation(pred = S21.3.U.Ko.IntST, valid = Stadtteile.Val[,-1])
validation(pred = S21.3.Z.Ko.IntST, valid = Stadtteile.Val[-20,-1]) # Beim Zensus fehlt ein Stadtteil

#--------------------------------------------#
#### Bezirke als Räumliche Informationen #####-----------------------------------------------------------------
#--------------------------------------------#

# Erstellen des Markov-Random fields
zt <- MarkovRandomField(bezirke, Bezirke = T)

# Neue raeumliche Information, der Rest bleibt gleich
fixed <- "s(Stadtbezirk, bs=\"mrf\", xt = zt) + s(Personenzahl.im.Haushalt, Altersklasse.Befragter, bs= \"tp\")"


#--------------------#
## Modellerstellung ##
#--------------------#

## Step AIC ##
if(calculate_model){
  step.model.B <- stepAIC()
  saveRDS(step.model.B, file="./Model_Results/step.model_all_B.rds")
} else {
  step.model.B <- readRDS(file = "./Model_Results/step.model_all_B.rds")
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

if(pred) {
  ## Vorhersage der individuellen Ausprägung ##
  pred.U.B <- Prediction(Umfrage, step.model.B$model.spat, IFUmfrage = T, binom = F)
  pred.Z.B <- Prediction(Zensus, step.model.B$model.spat, IFUmfrage = F, binom = F)
  write.csv2(pred.U.B, file = './Prediction_Results/S21_3_U_SB_einzel.csv', row.names=FALSE, quote=FALSE)
  write.csv2(pred.Z.B, file = './Prediction_Results/S21_3_Z_SB_einzel.csv', row.names=FALSE, quote=FALSE)
  
  ## Aggregation = Räumliche Extrapolation ##
  AggPred.U.B.ST <- Prediction.Aggregation(pred = pred.U.B[, c(1 : 3, 6)], agg = 'Stadtteil')
  AggPred.Z.B.ST <- Prediction.Aggregation(pred = pred.Z.B[, c(1 : 3, 6)], agg = 'Stadtteil')
  AggPred.U.B.SB <- Prediction.Aggregation(pred = pred.U.B[, c(1 : 3, 7)], agg = 'Stadtbezirk')
  AggPred.Z.B.SB <- Prediction.Aggregation(pred = pred.Z.B[, c(1 : 3, 7)], agg = 'Stadtbezirk')
  write.csv2(AggPred.U.B.ST, file = './Prediction_Results/S21_3_U_SB_AggST.csv', row.names = FALSE, quote = FALSE)
  write.csv2(AggPred.Z.B.ST, file = './Prediction_Results/S21_3_Z_SB_AggST.csv', row.names = FALSE, quote = FALSE)
  write.csv2(AggPred.U.B.SB, file = './Prediction_Results/S21_3_U_SB_AggSB.csv', row.names = FALSE, quote = FALSE)
  write.csv2(AggPred.Z.B.SB, file = './Prediction_Results/S21_3_Z_SB_AggSB.csv', row.names = FALSE, quote = FALSE)
} else{
  pred.U.B <- read.csv2('./Prediction_Results/S21_3_U_SB_einzel.csv')
  pred.Z.B <- read.csv2('./Prediction_Results/S21_3_Z_SB_einzel.csv')
  
  AggPred.U.B.ST <- read.csv2(file = './Prediction_Results/S21_3_U_SB_AggST.csv', as.is = TRUE)
  AggPred.Z.B.ST <- read.csv2(file = './Prediction_Results/S21_3_Z_SB_AggST.csv', as.is = TRUE)
  AggPred.U.B.SB <- read.csv2(file = './Prediction_Results/S21_3_U_SB_AggSB.csv', as.is = TRUE)
  AggPred.Z.B.SB <- read.csv2(file = './Prediction_Results/S21_3_Z_SB_AggSB.csv', as.is = TRUE)
}

PredBarPlot(sample, pred.U.B, x = c('Zustimmung', 'Neutral', 'Ablehnung'))
PredBarPlot(sample, pred.Z.B, x = c('Zustimmung', 'Neutral', 'Ablehnung'))

## Konfidenzintervalle ##
if (calc_CI){
  ## Allg. Einstellungen
  model <- step.model.B$model.spat
  sample <- sample
  ncores <- 4
  nboot <- 4
  coverage <- 0.95
  seed <- 123
  
  ## Konfidenzintervalle: Umfrage, Stadtteile ##
  population <- Umfrage
  aggregation <- "Stadtteil"
  pred.sum <- AggPred.U.B.ST
  IFUmfrage <- TRUE
  source('./prediction_interval.R')
  UInt.U.B.ST <- pred.interval$u_intervall
  OInt.U.B.ST <- pred.interval$o_intervall
  temp_mean <- pred.interval$mean
  temp_median <- pred.interval$median
  write.csv2(cbind(UInt.U.B.ST, OInt.U.B.ST[, c(2 : 4)], temp_mean[, c(2 : 4)], temp_median[, c(2 : 4)]), file = './Prediction_Results/S21_3_U_SB_IntST.csv', row.names = FALSE)
  S21.3.U.SB.IntST <- cbind(UInt.U.B.ST, OInt.U.B.ST[, c(2 : 4)], temp_mean[, c(2 : 4)], temp_median[, c(2 : 4)])
  rm(list = c('UInt.U.B.ST', 'OInt.U.B.ST', 'temp_mean', 'temp_median', 'pred.interval'))
  
  ## Konfidenzintervalle: Umfrage, Stadtbezirke ##
  pred.sum <- AggPred.U.B.SB
  aggregation <- "Stadtbezirk"
  source('./prediction_interval.R')
  UInt.U.B.SB <- pred.interval$u_intervall
  OInt.U.B.SB <- pred.interval$o_intervall
  temp_mean <- pred.interval$mean
  temp_median <- pred.interval$median
  write.csv2(cbind(UInt.U.B.SB, OInt.U.B.SB[, c(2 : 4)], temp_mean[, c(2 : 4)], temp_median[, c(2 : 4)]), file = './Prediction_Results/S21_3_U_SB_IntSB.csv', row.names = FALSE)
  S21.3.U.SB.IntSB <- cbind(UInt.U.B.SB, OInt.U.B.SB[, c(2 : 4)], temp_mean[, c(2 : 4)], temp_median[, c(2 : 4)])
  rm(list = c('UInt.U.B.SB', 'OInt.U.B.SB', 'temp_mean', 'temp_median', 'pred.interval'))
  
  ## Konfidenzintervalle: Zensus, Stadtteile
  population <- Zensus
  aggregation <- "Stadtteil"
  pred.sum <- AggPred.Z.B.ST
  IFUmfrage <- FALSE
  
  source('./prediction_interval.R')
  UInt.Z.B.ST <- pred.interval$u_intervall
  OInt.Z.B.ST <- pred.interval$o_intervall
  temp_mean <- pred.interval$mean
  temp_median <- pred.interval$median
  write.csv2(cbind(UInt.Z.B.ST, OInt.Z.B.ST[, c(2 : 4)], temp_mean[, c(2 : 4)], temp_median[, c(2 : 4)]), file = './Prediction_Results/S21_3_Z_SB_IntST.csv', row.names = FALSE)
  S21.3.Z.SB.IntST <- cbind(UInt.Z.B.ST, OInt.Z.B.ST[, c(2 : 4)], temp_mean[, c(2 : 4)], temp_median[, c(2 : 4)])
  rm(list = c('UInt.Z.B.ST', 'OInt.Z.B.ST', 'temp_mean', 'temp_median', 'pred.interval'))
  
  ## Konfidenzintervalle: Zensus, Stadtbezirke
  pred.sum <- AggPred.Z.B.SB
  aggregation <- "Stadtbezirk"
  
  source('./prediction_interval.R')
  UInt.Z.B.SB <- pred.interval$u_intervall
  OInt.Z.B.SB <- pred.interval$o_intervall
  temp_mean <- pred.interval$mean
  temp_median <- pred.interval$median
  write.csv2(cbind(UInt.Z.B.SB, OInt.Z.B.SB[, c(2 : 4)], temp_mean[, c(2 : 4)], temp_median[, c(2 : 4)]), file = './Prediction_Results/S21_3_Z_SB_IntSB.csv', row.names = FALSE)
  S21.3.Z.SB.IntSB <- cbind(UInt.Z.B.SB, OInt.Z.B.SB[, c(2 : 4)], temp_mean[, c(2 : 4)], temp_median[, c(2 : 4)])
  rm(list = c('UInt.Z.B.SB', 'OInt.Z.B.SB', 'temp_mean', 'temp_median', 'pred.interval'))
} else {
  S21.3.U.SB.IntST <- read.csv2('./Boot_Results/S21_3_U_SB_IntST.csv', as.is = TRUE)
  S21.3.U.SB.IntSB <- read.csv2('./Boot_Results/S21_3_U_SB_IntSB.csv', as.is = TRUE)
  S21.3.Z.SB.IntST <- read.csv2('./Boot_Results/S21_3_Z_SB_IntST.csv', as.is = TRUE)
  S21.3.Z.SB.IntSB <- read.csv2('./Boot_Results/S21_3_Z_SB_IntSB.csv', as.is = TRUE)
}

#---------------#
## Validierung ##
#---------------#

# Validierung auf Bezirksebene
validation(pred = S21.3.U.SB.IntSB, valid = Bezirke.Val)
validation(pred = S21.3.Z.SB.IntSB, valid = Bezirke.Val)

# Validierung auf Stadtteilebene (Ohne Briefwahl)
validation(pred = S21.3.U.SB.IntST, valid = Stadtteile.Val[,-1])
validation(pred = S21.3.Z.SB.IntST, valid = Stadtteile.Val[-20,-1]) # Beim Zensus fehlt ein Stadtteil



#-----------------------------------------------#
#### Stadtteile als Räumliche Informationen #####-----------------------------------------------------------------------------
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
if(calculate_model){
  step.model.S <- stepAIC()
  saveRDS(step.model.S, file="./Model_Results/step.model_all_S.rds")
} else {
  step.model.S <- readRDS(file = "./Model_Results/step.model_all_S.rds")
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

if(pred){
  ## Vorhersage der individuellen Ausprägung ##
  pred.U.S <- Prediction(Umfrage, step.model.S$model.spat, IFUmfrage = T, binom = F) ##
  pred.Z.S <- Prediction(Zensus, step.model.S$model.spat, IFUmfrage = F, binom = F)
  write.csv2(pred.U.S, file = './Prediction_Results/S21_3_U_ST_einzel.csv', row.names=FALSE, quote=FALSE)
  write.csv2(pred.Z.S, file = './Prediction_Results/S21_3_Z_ST_einzel.csv', row.names=FALSE, quote=FALSE)
  
  ## Aggregation = Räumliche Extrapolation ##
  AggPred.U.S.ST <- Prediction.Aggregation(pred = pred.U.S[, c(1 : 3, 6)], agg = 'Stadtteil')
  AggPred.Z.S.ST <- Prediction.Aggregation(pred = pred.Z.S[, c(1 : 3, 6)], agg = 'Stadtteil')
  AggPred.U.S.SB <- Prediction.Aggregation(pred = pred.U.S[, c(1 : 3, 7)], agg = 'Stadtbezirk')
  AggPred.Z.S.SB <- Prediction.Aggregation(pred = pred.Z.S[, c(1 : 3, 7)], agg = 'Stadtbezirk')
  write.csv2(AggPred.U.S.ST, file = './Prediction_Results/S21_3_U_ST_AggST.csv', row.names = FALSE, quote = FALSE)
  write.csv2(AggPred.Z.S.ST, file = './Prediction_Results/S21_3_Z_ST_AggST.csv', row.names = FALSE, quote = FALSE)
  write.csv2(AggPred.U.S.SB, file = './Prediction_Results/S21_3_U_ST_AggSB.csv', row.names = FALSE, quote = FALSE)
  write.csv2(AggPred.Z.S.SB, file = './Prediction_Results/S21_3_Z_ST_AggSB.csv', row.names = FALSE, quote = FALSE)
  
}else{
  pred.U.S <- read.csv2('./Prediction_Results/S21_3_U_ST_einzel.csv')
  pred.Z.S <- read.csv2('./Prediction_Results/S21_3_Z_ST_einzel.csv')
  
  AggPred.U.S.ST <- read.csv2(file = './Prediction_Results/S21_3_U_ST_AggST.csv', as.is = TRUE)
  AggPred.Z.S.ST <- read.csv2(file = './Prediction_Results/S21_3_Z_ST_AggST.csv', as.is = TRUE)
  AggPred.U.S.SB <- read.csv2(file = './Prediction_Results/S21_3_U_ST_AggSB.csv', as.is = TRUE)
  AggPred.Z.S.SB <- read.csv2(file = './Prediction_Results/S21_3_Z_ST_AggSB.csv', as.is = TRUE)
}

PredBarPlot(sample, pred.U.S, x = c('Zustimmung', 'Neutral', 'Ablehnung'))
PredBarPlot(sample, pred.Z.S, x = c('Zustimmung', 'Neutral', 'Ablehnung'))


## Konfidenzintervalle ##
if(calc_CI) {
  ## Allg. Einstellungen
  model <- step.model.S$model.spat
  sample <- sample
  ncores <- 4
  nboot <- 4
  coverage <- 0.95
  seed <- 123
  
  ## Konfidenzintervalle: Umfrage, Stadtteile ##
  population <- Umfrage
  aggregation <- "Stadtteil"
  pred.sum <- AggPred.U.S.ST
  IFUmfrage <- TRUE
  source('./prediction_interval.R')
  UInt.U.S.ST <- pred.interval$u_intervall
  OInt.U.S.ST <- pred.interval$o_intervall
  write.csv2(cbind(UInt.U.S.ST, OInt.U.S.ST[, c(2 : 4)]), file = './Prediction_Results/S21_3_U_ST_IntST.csv', row.names = FALSE)
  Int.U.S.ST <- cbind(UInt.U.S.ST, OInt.U.S.ST[, c(2 : 4)])
  ## Konfidenzintervalle: Umfrage, Stadtbezirke ##
  pred.sum <- AggPred.U.B.SB
  aggregation <- "Stadtbezirk"
  
  source('./prediction_interval.R')
  UInt.U.B.SB <- pred.interval$u_intervall
  OInt.U.B.SB <- pred.interval$o_intervall
  write.csv2(cbind(UInt.U.B.SB, OInt.U.B.SB[, c(2 : 4)]), file = './Prediction_Results/S21_3_U_SB_IntSB.csv', row.names = FALSE)
  Int.U.B.SB <- cbind(UInt.U.B.SB, OInt.U.B.SB[, c(2 : 4)])
  
  ## Konfidenzintervalle: Zensus, Stadtteile
  population <- Zensus
  aggregation <- "Stadtteil"
  pred.sum <- AggPred.Z.B.ST
  IFUmfrage <- FALSE
  
  source('./prediction_interval.R')
  UInt.Z.B.ST <- pred.interval$u_intervall
  OInt.Z.B.ST <- pred.interval$o_intervall
  
  ## Konfidenzintervalle: Zensus, Stadtbezirke
  pred.sum <- AggPred.Z.SB
  aggregation <- "Stadtbezirk"
  
  source('./prediction_interval.R')
  UInt.Z.SB <- pred.interval$u_intervall
  OInt.Z.SB <- pred.interval$o_intervall
}

#-------------#
# Validierung #
#-------------#

# Validierung auf Bezirksebene
validation(pred = AggPred.U.S.SB, valid = Bezirke.Val)
validation(pred = AggPred.Z.S.SB, valid = Bezirke.Val)

# Validierung auf Stadtteilebene (Ohne Briefwahl)
validation(pred = AggPred.U.S.ST, valid = Stadtteile.Val[,-1])
validation(pred = AggPred.Z.S.ST, valid = Stadtteile.Val[-20,-1]) # Beim Zensus fehlt ein Stadtteil