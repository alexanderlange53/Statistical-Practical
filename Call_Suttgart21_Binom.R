#------------------------------------------------#
#### Laden der Daten und Aufrufen der Skripte ####
#------------------------------------------------#

rm(list = ls())

## Pakete laden
require(rgdal);require(rgeos)
require(ggplot2)
require(maptools);require(rvest);require(dplyr)
library(ggplot2)
library("ROCR")
library("mgcv")
library("splines")
library('reshape2')
library(dplyr)

## Einstellungen ##

bearbeiter = 'Alex'
loadGeo <- TRUE # Geodaten laden?
calculate_model <- FALSE # Modelle erstellen und als RDS speichern? Oder als RDS laden
cross_eval <- FALSE # Kreuzevaluierung
pred = FALSE # Vorhersage berechnen und als CSV speichern? Oder CSV laden
calc_CI <- FALSE # Konfidenzintervalle berechnen und als CSV speichern? Dauert sehr lange, je nach Bootstrap-Wiederholungen bis zu mehreren Stunden!!

## Laden der Daten ##
if(bearbeiter == 'Alex') {
  setwd('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Presi/Statistical-Practical')
  sample <- read.table("/home/alex/Schreibtisch/Uni/statistisches_praktikum/Auswertung/Neue_Daten/Stuttgart21_aufbereitet.csv", header=TRUE, sep=";")
  bezirke <- readOGR(dsn = "/home/alex/Schreibtisch/Uni/statistisches_praktikum/Auswertung/Geodaten/bezirke", layer = "bezirke")
  stadtteile <- readOGR(dsn = "/home/alex/Schreibtisch/Uni/statistisches_praktikum/Daten_Kneib/Stick/Daten_Kneib/Stadtteile_netto", layer = "Stadtteile_netto")
  Bezirke.Val <- read.csv2('Bezirke_True.csv', as.is = TRUE)
  Stadtteile.Val <- read.csv2('Stadtteile_True.csv', as.is = TRUE)
  if(loadGeo){
    Umfrage <- read.csv2('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Daten_Kneib/Stick/buergerumfrage/population_aufbereitet_stadtteile.txt')
    Zensus <- read.csv2('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Daten_Kneib/Stick/zensus/population_aufbereitet_stadtteile.txt')
  }
} 
if(bearbeiter == 'Kai@Work') {
  setwd('/home/khusmann/mnt/U/Promotion/Kurse/Stat_Praktikum/Praesentation1_06062016/Statistical-Practical/')
  sample <- read.table("./Rohdaten/buergerumfrage_neu/Stuttgart21_aufbereitet.csv", header=TRUE, sep=";")
  bezirke <- readOGR(dsn = "./Rohdaten/Geodaten/bezirke/", layer = "bezirke")
  stadtteile <- readOGR(dsn = "./Rohdaten/Geodaten/Stadtteile_Shapefile/", layer = "Stadtteile_netto")
  Bezirke.Val <- read.csv2('Bezirke_True.csv', as.is = TRUE)
  Stadtteile.Val <- read.csv2('Stadtteile_True.csv', as.is = TRUE)
  if(loadGeo){
    Umfrage <- read.csv2('./Rohdaten/buergerumfrage/population_aufbereitet_stadtteile.txt')
    Zensus <- read.csv2('./Rohdaten/zensus/population_aufbereitet_stadtteile.txt')
  }
}
if(bearbeiter == 'Kai@Home') {
  setwd('/home/kai/Dokumente/Master/Stat_Practical/Statistical-Practical/')
  sample <- read.table("./Rohdaten/buergerumfrage_neu/Stuttgart21_aufbereitet.csv", header=TRUE, sep=";")
  bezirke <- readOGR(dsn = "/home/kai/Dokumente/Master/Stat_Practical/Statistical-Practical/Rohdaten/Geodaten/bezirke/", layer = "bezirke")
  stadtteile <- readOGR(dsn = "/home/kai/Dokumente/Master/Stat_Practical/Statistical-Practical/Rohdaten/Geodaten/Stadtteile_netto/", layer = "Stadtteile_netto")
  Bezirke.Val <- read.csv2('Bezirke_True.csv', as.is = TRUE)
  Stadtteile.Val <- read.csv2('Stadtteile_True.csv', as.is = TRUE)
  if(loadGeo){
    Umfrage <- read.csv2('/home/kai/Dokumente/Master/Stat_Practical/Statistical-Practical/Rohdaten/buergerumfrage/population_aufbereitet_stadtteile.txt')
    Zensus <- read.csv2('/home/kai/Dokumente/Master/Stat_Practical/Statistical-Practical/Rohdaten/zensus/population_aufbereitet_stadtteile.txt')
  }
}
if(bearbeiter == 'Cluster') {
  cat('Auf dem Cluster gibt es keinen GIT Ordner. Die Dateien müssen manuell aktualisiert werden. Es sollte keine Datei verändert werden. 
      Es ist eine alte, nicht kompatible Version von RCpp installiert. Die aktuelle RCpp Version muss händisch geladen werden, sonst funktioniert die subset Funktion nicht.')
  setwd('/home/khusmann/Statistical-Practical/')
  sample <- read.table("./Rohdaten/buergerumfrage_neu/Stuttgart21_aufbereitet.csv", header=TRUE, sep=";")
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
source('PseudoB2.R')
source("evaluation.R")
source("prediction_function.R")
source('PredBarPlot.R')
source('validation.R')

#---------------------------------------------#
#### Kontinuierliche räumliche Information ####
#---------------------------------------------#

#-------------------#
# Daten vorbereiten #
#-------------------#

# Wenn binom = T:
# Fasst die Gruppen 1 und 2 zu == 1 zusammen und
# 4 und 5 == 0
# Gruppen 6 und 3 werden gelöscht
#sample2 <- DataPrep(sample, binom = F)
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
fixed <- "s(X, Y, bs=\"tp\")" # die WW zwischen s(Personenzahl.im.Haushalt, Altersklasse.Befragter, bs = "tp") ist nicht signifikant

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

#--------------------#
## Model Evaluation ##
#--------------------#
evaluate.bivariate(step.model.binom$model.spat, data = sample)
evaluateAll.bivariate(step.model.binom, data = sample)

if (cross_eval){
  ## Cross Evaluation ##
  repeatitions = 2377
  model <- step.model.binom$model.spat
  
  leave_out <- sample.int(n = dim(sample)[1], size = repeatitions)
  crosseval <- data.frame(Observation.No = integer(), Observed.y = integer(), Predicted.y = integer())
    
  for (i in c(1 : repeatitions)) {
    all <- c(1 : dim(sample)[1])
    subset_i <- all[-leave_out[i]]
    print(paste('Model', i, 'of', repeatitions))
    gam_i <- gam(model$formula, family = model$family, method="REML", data = sample, weights = as.vector(sample[, "Gewicht"]), subset = as.vector(subset_i)) # Fit a GAM
    ret_i <- cbind(leave_out[i], sample$Meinung.zu.Stuttgart.21[leave_out[i]], predict(model, newdata = sample[leave_out[i],], type = "response")) # Compare true and estiamted y.
    crosseval <- rbind(crosseval, ret_i)
  }
  names(crosseval) = c("Observation.No", "Observed.y", "Predicted.Prob")
  crosseval$Predicted.y <- NA; crosseval$Predicted.y[crosseval$Predicted.Prob < 0.5] <- 0; crosseval$Predicted.y[crosseval$Predicted.Prob >= 0.5] <- 1
  rm(list = c("all", "subset_i", "gam_i", "ret_i"))
  write.csv2(crosseval, './cv_results/S21_2_Ko.csv', row.names = FALSE)
} else {
  cv.2 <- read.csv2('./cv_results/S21_2_Ko.csv', as.is = TRUE)
}
cv.2 <- cv.2[,-c(3)]
crossval(cv.2, sample)

#---------------#
## Prediction  ##
#---------------#


if(pred){
  ## Vorhersage der individuellen Ausprägung ##
  pred.binom.U <- Prediction(Umfrage, step.model.binom$model.spat, IFUmfrage = T, binom = T)
  pred.binom.Z <- Prediction(Zensus, step.model.binom$model.spat, IFUmfrage = F, binom = T)
  write.csv2(pred.binom.U, file = './Prediction_Results/S21_2_U_Ko_Einzel.csv', row.names=FALSE, quote=FALSE)
  write.csv2(pred.binom.Z, file = './Prediction_Results/S21_2_Z_Ko_Einzel.csv', row.names=FALSE, quote=FALSE)
  
  ## Aggregation = Räumliche Extrapolation
  AggPred.U.ST <- Prediction.Aggregation(pred = pred.binom.U[, c(1, 4)], agg = "Stadtteil", Anteile = T)
  AggPred.Z.ST <- Prediction.Aggregation(pred = pred.binom.Z[, c(1, 4)], agg = "Stadtteil", Anteile = T)
  AggPred.U.SB <- Prediction.Aggregation(pred = pred.binom.U[, c(1, 5)], agg = "Stadtbezirk", Anteile = T)
  AggPred.Z.SB <- Prediction.Aggregation(pred = pred.binom.Z[, c(1, 5)], agg = "Stadtbezirk", Anteile = T)
  write.csv2(AggPred.U.ST, file = './Prediction_Results/S21_2_U_Ko_AggST.csv', row.names = FALSE, quote = FALSE)
  write.csv2(AggPred.Z.ST, file = './Prediction_Results/S21_2_Z_Ko_AggST.csv', row.names = FALSE, quote = FALSE)
  write.csv2(AggPred.U.SB, file = './Prediction_Results/S21_2_U_Ko_AggSB.csv', row.names = FALSE, quote = FALSE)
  write.csv2(AggPred.Z.SB, file = './Prediction_Results/S21_2_Z_Ko_AggSB.csv', row.names = FALSE, quote = FALSE)
}else{
  pred.binom.U <- read.csv2('./Prediction_Results/S21_2_U_Ko_Einzel.csv')
  pred.binom.Z <- read.csv2('./Prediction_Results/S21_2_Z_Ko_Einzel.csv')
  
  AggPred.U.ST <- read.csv2('./Prediction_Results/S21_2_U_Ko_AggST.csv', as.is = TRUE)
  AggPred.Z.ST <- read.csv2('./Prediction_Results/S21_2_Z_Ko_AggST.csv', as.is = TRUE)
  AggPred.U.SB <- read.csv2('./Prediction_Results/S21_2_U_Ko_AggSB.csv', as.is = TRUE)
  AggPred.Z.SB <- read.csv2('./Prediction_Results/S21_2_Z_Ko_AggSB.csv', as.is = TRUE)
}


PredBarPlot(sample, pred.binom.U) # Die Zustimmungsw. ist 1 - Ablehnung
PredBarPlot(sample, pred.binom.Z)

## Konfidenzintervalle ##

if(calc_CI) {
  ## Allg. Einstellungen
  model <- step.model.binom$model.spat
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
  source('./prediction_interval_binom.R')
  UInt.U.ST <- pred.interval$u_intervall
  OInt.U.ST <- pred.interval$o_intervall
  temp_mean <- pred.interval$mean
  temp_median <- pred.interval$median
  S21.2.U.Ko.IntST <- cbind(UInt.U.ST, OInt.U.ST[, c(2)], temp_mean[, c(2)], temp_median[, c(2)])
  names(S21.2.U.Ko.IntST)[c(3 : 5)] <- c("o_intervall_1", "mean_1", "median_1")
  write.csv2(S21.2.U.Ko.IntST, file = './Prediction_Results/S21_2_U_Ko_IntST.csv', row.names = FALSE)
  
  rm(list = c('UInt.U.ST', 'OInt.U.ST', 'temp_mean', 'temp_median', 'pred.interval'))
  
  ## Konfidenzintervalle: Umfrage, Stadtbezirke ##
  pred.sum <- AggPred.U.SB
  aggregation <- "Stadtbezirk"
  
  source('./prediction_interval_binom.R')
  UInt.U.SB <- pred.interval$u_intervall
  OInt.U.SB <- pred.interval$o_intervall
  temp_mean <- pred.interval$mean
  temp_median <- pred.interval$median
  S21.2.U.Ko.IntSB <- cbind(UInt.U.SB, OInt.U.SB[, c(2)], temp_mean[, c(2)], temp_median[, c(2)])
  names(S21.2.U.Ko.IntSB)[c(3 : 5)] <- c("o_intervall_1", "mean_1", "median_1")
  write.csv2(S21.2.U.Ko.IntSB, file = './Prediction_Results/S21_2_U_Ko_IntSB.csv', row.names = FALSE)
  
  rm(list = c('UInt.U.SB', 'OInt.U.SB', 'temp_mean', 'temp_median', 'pred.interval'))
  
  ## Konfidenzintervalle: Zensus, Stadtteile
  population <- Zensus
  aggregation <- "Stadtteil"
  pred.sum <- AggPred.Z.ST
  IFUmfrage <- FALSE
  
  source('./prediction_interval_binom.R')
  UInt.Z.ST <- pred.interval$u_intervall
  OInt.Z.ST <- pred.interval$o_intervall
  temp_mean <- pred.interval$mean
  temp_median <- pred.interval$median
  S21.2.Z.Ko.IntST <- cbind(UInt.Z.ST, OInt.Z.ST[, c(2)], temp_mean[, c(2)], temp_median[, c(2)])
  names(S21.2.Z.Ko.IntST)[c(3 : 5)] <- c("o_intervall_1", "mean_1", "median_1")
  write.csv2(S21.2.Z.Ko.IntST, file = './Prediction_Results/S21_2_Z_Ko_IntST.csv', row.names = FALSE)
  
  rm(list = c('UInt.Z.ST', 'OInt.Z.ST', 'temp_mean', 'temp_median', 'pred.interval'))
  
  ## Konfidenzintervalle: Zensus, Stadtbezirke
  pred.sum <- AggPred.Z.SB
  aggregation <- "Stadtbezirk"
  
  source('./prediction_interval_binom.R')
  UInt.Z.SB <- pred.interval$u_intervall
  OInt.Z.SB <- pred.interval$o_intervall
  temp_mean <- pred.interval$mean
  temp_median <- pred.interval$median
  S21.2.Z.Ko.IntSB <- cbind(UInt.Z.SB, OInt.Z.SB[, c(2)], temp_mean[, c(2)], temp_median[, c(2)])
  names(S21.2.Z.Ko.IntSB)[c(3 : 5)] <- c("o_intervall_1", "mean_1", "median_1")
  write.csv2(S21.2.Z.Ko.IntSB, file = './Prediction_Results/S21_2_Z_Ko_IntSB.csv', row.names = FALSE)
  
  rm(list = c('UInt.Z.SB', 'OInt.Z.SB', 'temp_mean', 'temp_median', 'pred.interval'))
  
} else {
  S21.2.U.Ko.IntST <- read.csv2('./Boot_Results/S21_2_U_Ko_IntST.csv', as.is = TRUE)
  S21.2.U.Ko.IntSB <- read.csv2('./Boot_Results/S21_2_U_Ko_IntSB.csv', as.is = TRUE)
  S21.2.Z.Ko.IntST <- read.csv2('./Boot_Results/S21_2_Z_Ko_IntST.csv', as.is = TRUE)
  S21.2.Z.Ko.IntSB <- read.csv2('./Boot_Results/S21_2_Z_Ko_IntSB.csv', as.is = TRUE)
}

#---------------#
## Validierung ##
#---------------#

# Validierung auf Bezirksebene
v <- validation(pred = S21.2.U.Ko.IntSB, valid = Bezirke.Val, pop = Umfrage, errorbar = T)
validation(pred = S21.2.Z.Ko.IntSB, valid = Bezirke.Val, pop = Zensus, errorbar = T)
v <- v + ggtitle('Zwei Klassen Bezirke') 

# Validierung auf Stadtteilebene (Ohne Briefwahl)
v2 <-validation(pred = S21.2.U.Ko.IntST, valid = Stadtteile.Val[,-1], pop = Umfrage, errorbar = T)
validation(pred = S21.2.Z.Ko.IntST, valid = Stadtteile.Val[-20,-1], pop = Zensus, errorbar = T) 
v2 <- v2 + ggtitle('Zwei Klassen Stadtteile') 

#--------------------------------------#
# Bezirke als Räumliche Informationen  #-----------------------------------------------------------------
#--------------------------------------#

# Erstellen des Markov-Random fields
zt <- MarkovRandomField(bezirke, Bezirke = T)

# Neue raeumliche Information, der rest bleibt gleich
fixed <- "s(Stadtbezirk, bs=\"mrf\", xt = zt)" # also auch wieder ohne s(Personenzahl.im.Haushalt, Altersklasse.Befragter, bs= \"tp\")

#--------------------#
## Modellerstellung ##
#--------------------#

## Step AIC ##
if(calculate_model){
  step.model.binom.B <- stepAIC()
  saveRDS(step.model.binom.B$model.spat, file="step.model_binomB.rds")
  saveRDS(step.model.binom.B, file="step.model_all_binomB.rds")
} else {
  step.model.binom.B <- readRDS(file = "step.model_all_binomB.rds")
}

#--------------------#
## Model Evaluation ##
#--------------------#
evaluate.bivariate(step.model.binom.B$model.spat, data = sample)
evaluateAll.bivariate(step.model.binom.B, data = sample)

if(cross_eval) {
  ## Cross Evaluation ##
  repeatitions = 2377
  model <- step.model.binom.B$model.spat
  
  leave_out <- sample.int(n = dim(sample)[1], size = repeatitions)
  crosseval <- data.frame(Observation.No = integer(), Observed.y = integer(), Predicted.y = integer())
  
  for (i in c(1 : repeatitions)) {
    all <- c(1 : dim(sample)[1])
    subset_i <- all[-leave_out[i]]
    print(paste('Model', i, 'of', repeatitions))
    gam_i <- gam(model$formula, family = model$family, method="REML", data = sample, weights = as.vector(sample[, "Gewicht"]), subset = as.vector(subset_i)) # Fit a GAM
    ret_i <- cbind(leave_out[i], sample$Meinung.zu.Stuttgart.21[leave_out[i]], predict(model, newdata = sample[leave_out[i],], type = "response")) # Compare true and estiamted y.
    crosseval <- rbind(crosseval, ret_i)
  }
  names(crosseval) = c("Observation.No", "Observed.y", "Predicted.Prob")
  crosseval$Predicted.y <- NA; crosseval$Predicted.y[crosseval$Predicted.Prob < 0.5] <- 0; crosseval$Predicted.y[crosseval$Predicted.Prob >= 0.5] <- 1
  rm(list = c("all", "subset_i", "gam_i", "ret_i"))
  write.csv2(crosseval, './cv_results/S21_2_SB.csv', row.names = FALSE)
} else {
  cv.2.B <- read.csv2('./cv_results/S21_2_SB.csv', as.is = TRUE)
}
cv.2.B <- cv.2.B[,-3]
crossval(cv.2.B, sample)


#--------------------------------#
## Modelleffekte interpretieren ##
#--------------------------------#
## GAM Plots ##
m1 <- step.model.binom.B$model.spat
plot(m1, select = 1, all = TRUE, ylab = "GK Hochwert", xlab = "GK Rechtswert") # Cont. spat. effect
plot(m1, select = 2, all = TRUE, ylab = "s(Altersklasse)", xlab = "Altersklasse") # Alter

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
#plot(step.model.binom.B$model.spat, all = T)

#---------------#
## Prediction  ##
#---------------#

if(pred) {
  ## Vorhersage der individuellen Ausprägung ##
  pred.binom.U.B <- Prediction(Umfrage, step.model.binom.B$model.spat, IFUmfrage = T, binom = T)
  pred.binom.Z.B <- Prediction(Zensus, step.model.binom.B$model.spat, IFUmfrage = F, binom = T)
  write.csv2(pred.binom.U.B, file = './Prediction_Results/S21_2_U_SB_einzel.csv', row.names=FALSE, quote=FALSE)
  write.csv2(pred.binom.Z.B, file = './Prediction_Results/S21_2_Z_SB_einzel.csv', row.names=FALSE, quote=FALSE)
  
  ## Aggregation = Räumliche Extrapolation ##
  AggPred.U.B.ST <- Prediction.Aggregation(pred = pred.binom.U.B[, c(1, 4)], agg = 'Stadtteil', Anteile = T)
  AggPred.Z.B.ST <- Prediction.Aggregation(pred = pred.binom.Z.B[, c(1, 4)], agg = 'Stadtteil', Anteile = T)
  AggPred.U.B.SB <- Prediction.Aggregation(pred = pred.binom.U.B[, c(1, 5)], agg = 'Stadtbezirk', Anteile = T)
  AggPred.Z.B.SB <- Prediction.Aggregation(pred = pred.binom.Z.B[, c(1, 5)], agg = 'Stadtbezirk', Anteile = T)
  write.csv2(AggPred.U.B.ST, file = './Prediction_Results/S21_2_U_SB_AggST.csv', row.names = FALSE, quote = FALSE)
  write.csv2(AggPred.Z.B.ST, file = './Prediction_Results/S21_2_Z_SB_AggST.csv', row.names = FALSE, quote = FALSE)
  write.csv2(AggPred.U.B.SB, file = './Prediction_Results/S21_2_U_SB_AggSB.csv', row.names = FALSE, quote = FALSE)
  write.csv2(AggPred.Z.B.SB, file = './Prediction_Results/S21_2_Z_SB_AggSB.csv', row.names = FALSE, quote = FALSE)
} else{
  pred.binom.U.B <- read.csv2('./Prediction_Results/S21_2_U_SB_einzel.csv')
  pred.binom.Z.B <- read.csv2('./Prediction_Results/S21_2_Z_SB_einzel.csv')
  
  AggPred.U.B.ST <- read.csv2(file = './Prediction_Results/S21_2_U_SB_AggST.csv', as.is = TRUE)
  AggPred.Z.B.ST <- read.csv2(file = './Prediction_Results/S21_2_Z_SB_AggST.csv', as.is = TRUE)
  AggPred.U.B.SB <- read.csv2(file = './Prediction_Results/S21_2_U_SB_AggSB.csv', as.is = TRUE)
  AggPred.Z.B.SB <- read.csv2(file = './Prediction_Results/S21_2_Z_SB_AggSB.csv', as.is = TRUE)
}

PredBarPlot(sample, pred.binom.U.B, x = c('Zustimmung', 'Ablehnung'))
PredBarPlot(sample, pred.binom.Z.B, x = c('Zustimmung', 'Ablehnung'))

## Konfidenzintervalle ##

if(calc_CI) {
  ## Allg. Einstellungen
  model <- step.model.binom.B$model.spat
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
  source('./prediction_interval_binom.R')
  UInt.U.ST <- pred.interval$u_intervall
  OInt.U.ST <- pred.interval$o_intervall
  temp_mean <- pred.interval$mean
  temp_median <- pred.interval$median
  S21.2.U.SB.IntST <- cbind(UInt.U.ST, OInt.U.ST[, c(2)], temp_mean[, c(2)], temp_median[, c(2)])
  names(S21.2.U.SB.IntST)[c(3 : 5)] <- c("o_intervall_1", "mean_1", "median_1")
  write.csv2(S21.2.U.SB.IntST, file = './Prediction_Results/S21_2_U_SB_IntST.csv', row.names = FALSE)
  
  rm(list = c('UInt.U.ST', 'OInt.U.ST', 'temp_mean', 'temp_median', 'pred.interval'))
  
  ## Konfidenzintervalle: Umfrage, Stadtbezirke ##
  pred.sum <- AggPred.U.B.SB
  aggregation <- "Stadtbezirk"
  
  source('./prediction_interval_binom.R')
  UInt.U.SB <- pred.interval$u_intervall
  OInt.U.SB <- pred.interval$o_intervall
  temp_mean <- pred.interval$mean
  temp_median <- pred.interval$median
  S21.2.U.SB.IntSB <- cbind(UInt.U.SB, OInt.U.SB[, c(2)], temp_mean[, c(2)], temp_median[, c(2)])
  names(S21.2.U.SB.IntSB)[c(3 : 5)] <- c("o_intervall_1", "mean_1", "median_1")
  write.csv2(S21.2.U.SB.IntSB, file = './Prediction_Results/S21_2_U_SB_IntSB.csv', row.names = FALSE)
  
  rm(list = c('UInt.U.SB', 'OInt.U.SB', 'temp_mean', 'temp_median', 'pred.interval'))
  
  ## Konfidenzintervalle: Zensus, Stadtteile
  population <- Zensus
  aggregation <- "Stadtteil"
  pred.sum <- AggPred.Z.B.ST
  IFUmfrage <- FALSE
  
  source('./prediction_interval_binom.R')
  UInt.Z.ST <- pred.interval$u_intervall
  OInt.Z.ST <- pred.interval$o_intervall
  temp_mean <- pred.interval$mean
  temp_median <- pred.interval$median
  S21.2.Z.SB.IntST <- cbind(UInt.Z.ST, OInt.Z.ST[, c(2)], temp_mean[, c(2)], temp_median[, c(2)])
  names(S21.2.Z.SB.IntST)[c(3 : 5)] <- c("o_intervall_1", "mean_1", "median_1")
  write.csv2(S21.2.Z.SB.IntST, file = './Prediction_Results/S21_2_Z_SB_IntST.csv', row.names = FALSE)
  
  rm(list = c('UInt.Z.ST', 'OInt.Z.ST', 'temp_mean', 'temp_median', 'pred.interval'))
  
  ## Konfidenzintervalle: Zensus, Stadtbezirke
  pred.sum <- AggPred.Z.B.SB
  aggregation <- "Stadtbezirk"
  
  source('./prediction_interval_binom.R')
  UInt.Z.SB <- pred.interval$u_intervall
  OInt.Z.SB <- pred.interval$o_intervall
  temp_mean <- pred.interval$mean
  temp_median <- pred.interval$median
  S21.2.Z.SB.IntSB <- cbind(UInt.Z.SB, OInt.Z.SB[, c(2)], temp_mean[, c(2)], temp_median[, c(2)])
  names(S21.2.Z.SB.IntSB)[c(3 : 5)] <- c("o_intervall_1", "mean_1", "median_1")
  write.csv2(S21.2.Z.SB.IntSB, file = './Prediction_Results/S21_2_Z_SB_IntSB.csv', row.names = FALSE)
  
  rm(list = c('UInt.Z.SB', 'OInt.Z.SB', 'temp_mean', 'temp_median', 'pred.interval'))
  
} else {
  S21.2.U.SB.IntST <- read.csv2('./Boot_Results/S21_2_U_SB_IntST.csv', as.is = TRUE)
  S21.2.U.SB.IntSB <- read.csv2('./Boot_Results/S21_2_U_SB_IntSB.csv', as.is = TRUE)
  S21.2.Z.SB.IntST <- read.csv2('./Boot_Results/S21_2_Z_SB_IntST.csv', as.is = TRUE)
  S21.2.Z.SB.IntSB <- read.csv2('./Boot_Results/S21_2_Z_SB_IntSB.csv', as.is = TRUE)
}

#---------------#
## Validierung ##
#---------------#

# Validierung auf Bezirksebene
validation(pred = S21.2.U.SB.IntSB, valid = Bezirke.Val, pop = Umfrage, errorbar = T)
v22 <- validation(pred = S21.2.Z.SB.IntSB, valid = Bezirke.Val, pop = Zensus, errorbar = T)
v22 <- v22 + ggtitle('Zwei Klassen Bezirke') 

# Validierung auf Stadtteilebene (Ohne Briefwahl)
validation(pred = S21.2.U.SB.IntST, valid = Stadtteile.Val[,-1], pop = Umfrage, errorbar = T)
vv22 <- validation(pred = S21.2.Z.SB.IntST, valid = Stadtteile.Val[-20,-1], pop = Zensus, errorbar = T) 
vv22 <- vv22 + ggtitle('Zwei Klassen Stadtteile')

#-----------------------------------------#
# Stadtteile als Räumliche Informationen  #--------------------------------------------------------------
#-----------------------------------------#

# Erstellen des Markov Random fields
zt <- MarkovRandomField(stadtteile, Bezirke = F)

# Erstellen der Pseudo Beobachtungen und in Datensatz integrieren
sample <- PseudoB2(sample, SpatOb =  stadtteile, binom = T, response)

#sample$Stadtteil <- as.character(sample$Stadtteil)
# Neue raeumliche Information, der rest bleibt gleich
fixed <- "s(Stadtteil, bs=\"mrf\", xt = zt) + s(Personenzahl.im.Haushalt, Altersklasse.Befragter, bs= \"tp\")"

#--------------------#
## Modellerstellung ##
#--------------------#

## Step AIC ##
if(calculate_model){
  step.model.binom.S <- stepAIC()
  saveRDS(step.model.binom.S, file="./Model_Results/step.model_all_binomS.rds")
} else {
  step.model.binom.S <- readRDS(file = "./Model_Results/step.model_all_binomS.rds")
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
#plot(step.model.binom.S$model.spat, all = T)

#--------------------#
## Model Evaluation ##
#--------------------#

evaluate.bivariate(step.model.binom.S$model.spat, data = sample)
evaluateAll.bivariate(step.model.binom.S, data = sample)

if(cross_eval) {
  ## Cross Evaluation ##
  repeatitions = 2377
  
  
  model <- step.model.binom.S$model.spat
  
  leave_out <- sample.int(n = dim(sample)[1], size = repeatitions)
  crosseval <- data.frame(Observation.No = integer(), Observed.y = integer(), Predicted.y = integer())
  
  tryfun <- function(subset_i, sample_i, crosseval) {
    gam_i <- gam(model$formula, family = model$family, method="REML", data = sample_i, weights = as.vector(sample_i[, "Gewicht"])) # Fit a GAM
    ret_i <- cbind(leave_out[i], sample$Meinung.zu.Stuttgart.21[leave_out[i]], ifelse(predict(model, newdata = sample[leave_out[i],], type = "response") <=0.5, 0, 1)) # Compare true and estiamted y.
    crosseval <- rbind(crosseval, ret_i)
    return(crosseval)
  }
  
  for (i in c(1 : repeatitions)) {
    all <- c(1 : dim(sample)[1])
    subset_i <- all[-leave_out[i]]
    sample_i <- sample[subset_i,]
    sample_i <- PseudoB2(sample = sample_i, SpatOb = stadtteile, binom = T, response = response)
    
    print(paste('Model', i, 'of', repeatitions))
    
    try(
      crosseval <- tryfun(subset_i, sample_i, crosseval)
    )
  }
  names(crosseval) = c("Observation.No", "Observed.y", "Predicted.y")
  #rm(list = c("all", "subset_i", "gam_i", "ret_i"))
  cv.2.ST <- crosseval
  write.csv2(crosseval, './cv_results/S21_2_ST.csv')
}
table(cv.2.ST$Observed.y, cv.3.ST$Predicted.y)
#---------------#
## Prediction  ##
#---------------#

if(pred) {
  ## Vorhersage der individuellen Ausprägung ##
  pred.binom.U.B <- Prediction(Umfrage, step.model.binom.S$model.spat, IFUmfrage = T, binom = T)
  pred.binom.Z.B <- Prediction(Zensus, step.model.binom.S$model.spat, IFUmfrage = F, binom = T)
  write.csv2(pred.binom.U.B, file = './Prediction_Results/S21_2_U_ST_einzel.csv', row.names=FALSE, quote=FALSE)
  write.csv2(pred.binom.Z.B, file = './Prediction_Results/S21_2_Z_ST_einzel.csv', row.names=FALSE, quote=FALSE)
  
  ## Aggregation = Räumliche Extrapolation ##
  AggPred.U.ST.ST <- Prediction.Aggregation(pred = pred.binom.U.B[, c(1, 4)], agg = 'Stadtteil', Anteile = T)
  AggPred.Z.ST.ST <- Prediction.Aggregation(pred = pred.binom.Z.B[, c(1, 4)], agg = 'Stadtteil', Anteile = T)
  AggPred.U.ST.SB <- Prediction.Aggregation(pred = pred.binom.U.B[, c(1, 5)], agg = 'Stadtbezirk', Anteile = T)
  AggPred.Z.ST.SB <- Prediction.Aggregation(pred = pred.binom.Z.B[, c(1, 5)], agg = 'Stadtbezirk', Anteile = T)
  write.csv2(AggPred.U.ST.ST, file = './Prediction_Results/S21_2_U_ST_AggST.csv', row.names = FALSE, quote = FALSE)
  write.csv2(AggPred.Z.ST.ST, file = './Prediction_Results/S21_2_Z_ST_AggST.csv', row.names = FALSE, quote = FALSE)
  write.csv2(AggPred.U.ST.SB, file = './Prediction_Results/S21_2_U_ST_AggSB.csv', row.names = FALSE, quote = FALSE)
  write.csv2(AggPred.Z.ST.SB, file = './Prediction_Results/S21_2_Z_ST_AggSB.csv', row.names = FALSE, quote = FALSE)
} else{
  pred.binom.U.B <- read.csv2('./Prediction_Results/S21_2_U_ST_einzel.csv')
  pred.binom.Z.B <- read.csv2('./Prediction_Results/S21_2_Z_ST_einzel.csv')
  
  AggPred.U.ST.ST <- read.csv2(file = './Prediction_Results/S21_2_U_ST_AggST.csv', as.is = TRUE)
  AggPred.Z.ST.ST <- read.csv2(file = './Prediction_Results/S21_2_Z_ST_AggST.csv', as.is = TRUE)
  AggPred.U.ST.SB <- read.csv2(file = './Prediction_Results/S21_2_U_ST_AggSB.csv', as.is = TRUE)
  AggPred.Z.ST.SB <- read.csv2(file = './Prediction_Results/S21_2_Z_ST_AggSB.csv', as.is = TRUE)
}

## Konfidenzintervalle ##
if(calc_CI) {
  ## Allg. Einstellungen
  model <- step.model.S$model.spat
  sample <- sample
  ncores <- 4
  nboot <- 1000
  coverage <- 0.95
  seed <- 123
  
  ## Konfidenzintervalle: Umfrage, Stadtteile ##
  population <- Umfrage
  aggregation <- "Stadtteil"
  pred.sum <- AggPred.U.S.ST
  IFUmfrage <- TRUE
  IFStadtteil <- TRUE
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

#---------------#
## Validierung ##
#---------------#

# Validierung auf Bezirksebene
validation(pred = AggPred.U.ST.SB, valid = Bezirke.Val, pop = Umfrage)
validation(pred = AggPred.Z.ST.SB, valid = Bezirke.Val, pop = Zensus)

# Validierung auf Stadtteilebene (Ohne Briefwahl)
validation(pred = AggPred.U.ST.ST, valid = Stadtteile.Val[,-1], pop = Umfrage)
validation(pred = AggPred.Z.ST.ST, valid = Stadtteile.Val[-20,-1], pop =  Zensus) 


