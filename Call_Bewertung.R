#------------------------------------------------#
#### Laden der Daten und Aufrufen der Skripte ####
#------------------------------------------------#

rm(list = ls())

## Pakete und Funktionen laden ##
library("ROCR")
library("mgcv")
library("splines")
#library(MASS)
require(rgdal);require(rgeos)
require(ggplot2)
require(maptools);require(rvest);require(dplyr)
library(ggplot2)
library(reshape2)

## Einstellungen ##

bearbeiter <- 'Alex'
loadGeo <- TRUE # Geodaten laden?
calculate_model <- FALSE# Modelle erstellen und als RDS speichern? Oder als RDS laden
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
  Stadtteile.Val <- read.csv2('Stadtteile_True.csv', as.is = T)
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
  if(loadGeo){
    Umfrage <- read.csv2('./Rohdaten/buergerumfrage/population_aufbereitet_stadtteile.txt', as.is = TRUE)
    Zensus <- read.csv2('./Rohdaten/zensus/population_aufbereitet_stadtteile.txt', as.is = TRUE)
  }
  Bezirke.Val <- read.csv2('Bezirke_True.csv', as.is = TRUE)
  Stadtteile.Val <- read.csv2('Stadtteile_True.csv', as.is = T)
}
if(bearbeiter == 'Kai@Home') {
  setwd('/home/kai/Dokumente/Master/Stat_Practical/Statistical-Practical/')
  sample <- read.table("./Rohdaten/buergerumfrage_neu/Stuttgart21_aufbereitet.csv", header=TRUE, sep=";")
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
  cat('Auf dem Cluster gibt es keinen GIT Ordner. Die Dateien müssen manuell aktualisiert werden. Es sollte keine Datei verändert werden.')
  setwd('/home/khusmann/Statistical-Practical/')
  sample <- read.table("./Rohdaten/buergerumfrage_neu/Stuttgart21_aufbereitet.csv", header=TRUE, sep=";")
  Umfrage <- read.csv2('./Rohdaten/buergerumfrage/population_aufbereitet_stadtteile.txt', as.is = TRUE)
  Zensus <- read.csv2('./Rohdaten/zensus/population_aufbereitet_stadtteile.txt', as.is = TRUE)
  bezirke <- readOGR(dsn = "./Rohdaten/Geodaten/bezirke/", layer = "bezirke")
  stadtteile <- readOGR(dsn = "./Rohdaten/Geodaten/Stadtteile_Shapefile/", layer = "Stadtteile_netto")
  Bezirke.Val <- read.csv2('Bezirke_True.csv', as.is = TRUE)
  Stadtteile.Val <- read.csv2('Stadtteile_True.csv', as.is = T)
}

source("stepAIC.R")
source("evaluation.R")
source('DataPrep.R')
source('MarkovRandomField.R')
source('PseudoB2.R')
source("prediction_function.R")
source('PredBarPlot.R')
source('validation.R')
source('SpatialPlots.R')
source('PlotModel.R')
source('spat_effect_plot.R')

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
  saveRDS(step.model.Bewertung.5, file="./Model_Results/step.mode.Bewertungl.5_all.rds")
} else {
  step.model.Bewertung.5 <- readRDS(file = "./Model_Results/step.mode.Bewertungl.5_all.rds")
}

model.i.2 <- data.frame(m = factor(c('s(Spat)', 
                                     's(Spat) + s(Alter)',
                                     's(Spat) + Nationalität + \n
                                      s(Alter)',
                                     's(Spat) +  Nationalität +\n
                                      Person + s(Alter)'
                                     )), mm = factor(1:4))
model.i.2$m <- factor(model.i.2$m, levels = model.i.2[order(model.i.2$mm), 'm'])
aic.i <-c(7062.108, 7055.717, 7051.906, 7051.606)
AIC.it.BW <- data.frame(vari = 'Bewertung der Wohngegend', model.i.2, aic.i)

#--------------------------------#
## Modelleffekte interpretieren ##
#--------------------------------#
## GAM Plots ##
m1 <- step.model.Bewertung.5$model.spat
m.gk.5 <- step.model.Bewertung.5$model.spat
# Non Parametric Effects
variables <- c('Altersklasse.Befragter', 'Personenzahl.im.Haushalt')
g1 <-ggplot.model(m1, variables = variables)
ggsave('./Essay/Pictures/BWGKnoParam.pdf', height = 2.5, width = 8)

# Parametric Effects
variables <- c("Nationalität")
ll <- function(part){   
  data.frame(Variable = part$meta$x, 
             x=part$fit[[part$meta$x]], 
             smooth=part$fit$visregFit, 
             lower=part$fit$visregLwr, 
             upper=part$fit$visregUpr)}
plotdata <- visreg(m1, xvar = variables, plot = FALSE)
smooths <- ll(plotdata)
g2 <- ggplot(smooths, aes(x, smooth)) + geom_hline(yintercept = 0, color = 'red') +
  geom_errorbar(aes(x = x, ymin = lower, ymax= upper), width=.5, position=position_dodge(.05)) +
  geom_point(shape = 21, fill = 'darkblue', size = 5) + labs(x = NULL, y = NULL) +
  facet_grid(. ~ Variable, scales = "free_x") + theme_bw(11)

pdf('./Essay/Pictures/BWModelEffects.pdf', height = 4, width = 8)
grid.arrange(g1,g2, nrow = 2, ncol = 1, top = NULL)
dev.off()


# Spat- Effect #
spat.p.c <- spat.plot.cont(m1)
pdf('./Essay/Pictures/W_5_Kont_SpatEff.pdf', h = 4, w = 4.5)
spat.p.c
dev.off()

ggsave(plot = spat.p.c, filename = './Essay/Pictures/W_5_Kont_SpatEff.pdf', device = 'pdf', height = 5, width = 5.5)


AIC(step.model.Bewertung.5$model.spat)
AIC(step.model.Bewertung.5$model.nospat)
AIC(step.model.Bewertung.5$model.spatonly)
#plot(step.model.Bewertung.5$model.spat, all = TRUE, pages = 1)
#--------------------#
## Model Evaluation ##
#--------------------#
evaluate(step.model.Bewertung.5$model.spat, data = sample)
evaluateAll(step.model.Bewertung.5, data = sample)

# Es ist bei der C.E. nicht wichtigt, dass die unwahrscheinlicheren Gruppen gut getroffen werden. Dies muss vor allem bei der kleinräumigen Extrapolation
# berücksichtigt werden.

## Cross Evaluation ##
if (cross_eval) {
  repeatitions = 3127
  model <- step.model.Bewertung.5$model.spat
  
  leave_out <- sample.int(n = dim(sample)[1], size = repeatitions)
  crosseval <- data.frame(Observation.No = integer(), Observed.y = integer(), Predicted.y = integer())
  for (i in c(1 : repeatitions)) {
    all <- c(1 : dim(sample)[1])
    subset_i <- all[-leave_out[i]]
    print(paste('Model', i, 'of', repeatitions))
    gam_i <- gam(model$formula, family = model$family, method="REML", data = sample, weights = as.vector(sample[, "Gewicht"]), subset = as.vector(subset_i)) # Fit a GAM
    ret_i <- cbind(leave_out[i], sample$Bewertung.Wohngegend[leave_out[i]], apply(predict(model, newdata = sample[leave_out[i],], type = "response"), 1, which.max)) # Compare true and estiamted y.
    crosseval <- rbind(crosseval, ret_i)
  }
  names(crosseval) = c("Observation.No", "Observed.y", "Predicted.y")
  rm(list = c("all", "subset_i", "gam_i", "ret_i", "repeatitions", "model"))
  write.csv2(crosseval,'./cv_results/W_5_Ko.csv', row.names = FALSE)
  cv.5 <- crosseval
} else {
  cv.5 <- read.csv2('./cv_results/W_5_Ko.csv', as.is = TRUE)
}
table(cv.5$Observed.y, cv.5$Predicted.y)
crossval(cv.5, sample)

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
  pred.U.k <- read.csv2('./Prediction_Results/W_5_U_Ko_Einzel.csv', as.is = TRUE)
  pred.Z.k <- read.csv2('./Prediction_Results/W_5_Z_Ko_Einzel.csv', as.is = TRUE)
  
  AggPred.U.ST <- read.csv2('./Prediction_Results/W_5_U_Ko_AggST.csv', as.is = TRUE)
  AggPred.Z.ST <- read.csv2('./Prediction_Results/W_5_Z_Ko_AggST.csv', as.is = TRUE)
  AggPred.U.SB <- read.csv2('./Prediction_Results/W_5_U_Ko_AggSB.csv', as.is = TRUE)
  AggPred.Z.SB <- read.csv2('./Prediction_Results/W_5_Z_Ko_AggSB.csv', as.is = TRUE)
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
  write.csv2(cbind(UInt.U.ST, OInt.U.ST[, c(2 : 6)], temp_mean[, c(2 : 6)], temp_median[, c(2 : 6)]), file = './Prediction_Results/W_5_U_Ko_IntST.csv', row.names = FALSE)
  W.5.U.Ko.IntST <- cbind(UInt.U.ST, OInt.U.ST[, c(2 : 6)], temp_mean[, c(2 : 6)], temp_median[, c(2 : 6)])
  rm(list = c('UInt.U.ST', 'OInt.U.ST', 'temp_mean', 'temp_median', 'pred.interval'))
  
  ## Konfidenzintervalle: Umfrage, Stadtbezirke ##
  pred.sum <- AggPred.U.SB
  aggregation <- "Stadtbezirk"
  
  source('./prediction_interval.R')
  UInt.U.SB <- pred.interval$u_intervall
  OInt.U.SB <- pred.interval$o_intervall
  temp_mean <- pred.interval$mean
  temp_median <- pred.interval$median
  write.csv2(cbind(UInt.U.SB, OInt.U.SB[, c(2 : 6)], temp_mean[, c(2 : 6)], temp_median[, c(2 : 6)]), file = './Prediction_Results/W_5_U_Ko_IntSB.csv', row.names = FALSE)
  W.5.U.Ko.IntSB <- cbind(UInt.U.SB, OInt.U.SB[, c(2 : 6)], temp_mean[, c(2 : 6)], temp_median[, c(2 : 6)])
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
  write.csv2(cbind(UInt.Z.ST, OInt.Z.ST[, c(2 : 6)], temp_mean[, c(2 : 6)], temp_median[, c(2 : 6)]), file = './Prediction_Results/W_5_Z_Ko_IntST.csv', row.names = FALSE)
  W.5.Z.Ko.IntST <- cbind(UInt.Z.ST, OInt.Z.ST[, c(2 : 6)], temp_mean[, c(2 : 6)], temp_median[, c(2 : 6)])
  rm(list = c('UInt.Z.ST', 'OInt.Z.ST', 'temp_mean', 'temp_median', 'pred.interval'))
  
  ## Konfidenzintervalle: Zensus, Stadtbezirke
  pred.sum <- AggPred.Z.SB
  aggregation <- "Stadtbezirk"
  
  source('./prediction_interval.R')
  UInt.Z.SB <- pred.interval$u_intervall
  OInt.Z.SB <- pred.interval$o_intervall
  temp_mean <- pred.interval$mean
  temp_median <- pred.interval$median
  write.csv2(cbind(UInt.Z.SB, OInt.Z.SB[, c(2 : 6)], temp_mean[, c(2 : 6)], temp_median[, c(2 : 6)]), file = './Prediction_Results/W_5_Z_Ko_IntSB.csv', row.names = FALSE)
  W.5.Z.Ko.IntSB <- cbind(UInt.Z.SB, OInt.Z.SB[, c(2 : 6)], temp_mean[, c(2 : 6)], temp_median[, c(2 : 6)])
  rm(list = c('UInt.Z.SB', 'OInt.Z.SB', 'temp_mean', 'temp_median', 'pred.interval'))
  
} else {
  W.5.U.Ko.IntST <- read.csv2('./Boot_Results/W_5_U_Ko_IntST.csv', as.is = TRUE)
  W.5.U.Ko.IntSB <- read.csv2('./Boot_Results/W_5_U_Ko_IntSB.csv', as.is = TRUE)
  W.5.Z.Ko.IntST <- read.csv2('./Boot_Results/W_5_Z_Ko_IntST.csv', as.is = TRUE)
  W.5.Z.Ko.IntSB <- read.csv2('./Boot_Results/W_5_Z_Ko_IntSB.csv', as.is = TRUE)
}


#--------------------------------------------#
#### Bezirke als Räumliche Informationen #####-----------------------------------------------------------------
#--------------------------------------------#

# Erstellen des Markov-Random fields
zt <- MarkovRandomField(bezirke, Bezirke = T)

# Neue raeumliche Information, der Rest bleibt gleich
fixed <- "s(Stadtbezirk, bs=\"mrf\", xt = zt)"


#--------------------#
## Modellerstellung ##
#--------------------#

## Step AIC ##
if(calculate_model){
  step.model.Bewertung.5.B <- stepAIC()
  saveRDS(step.model.Bewertung.5.B, file="./Model_Results/step.mode.Bewertungl.5_all_B.rds")
} else {
  step.model.Bewertung.5.B <- readRDS(file = "./Model_Results/step.mode.Bewertungl.5_all_B.rds")
}


#--------------------#
## Model Evaluation ##
#--------------------#

evaluate(step.model.Bewertung.5.B$model.spat, data = sample)
evaluateAll(step.model.Bewertung.5.B, data = sample)

## Cross Evaluation ##
if(cross_eval) {
  repeatitions = 3127
  model <- step.model.Bewertung.5.B$model.spat
  
  leave_out <- sample.int(n = dim(sample)[1], size = repeatitions)
  crosseval <- data.frame(Observation.No = integer(), Observed.y = integer(), Predicted.y = integer())
  
  for (i in c(1 : repeatitions)) {
    all <- c(1 : dim(sample)[1])
    subset_i <- all[-leave_out[i]]
    print(paste('Model', i, 'of', repeatitions))
    gam_i <- gam(model$formula, family = model$family, method="REML", data = sample, weights = as.vector(sample[, "Gewicht"]), subset = as.vector(subset_i)) # Fit a GAM
    ret_i <- cbind(leave_out[i], sample$Bewertung.Wohngegend[leave_out[i]], apply(predict(model, newdata = sample[leave_out[i],], type = "response"), 1, which.max)) # Compare true and estiamted y.
    crosseval <- rbind(crosseval, ret_i)
  }
  names(crosseval) = c("Observation.No", "Observed.y", "Predicted.y")
  rm(list = c("all", "subset_i", "gam_i", "ret_i"))
  write.csv2(crosseval,'./cv_results/W_5_SB.csv', row.names = FALSE)
  cv.5.B <- crosseval
  } else {
  cv.5.B <- read.csv2('./cv_results/W_5_SB.csv', as.is = TRUE)
}

crossval(cv.5.B, sample)
table(cv.5.B$Observed.y, cv.5.B$Predicted.y)

#--------------------------------#
## Modelleffekte interpretieren ##
#--------------------------------#
## GAM Plots ##
m1 <- step.model.Bewertung.5.B$model.spat
m.sb.5 <- step.model.Bewertung.5.B$model.spat
plot(m1, select = 1, all = TRUE, ylab = "GK Hochwert", xlab = "GK Rechtswert") # Cont. spat. effect
plot(m1, select = 3, all = TRUE, ylab = "s(Altersklasse)", xlab = "Altersklasse") # Alter

#x11()
#par(mfrow = c(2, 2))
plot(m1, select = 4, all = TRUE, ann = F) # Geschlecht
#mtext(side = 1, line = 3, "Geschlecht"); mtext(side = 2, line = 3, "Einfluss des Geschlechts")
plot(m1, select = 2, all = TRUE, ann = F) # Nationalität
#mtext(side = 1, line = 3, "Nationalität"); mtext(side = 2, line = 3, "Einfluss der Nationalität")
#plot(m1, select = 6, all = TRUE, ann = F) # Familienstand
#mtext(side = 1, line = 3, "Familienstand"); mtext(side = 2, line = 3, "Einfluss des Familienstands")

dev.off()

AIC(step.model.Bewertung.5.B$model.spat)
AIC(step.model.Bewertung.5.B$model.nospat)
AIC(step.model.Bewertung.5.B$model.spatonly)

summary(step.model.Bewertung.5.B$model.spat)

# Spatial effect
spat.p.bez <- spat.plot.disc(m1, IFbezirk = TRUE)
pdf('./Essay/Pictures/W_5_Bezirke_SpatEff.pdf', h = 4, w = 4.5)
spat.p.bez
dev.off()

#---------------#
## Prediction  ##
#---------------#

if(pred) {
  ## Vorhersage der individuellen Ausprägung ##
  pred.U.B <- Prediction(Umfrage, step.model.Bewertung.5.B$model.spat, IFUmfrage = T, binom = F)
  pred.Z.B <- Prediction(Zensus, step.model.Bewertung.5.B$model.spat, IFUmfrage = F, binom = F)
  write.csv2(pred.U.B, file = './Prediction_Results/W_5_U_SB_einzel.csv', row.names=FALSE, quote=FALSE)
  write.csv2(pred.Z.B, file = './Prediction_Results/W_5_Z_SB_einzel.csv', row.names=FALSE, quote=FALSE)
  
  ## Aggregation = Räumliche Extrapolation ##
  AggPred.U.B.ST <- Prediction.Aggregation(pred = pred.U.B[, c(1 : 5, 8)], agg = 'Stadtteil')
  AggPred.Z.B.ST <- Prediction.Aggregation(pred = pred.Z.B[, c(1 : 5, 8)], agg = 'Stadtteil')
  AggPred.U.B.SB <- Prediction.Aggregation(pred = pred.U.B[, c(1 : 5, 9)], agg = 'Stadtbezirk')
  AggPred.Z.B.SB <- Prediction.Aggregation(pred = pred.Z.B[, c(1 : 5, 9)], agg = 'Stadtbezirk')
  write.csv2(AggPred.U.B.ST, file = './Prediction_Results/W_5_U_SB_AggST.csv', row.names = FALSE, quote = FALSE)
  write.csv2(AggPred.Z.B.ST, file = './Prediction_Results/W_5_Z_SB_AggST.csv', row.names = FALSE, quote = FALSE)
  write.csv2(AggPred.U.B.SB, file = './Prediction_Results/W_5_U_SB_AggSB.csv', row.names = FALSE, quote = FALSE)
  write.csv2(AggPred.Z.B.SB, file = './Prediction_Results/W_5_Z_SB_AggSB.csv', row.names = FALSE, quote = FALSE)
} else{
  pred.U.B <- read.csv2('./Prediction_Results/W_5_U_SB_einzel.csv')
  pred.Z.B <- read.csv2('./Prediction_Results/W_5_Z_SB_einzel.csv')
  
  AggPred.U.B.ST <- read.csv2(file = './Prediction_Results/W_5_U_SB_AggST.csv', as.is = TRUE)
  AggPred.Z.B.ST <- read.csv2(file = './Prediction_Results/W_5_Z_SB_AggST.csv', as.is = TRUE)
  AggPred.U.B.SB <- read.csv2(file = './Prediction_Results/W_5_U_SB_AggSB.csv', as.is = TRUE)
  AggPred.Z.B.SB <- read.csv2(file = './Prediction_Results/W_5_Z_SB_AggSB.csv', as.is = TRUE)
}

PredBarPlot(sample, pred.U.B, x = c('Zustimmung', 'Neutral', 'Ablehnung'))
PredBarPlot(sample, pred.Z.B, x = c('Zustimmung', 'Neutral', 'Ablehnung'))

## Konfidenzintervalle ##
if (calc_CI){
  ## Allg. Einstellungen
  model <- step.model.Bewertung.5.B$model.spat
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
  write.csv2(cbind(UInt.U.B.ST, OInt.U.B.ST[, c(2 : 6)], temp_mean[, c(2 : 6)], temp_median[, c(2 : 6)]), file = './Prediction_Results/W_5_U_SB_IntST.csv', row.names = FALSE)
  W.5.U.SB.IntST <- cbind(UInt.U.B.ST, OInt.U.B.ST[, c(2 : 6)], temp_mean[, c(2 : 6)], temp_median[, c(2 : 6)])
  rm(list = c('UInt.U.B.ST', 'OInt.U.B.ST', 'temp_mean', 'temp_median', 'pred.interval'))
  
  ## Konfidenzintervalle: Umfrage, Stadtbezirke ##
  pred.sum <- AggPred.U.B.SB
  aggregation <- "Stadtbezirk"
  source('./prediction_interval.R')
  UInt.U.B.SB <- pred.interval$u_intervall
  OInt.U.B.SB <- pred.interval$o_intervall
  temp_mean <- pred.interval$mean
  temp_median <- pred.interval$median
  write.csv2(cbind(UInt.U.B.SB, OInt.U.B.SB[, c(2 : 6)], temp_mean[, c(2 : 6)], temp_median[, c(2 : 6)]), file = './Prediction_Results/W_5_U_SB_IntSB.csv', row.names = FALSE)
  W.5.U.SB.IntSB <- cbind(UInt.U.B.SB, OInt.U.B.SB[, c(2 : 6)], temp_mean[, c(2 : 6)], temp_median[, c(2 : 6)])
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
  write.csv2(cbind(UInt.Z.B.ST, OInt.Z.B.ST[, c(2 : 6)], temp_mean[, c(2 : 6)], temp_median[, c(2 : 6)]), file = './Prediction_Results/W_5_Z_SB_IntST.csv', row.names = FALSE)
  W.5.Z.SB.IntST <- cbind(UInt.Z.B.ST, OInt.Z.B.ST[, c(2 : 6)], temp_mean[, c(2 : 6)], temp_median[, c(2 : 6)])
  rm(list = c('UInt.Z.B.ST', 'OInt.Z.B.ST', 'temp_mean', 'temp_median', 'pred.interval'))
  
  ## Konfidenzintervalle: Zensus, Stadtbezirke
  pred.sum <- AggPred.Z.B.SB
  aggregation <- "Stadtbezirk"
  
  source('./prediction_interval.R')
  UInt.Z.B.SB <- pred.interval$u_intervall
  OInt.Z.B.SB <- pred.interval$o_intervall
  temp_mean <- pred.interval$mean
  temp_median <- pred.interval$median
  write.csv2(cbind(UInt.Z.B.SB, OInt.Z.B.SB[, c(2 : 6)], temp_mean[, c(2 : 6)], temp_median[, c(2 : 6)]), file = './Prediction_Results/W_5_Z_SB_IntSB.csv', row.names = FALSE)
  W.5.Z.SB.IntSB <- cbind(UInt.Z.B.SB, OInt.Z.B.SB[, c(2 : 6)], temp_mean[, c(2 : 6)], temp_median[, c(2 : 6)])
  rm(list = c('UInt.Z.B.SB', 'OInt.Z.B.SB', 'temp_mean', 'temp_median', 'pred.interval'))
} else {
  W.5.U.SB.IntST <- read.csv2('./Boot_Results/W_5_U_SB_IntST.csv', as.is = TRUE)
  W.5.U.SB.IntSB <- read.csv2('./Boot_Results/W_5_U_SB_IntSB.csv', as.is = TRUE)
  W.5.Z.SB.IntST <- read.csv2('./Boot_Results/W_5_Z_SB_IntST.csv', as.is = TRUE)
  W.5.Z.SB.IntSB <- read.csv2('./Boot_Results/W_5_Z_SB_IntSB.csv', as.is = TRUE)
}



#-----------------------------------------------#
#### Stadtteile als Räumliche Informationen #####-----------------------------------------------------------------------------
#-----------------------------------------------#

# Erstellen des Markov Random fields
zt <- MarkovRandomField(stadtteile, Bezirke = F)

# Erstellen der Pseudo Beobachtungen und in Datensatz integrieren
sample <- PseudoB2(sample, SpatOb =  stadtteile, binom = F, response)

# Neue raeumliche Information, der rest bleibt gleich
fixed <- "s(Stadtteil, bs=\"mrf\", xt = zt)"

#--------------------#
## Modellerstellung ##
#--------------------#


## Step AIC ##
if(calculate_model){
  step.model.Bewertung.5.S <- stepAIC()
  saveRDS(step.model.Bewertung.5.S, file="./Model_Results/step.model.Bewertung.5.S.rds")
} else {
  step.model.Bewertung.5.S <- readRDS(file = "./Model_Results/step.model.Bewertung.5.S.rds")
}

#--------------------#
## Model Evaluation ##
#--------------------#

evaluate(step.model.Bewertung.5.S$model.spat, data = sample)
evaluateAll(step.model.Bewertung.5.S, data = sample)


## Cross Evaluation ##
if(cross_eval){
repeatitions = 3437
model <- step.model.Bewertung.5.S$model.spat

leave_out <- sample.int(n = dim(sample)[1], size = repeatitions)
crosseval <- data.frame(Observation.No = integer(), Observed.y = integer(), Predicted.y = integer())

tryfun <- function(subset_i, sample_i, crosseval) {
  gam_i <- gam(model$formula, family = model$family, method="REML", data = sample_i, weights = as.vector(sample_i[, "Gewicht"])) # Fit a GAM
  ret_i <- cbind(leave_out[i], sample$Bewertung.Wohngegend[leave_out[i]], apply(predict(model, newdata = sample[leave_out[i],], type = "response"), 1, which.max)) # Compare true and estiamted y.
  crosseval <- rbind(crosseval, ret_i)
  return(crosseval)
}

for (i in c(1 : repeatitions)) {
  all <- c(1 : dim(sample)[1])
  subset_i <- all[-leave_out[i]]
  sample_i <- sample[subset_i,]
  sample_i <- PseudoB2(sample = sample_i, SpatOb = stadtteile, binom = F, response = response)
  print(paste('Model', i, 'of', repeatitions))
  
  try(
    crosseval <- tryfun(subset_i, sample_i, crosseval)
  )
  
}
names(crosseval) = c("Observation.No", "Observed.y", "Predicted.y")

cv.5.S <- crosseval
table(cv.5.S$Observed.y, cv.5.S$Predicted.y)
write.csv2(cv.5.S, './cv_results/W_5_ST.csv')
} else {
  cv.5.S <- read.csv2('./cv_results/W_5_ST.csv')
}

cv.5.S <- cv.5.S[,-1]
crossval(cv.5.S, sample)

#--------------------------------#
## Modelleffekte interpretieren ##
#--------------------------------#
## GAM Plots ##
m1 <- step.model.Bewertung.5.S$model.spat
m.st.5 <- step.model.Bewertung.5.S$model.spat
stargazer(m.gk.5, m.sb.5, m.st.5, title = 'Parameter Bewertung der Wohngegend')
summary(m.gk.5)
summary(m.sb.5)
summary(m.st.5)


plot(m1, select = 1, all = TRUE, ylab = "GK Hochwert", xlab = "GK Rechtswert") # Cont. spat. effect
plot(m1, select = 2, all = TRUE, ylab = "s(Altersklasse)", xlab = "Altersklasse") # Alter

#x11()
#par(mfrow = c(2, 2))
plot(m1, select = 3, all = TRUE, ann = F) # Geschlecht
#mtext(side = 1, line = 3, "Geschlecht"); mtext(side = 2, line = 3, "Einfluss des Geschlechts")
plot(m1, select = 4, all = TRUE, ann = F) # Nationalität
#mtext(side = 1, line = 3, "Nationalität"); mtext(side = 2, line = 3, "Einfluss der Nationalität")
plot(m1, select = 5, all = TRUE, ann = F) # Familienstand
#mtext(side = 1, line = 3, "Familienstand"); mtext(side = 2, line = 3, "Einfluss des Familienstands")
#plot(m1, select = 6, all = TRUE, ann = F) # Personenzahl
#mtext(side = 1, line = 3, "Personenzahl im Haushalt"); mtext(side = 2, line = 3, "Einfluss der Personenzahl im Haushalt")

dev.off()

AIC(step.model.Bewertung.5.S$model.spat)
AIC(step.model.Bewertung.5.S$model.nospat)
AIC(step.model.Bewertung.5.S$model.spatonly)

# Spatial effect
spat.p.steil <- spat.plot.disc(m1, IFbezirk = FALSE)
pdf('./Essay/Pictures/W_5_Stadtt_SpatEff.pdf', h = 4, w = 4.5)
spat.p.steil
dev.off()



#---------------#
## Prediction  ##
#---------------#

if(pred){
  ## Vorhersage der individuellen Ausprägung ##
  pred.U.S <- Prediction(Umfrage, step.model.Bewertung.5.S$model.spat, IFUmfrage = T, binom = F) ##
  pred.Z.S <- Prediction(Zensus, step.model.Bewertung.5.S$model.spat, IFUmfrage = F, binom = F)
  write.csv2(pred.U.S, file = './Prediction_Results/W_5_U_ST_einzel.csv', row.names=FALSE, quote=FALSE)
  write.csv2(pred.Z.S, file = './Prediction_Results/W_5_Z_ST_einzel.csv', row.names=FALSE, quote=FALSE)
  
  ## Aggregation = Räumliche Extrapolation ##
  AggPred.U.S.ST <- Prediction.Aggregation(pred = pred.U.S[, c(1 : 5, 8)], agg = 'Stadtteil')
  AggPred.Z.S.ST <- Prediction.Aggregation(pred = pred.Z.S[, c(1 : 5, 8)], agg = 'Stadtteil')
  AggPred.U.S.SB <- Prediction.Aggregation(pred = pred.U.S[, c(1 : 5, 9)], agg = 'Stadtbezirk')
  AggPred.Z.S.SB <- Prediction.Aggregation(pred = pred.Z.S[, c(1 : 5, 9)], agg = 'Stadtbezirk')
  write.csv2(AggPred.U.S.ST, file = './Prediction_Results/W_5_U_ST_AggST.csv', row.names = FALSE, quote = FALSE)
  write.csv2(AggPred.Z.S.ST, file = './Prediction_Results/W_5_Z_ST_AggST.csv', row.names = FALSE, quote = FALSE)
  write.csv2(AggPred.U.S.SB, file = './Prediction_Results/W_5_U_ST_AggSB.csv', row.names = FALSE, quote = FALSE)
  write.csv2(AggPred.Z.S.SB, file = './Prediction_Results/W_5_Z_ST_AggSB.csv', row.names = FALSE, quote = FALSE)
  
}else{
  pred.U.S <- read.csv2('./Prediction_Results/W_5_U_ST_einzel.csv')
  pred.Z.S <- read.csv2('./Prediction_Results/W_5_Z_ST_einzel.csv')
  
  AggPred.U.S.ST <- read.csv2(file = './Prediction_Results/W_5_U_ST_AggST.csv', as.is = TRUE)
  AggPred.Z.S.ST <- read.csv2(file = './Prediction_Results/W_5_Z_ST_AggST.csv', as.is = TRUE)
  AggPred.U.S.SB <- read.csv2(file = './Prediction_Results/W_5_U_ST_AggSB.csv', as.is = TRUE)
  AggPred.Z.S.SB <- read.csv2(file = './Prediction_Results/W_5_Z_ST_AggSB.csv', as.is = TRUE)
}

PredBarPlot(sample, pred.U.S, x = c('Zustimmung', 'Neutral', 'Ablehnung'))
PredBarPlot(sample, pred.Z.S, x = c('Zustimmung', 'Neutral', 'Ablehnung'))




## Konfidenzintervalle ##
if(calc_CI) {
  ## Allg. Einstellungen
  model <- step.model.Bewertung.5.S$model.spat
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
  # Dies Funktioniert wegen der Pseudobeob. nicht: Es funktioniert nur das spezifische Skript: prediction_inteval_S21_ST.R
  # Dies muss Schritt für Schritt aufgerufen werden
  # source('./prediction_interval.R')
  source('./prediction_interval_W_ST.R')
  
  UInt <- pred.interval$u_intervall
  OInt <- pred.interval$o_intervall
  meanInt <- pred.interval$mean
  mediInt <- pred.interval$median
  
  Int.U.ST.ST <- cbind(UInt, OInt[, c(2 : 6)], meanInt[, c(2 : 6)], mediInt[, c(2 : 6)])
  write.csv2(Int.U.ST.ST, file = './Prediction_Results/W_5_U_ST_IntST.csv', row.names = FALSE)
  
  ## Konfidenzintervalle: Umfrage, Stadtbezirke ##
  pred.sum <- AggPred.U.B.SB
  aggregation <- "Stadtbezirk"
  # Dies Funktioniert wegen der Pseudobeob. nicht: Es funktioniert nur das spezifische Skript: prediction_inteval_S21_ST.R
  # Dies muss Schritt für Schritt aufgerufen werden
  #source('./prediction_interval.R')
  source('./prediction_interval_W_ST.R')
  
  UInt <- pred.interval$u_intervall
  OInt <- pred.interval$o_intervall
  meanInt <- pred.interval$mean
  mediInt <- pred.interval$median
  
  Int.U.ST.SB <- cbind(UInt, OInt[, c(2 : 6)], meanInt[, c(2 : 6)], mediInt[, c(2 : 6)])
  write.csv2(Int.U.ST.SB, file = './Prediction_Results/W_5_U_ST_IntSB.csv', row.names = FALSE)
  
  
  ## Konfidenzintervalle: Zensus, Stadtteile
  population <- Zensus
  aggregation <- "Stadtteil"
  pred.sum <- AggPred.Z.B.ST
  IFUmfrage <- FALSE
  
  # Dies Funktioniert wegen der Pseudobeob. nicht: Es funktioniert nur das spezifische Skript: prediction_inteval_S21_ST.R
  # Dies muss Schritt für Schritt aufgerufen werden
  # source('./prediction_interval.R')
  source('./prediction_interval_W_ST.R')
  
  UInt <- pred.interval$u_intervall
  OInt <- pred.interval$o_intervall
  meanInt <- pred.interval$mean
  mediInt <- pred.interval$median
  
  Int.Z.ST.ST <- cbind(UInt, OInt[, c(2 : 6)], meanInt[, c(2 : 6)], mediInt[, c(2 : 6)])
  write.csv2(Int.Z.ST.ST, file = './Prediction_Results/W_5_Z_ST_IntST.csv', row.names = FALSE)
  
  
  ## Konfidenzintervalle: Zensus, Stadtbezirke
  pred.sum <- AggPred.Z.SB
  aggregation <- "Stadtbezirk"
  
  # Dies Funktioniert wegen der Pseudobeob. nicht: Es funktioniert nur das spezifische Skript: prediction_inteval_S21_ST.R
  # Dies muss Schritt für Schritt aufgerufen werden
  #source('./prediction_interval.R')
  source('./prediction_interval_W_ST.R')
  
  UInt <- pred.interval$u_intervall
  OInt <- pred.interval$o_intervall
  meanInt <- pred.interval$mean
  mediInt <- pred.interval$median
  
  Int.Z.ST.SB <- cbind(UInt, OInt[, c(2 : 6)], meanInt[, c(2 : 6)], mediInt[, c(2 : 6)])
  write.csv2(Int.Z.ST.SB, file = './Prediction_Results/W_5_Z_ST_IntSB.csv', row.names = FALSE)
  } else {
    W.5.U.ST.IntST <- read.csv2('./Boot_Results/W_5_U_ST_IntST.csv', as.is = TRUE)
    W.5.U.ST.IntSB <- read.csv2('./Boot_Results/W_5_U_ST_IntSB.csv', as.is = TRUE)
    W.5.Z.ST.IntST <- read.csv2('./Boot_Results/W_5_Z_ST_IntST.csv', as.is = TRUE)
    W.5.Z.ST.IntSB <- read.csv2('./Boot_Results/W_5_Z_ST_IntSB.csv', as.is = TRUE)
}



# Insgesamter Vergleich aller geschätzter modelle mit 3 Klassen
predlist <- list(W.5.U.Ko.IntSB[,-c(1,12:16)], W.5.Z.Ko.IntSB[,-c(1,12:16)],
                 W.5.U.SB.IntSB[,-c(1,12:16)], W.5.Z.SB.IntSB[,-c(1,12:16)],
                 W.5.U.ST.IntSB[,-c(1,12:16)], W.5.Z.ST.IntSB[,-c(1,12:16)])
models <- c('1 Gauss-Kr. M.', '1 Gauss-Kr. Z.', 
            '1 Bezirke M.', '1 Bezirke Z.',
            '1 Stadteile M.',
            '1 Stadteile Z.', 
            '2 Gauss-Kr. M.', '2 Gauss-Kr. Z.', 
            '2 Bezirke M.', '2 Bezirke Z.',
            '2 Stadteile M.',
            '2 Stadteile Z.', 
            '3 Gauss-Kr. M.', '3 Gauss-Kr. Z.',
            '3 Bezirke M.', '3 Bezirke Z.',
            '3 Stadteile M.',
            '3 Stadteile Z.', 
            '4 Gauss-Kr. M.', '4 Gauss-Kr. Z.',
            '4 Bezirke M.', '4 Bezirke Z.',
            '4 Stadteile M.',
            '4 Stadteile Z.',
            '5 Gauss-Kr. M.', '5 Gauss-Kr. Z.', 
            '5 Bezirke M.', '5 Bezirke Z.',
            '5 Stadteile M.',
            '5 Stadteile Z.')
ResultPlot5(predlist = predlist, sample = sample, models = models)
ggsave('./Essay/Pictures/WohngegendAlleModelle2.pdf', height = 5, width = 8)

# Anteile
colSums(W.5.U.Ko.IntSB[,17:21])/sum(W.5.U.Ko.IntSB[,17:21])*100
colSums(W.5.U.SB.IntSB[,17:21])/sum(W.5.U.SB.IntSB[,17:21])*100
colSums(W.5.U.ST.IntSB[,17:21])/sum(W.5.U.ST.IntSB[,17:21])*100

ex <- ExtraPlot(W.5.U.Ko.IntST[,c(1,17:21)], Stadtteile = stadtteile, samescale = F)
pdf('./Essay/Pictures/BWohnExtra.pdf', height = 7, width = 8)
marrangeGrob(ex, nrow = 2, ncol = 3, top = NULL)
dev.off()

# Insgesamter Vergleich aller geschätzter modelle ohen Aggregate
predlist <- list(W.5.U.Ko.IntSB[,-c(1,12:16)], W.5.Z.Ko.IntSB[,-c(1,12:16)], W.5.U.Ko.IntST[,-c(1,12:16)],
                 W.5.Z.Ko.IntST[,-c(1,12:16)], W.5.U.SB.IntSB[,-c(1,12:16)], W.5.Z.SB.IntSB[,-c(1,12:16)],
                 W.5.U.SB.IntST[,-c(1,12:16)], W.5.Z.SB.IntST[,-c(1,12:16)], W.5.U.ST.IntSB[,-c(1,12:16)], W.5.Z.ST.IntSB[,-c(1,12:16)],
                 W.5.U.ST.IntST[,-c(1,12:16)], W.5.Z.ST.IntST[,-c(1,12:16)])
models <- c('1 G-K auf Bezirke Umfr.', '1 G-K auf Bezirke Zen.', '1 G-K auf S.Teile Umfr.',
            '1 G-K auf S.Teile Zen.', '1 Bezirke auf Bezirke Umfr.', '1 Bezirke auf Bezirke Zen.',
            '1 Bezirke auf S.Teile Umfr.', '1 Bezirke auf S.Teile Zen.', '1 S.Teile auf Bezirke Umfr.',
            '1 S.Teile auf Bezirke Zen.', '1 S.Teile auf S.Teile Umfr.', '1 S.Teile auf S.Teile Zen.',
            '2 G-K auf Bezirke Umfr.', '2 G-K auf Bezirke Zen.', '2 G-K auf S.Teile Umfr.',
            '2 G-K auf S.Teile Zen.', '2 Bezirke auf Bezirke Umfr.', '2 Bezirke auf Bezirke Zen.',
            '2 Bezirke auf S.Teile Umfr.', '2 Bezirke auf S.Teile Zen.', '2 S.Teile auf Bezirke Umfr.',
            '2 S.Teile auf Bezirke Zen.', '2 S.Teile auf S.Teile Umfr.', '2 S.Teile auf S.Teile Zen.',
            '3 G-K auf Bezirke Umfr.', '3 G-K auf Bezirke Zen.', '3 G-K auf S.Teile Umfr.',
            '3 G-K auf S.Teile Zen.', '3 Bezirke auf Bezirke Umfr.', '3 Bezirke auf Bezirke Zen.',
            '3 Bezirke auf S.Teile Umfr.', '3 Bezirke auf S.Teile Zen.', '3 S.Teile auf Bezirke Umfr.',
            '3 S.Teile auf Bezirke Zen.', '3 S.Teile auf S.Teile Umfr.', '3 S.Teile auf S.Teile Zen.',
            '4 G-K auf Bezirke Umfr.', '4 G-K auf Bezirke Zen.', '4 G-K auf S.Teile Umfr.',
            '4 G-K auf S.Teile Zen.', '4 Bezirke auf Bezirke Umfr.', '4 Bezirke auf Bezirke Zen.',
            '4 Bezirke auf S.Teile Umfr.', '4 Bezirke auf S.Teile Zen.', '4 S.Teile auf Bezirke Umfr.',
            '4 S.Teile auf Bezirke Zen.', '4 S.Teile auf S.Teile Umfr.', '4 S.Teile auf S.Teile Zen.',
            '5 G-K auf Bezirke Umfr.', '5 G-K auf Bezirke Zen.', '5 G-K auf S.Teile Umfr.',
            '5 G-K auf S.Teile Zen.', '5 Bezirke auf Bezirke Umfr.', '5 Bezirke auf Bezirke Zen.',
            '5 Bezirke auf S.Teile Umfr.', '5 Bezirke auf S.Teile Zen.', '5 S.Teile auf Bezirke Umfr.',
            '5 S.Teile auf Bezirke Zen.', '5 S.Teile auf S.Teile Umfr.', '5 S.Teile auf S.Teile Zen.')
ResultPlot5(predlist = predlist, sample = sample, models = models)
ggsave('./Essay/Pictures/WohngegendAlleModelle2.pdf', height = 8, width = 8)