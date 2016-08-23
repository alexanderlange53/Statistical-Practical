#------------------------------------------------#
#### Laden der Daten und Aufrufen der Skripte ####
#------------------------------------------------#

require(rgdal);require(rgeos)
require(ggplot2)
require(maptools);require(rvest);require(dplyr)
library(ggplot2)

rm(list = ls())

## Working directory ##

bearbeiter = 'Alex'
pred = F

if(bearbeiter == 'Alex') {
  setwd('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Presi/Statistical-Practical')
  sample <- read.table("/home/alex/Schreibtisch/Uni/statistisches_praktikum/Auswertung/Neue_Daten/Stuttgart21_aufbereitet.csv", header=TRUE, sep=";")
  bezirke <- readOGR(dsn = "/home/alex/Schreibtisch/Uni/statistisches_praktikum/Auswertung/Geodaten/bezirke", layer = "bezirke")
  stadtteile <- readOGR(dsn = "/home/alex/Schreibtisch/Uni/statistisches_praktikum/Daten_Kneib/Stick/Daten_Kneib/Stadtteile_netto", layer = "Stadtteile_netto")
  if(pred == T){
    Umfrage <- read.csv2('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Daten_Kneib/Stick/buergerumfrage/population_aufbereitet.txt')
    Zensus <- read.csv2('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Daten_Kneib/Stick/zensus/population_aufbereitet.txt')
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
source("evaluation.R")
source("Prediction.R")
source('PredBarPlot.R')

library("ROCR")
library("mgcv")
library("splines")
library('reshape2')

#--------------------------------#
# Daten einlesen und vorbereiten #
#--------------------------------#

# Wenn binom = T:
# Fasst die Gruppen 1 und 2 zu == 1 zusammen und
# 4 und 5 == 0
# Gruppen 6 und 3 werden gelöscht
sample2 <- DataPrep(sample, binom = F)
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

evaluate.bivariate(step.model.binom$model.spat, data = sample)

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

## Cross Evaluation ##
repeatitions = 5
model <- step.model.binom$model.spat

leave_out <- sample.int(n = dim(sample)[1], size = repeatitions)
crosseval <- data.frame(Observation.No = integer(), Observed.y = integer(), Predicted.y = integer())

for (i in c(1 : repeatitions)) {
  all <- c(1 : dim(sample)[1])
  subset_i <- all[-leave_out]
  print(paste('Model', i, 'of', repeatitions))
  gam_i <- gam(model$formula, family = model$family, method="REML", data = sample, weights = as.vector(sample[, "Gewicht"]), subset = as.vector(subset_i)) # Fit a GAM
  ret_i <- cbind(leave_out[i], sample$Meinung.zu.Stuttgart.21[leave_out[i]], predict(model, newdata = sample[leave_out[i],], type = "response")) # Compare true and estiamted y.
  crosseval <- rbind(crosseval, ret_i)
}
names(crosseval) = c("Observation.No", "Observed.y", "Predicted.Prob")
crosseval$Predicted.y <- NA; crosseval$Predicted.y[crosseval$Predicted.Prob < 0.5] <- 0; crosseval$Predicted.y[crosseval$Predicted.Prob >= 0.5] <- 1
rm(list = c("all", "subset_i", "gam_i", "ret_i"))

## Comparison with GLM
glm.model <- glm(Meinung.zu.Stuttgart.21 ~ X * Y + Personenzahl.im.Haushalt * Altersklasse.Befragter + Geschlecht + Nationalität + Familienstand, data = sample, family = binomial)
summary(glm.model)
evaluate.bivariate(glm.model, data = sample)

pred <- predict(glm.model, newdata = sample, type = "response")

#---------------#
## Prediction  ##
#---------------#

if(pred == T){
  pred.binom.U <- Prediction(Umfrage, step.model.binom$model.spat, Umfrage = T, binom = T)
  pred.binom.Z <- Prediction(Zensus, step.model.binom$model.spat, Umfrage = F, binom = T)
  write.table(pred.binom.U, file = 'predbinom_U.csv', sep=";", col.names=TRUE, row.names=FALSE, quote=FALSE)
  write.table(pred.binom.Z, file = 'predbinom_Z.csv', sep=";", col.names=TRUE, row.names=FALSE, quote=FALSE)
}else{
  pred.binom.U <- read.csv2('predbinom_U.csv')
  pred.binom.Z <- read.csv2('predbinom_Z.csv')
}
pred.binom.U2 <- pred.binom.U
pred.binom.U2$X1 <- as.numeric(as.character(pred.binom.U2$X1))
pred.binom.U2 <- tryf(pred.binom.U2)

PredBarPlot(sample2, pred.binom.U2, x = c('Zustimmung', 'Neutral', 'Ablehnung'))
PredBarPlot(sample, pred.binom.U)
PredBarPlot(sample, pred.binom.Z)

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

evaluate.bivariate(step.model.binom.B$model.spat, data = sample)
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

evaluate.bivariate(step.model.binom.S$model.spat, data = sample)
