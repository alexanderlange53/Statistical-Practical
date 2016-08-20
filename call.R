#------------------------------------------------#
#### Laden der Daten und Aufrufen der Skripte ####
#------------------------------------------------#

rm(list = ls())

## Working directory ##

bearbeiter <- 'Alex'
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
}

source("stepAIC.R")
source("evaluation.R")
source('DataPrep.R')
source('MarkovRandomField.R')
source('PseudoB.R')
source("Prediction.R")

library("ROCR")
library("mgcv")
library("splines")
require(rgdal);require(rgeos)
require(ggplot2)
require(maptools);require(rvest);require(dplyr)
library(ggplot2)


#--------------------------------#
# Daten einlesen und vorbereiten #
#--------------------------------#

# Wenn binom = F:
# erstellt aus Gruppen 1 und 2 = 1
# erstellt aus Gruppen 3 = 2
# erstellt aus Grppen 4 und 5 = 3
# löscht Gruppe 6
sample <- DataPrep(sample, binom = F)

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
parallel <- FALSE
ncore <- 20
seed <- 123

#--------------------#
## Modellerstellung ##
#--------------------#

load_model <- TRUE
## Step AIC ##
if(!load_model){
  step.model <- stepAIC()
  saveRDS(step.model$model.spat, file="step.model.rds")
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

summary(step.model$model.spat)

#--------------------#
## Model Evaluation ##
#--------------------#

evaluate(step.model$model.spat, data = sample)
cross.evaluation(model = step.model$model.spat, data = sample, repeatitions = 5)

#---------------#
## Prediction  ##
#---------------#

if(pred == T){
  pred.U <- Prediction(Umfrage, step.model$model.spat, Umfrage = T, binom = F)
  pred.Z <- Prediction(Zensus, step.model$model.spat, Umfrage = F, binom = F)
  write.table(pred.U, file = 'pred_U.csv', sep=";", col.names=TRUE, row.names=FALSE, quote=FALSE)
  write.table(pred.Z, file = 'pred_Z.csv', sep=";", col.names=TRUE, row.names=FALSE, quote=FALSE)
}else{
  pred.U <- read.csv2('pred_U.csv')
  pred.Z <- read.csv2('pred_Z.csv')
}

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
  step.model.B <- stepAIC()
  saveRDS(step.model.B$model.spat, file="step.model_B.rds")
  saveRDS(step.model.B, file="step.model_all_B.rds")
} else {
  step.model.B <- readRDS(file = "step.model_all_B.rds")
}

#--------------------#
## Model Evaluation ##
#--------------------#

evaluate(step.model.B$model.spat, data = sample)
cross.evaluation(model = step.model.B$model.spat, data = sample, repeatitions = 5)

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
plot(m1, select = 6, all = TRUE, ann = F) # Familienstand
mtext(side = 1, line = 3, "Familienstand"); mtext(side = 2, line = 3, "Einfluss des Familienstands")
plot(m1, select = 7, all = TRUE, ann = F) # Personenzahl
mtext(side = 1, line = 3, "Personenzahl im Haushalt"); mtext(side = 2, line = 3, "Einfluss der Personenzahl im Haushalt")

dev.off()

AIC(step.model.B$model.spat)
AIC(step.model.B$model.nospat)
AIC(step.model.B$model.spatonly)

summary(step.model.B$model.spat)
plot(step.model.B$model.spat, all = T)

#-----------------------------------------#
# Stadtteile als Räumliche Informationen  #--------------------------------------------------------------
#-----------------------------------------#

# Erstellen des Markov Random fields
zt <- MarkovRandomField(stadtteile, Bezirke = F)

# Erstellen der Pseudo Beobachtungen und in Datensatz integrieren
sample <- PseudoB(sample, stadtteile, binom = F)

# Neue raeumliche Information, der rest bleibt gleich
fixed <- "s(Stadtteil, bs=\"mrf\", xt = zt) + s(Personenzahl.im.Haushalt, Altersklasse.Befragter, bs= \"tp\")"

#--------------------#
## Modellerstellung ##
#--------------------#

load_model <- TRUE
## Step AIC ##
if(!load_model){
  step.model.S <- stepAIC()
  saveRDS(step.model.S$model.spat, file="step.model_S.rds")
  saveRDS(step.model.S, file="step.model_all_S.rds")
} else {
  step.model.S <- readRDS(file = "step.model_all_S.rds")
}

#--------------------#
## Model Evaluation ##
#--------------------#

evaluate(step.model.S$model.spat, data = sample)
cross.evaluation(model = step.model.S$model.spat, data = sample, repeatitions = 5)

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

summary(step.model.binom.S$model.spat)
plot(step.model.binom.S$model.spat, all = T)



#--------------#
## Alter Code ##
#--------------#
# Vielleicht für das Papaer hilfreich, im Moment aber nicht wichtig

mean.erg <- erg[[1]]$tab
for(i in c(2 : 10)){
  mean.erg <- mean.erg + erg[[i]]$tab
}
mean.erg <- mean.erg / 10
mean.erg <- as.data.frame(mean.erg)
colnames(mean.erg) <- c("Dafür", "Neutral", "Dagegen")
rownames(mean.erg) <- c("Dafür", "Neutral", "Dagegen")
library(stargazer)
stargazer(mean.erg)
## lauft bis hier

# Descr. Stat: Neutral
hist(sample[sample$Meinung.zu.Stuttgart.21 == 2, "Personenzahl.im.Haushalt"])
hist(as.numeric(sample[sample$Meinung.zu.Stuttgart.21 == 2, "Geschlecht"]))
hist(sample[sample$Meinung.zu.Stuttgart.21 == 2, "Altersklasse.Befragter"])
if(parallel)
{
  set.seed(seed)
  n <- nrow(sample)
  indmat <- matrix(0, nrow=n, ncol=nboot)
  wmat <- matrix(0, nrow=n, ncol=nboot)
  for(b in 1:nboot)
  {
    indmat[,b] <- sample(1:n, size=n, replace=TRUE)
    wmat[,b] <- sample[indmat[,b],gewichte]
  }
}


pred <- prediction(step.model)
