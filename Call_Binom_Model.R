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

load_model <- TRUE
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
plot(step.model.binom$model.spat, all = T)

#--------------------------------------#
# Bezirke als Räumliche Informationen  #-----------------------------------------------------------------
#--------------------------------------#

# Erstellen des Markov-Random fields
bezirke@data$id <- rownames(bezirke@data)
helpdf <- fortify(bezirke, region = "id")
bb <- merge(helpdf, bezirke@data, by = 'id', all.x = T)
bb2 <- select(bb, long, lat, STADTBEZIR)
bb3 <- list()
for(i in levels(factor(bb2$STADTBEZIR))) {
  bb3[[i]] <- bb2[bb2$STADTBEZIR == i, c('long', 'lat')]
}
zt <- list(polys = bb3)

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

# Extrahieren der räumlichen Informationen der Stadtbezirke
stadtteile@data$id <- rownames(stadtteile@data)
helpdf <- fortify(stadtteile, region = "id")
bb <- merge(helpdf, stadtteile@data, by = 'id', all.x = T)
bb2 <- select(bb, long, lat, STADTTEIL)

# Listenstruktur erstellen für Makrov random field im spline
bb3 <- list()
for(i in levels(factor(bb2$STADTTEIL))) {
  bb3[[i]] <- bb2[bb2$STADTTEIL == i, c('long', 'lat')]
  
}
zt <- list(polys = bb3)

# Problem ist, dass die GAM funktion immer mindestens eine Beobachtung für jede Kategorie in jedem Polygon erwaretet
# Darum ist die Regression mit 5 Kategorien praktisch nicht möglich. Aber auch mit 3 Kategorien funktioniert es so nicht.
# Lösung: Pseudobeobachtung erstellen und danach mit 0 gewichten.
dataS <- sample
m <- as.data.frame(table(dataS$Stadtteil, dataS$Meinung.zu.Stuttgart.21)) # Um zu sehen wo Beobachtungen fehlen
fehlende.b <- filter(m, Freq == 0) # Stadtteile in denen Beobachtungen fehlen

# Weiteres Problem: Einige Stadtteile wie z.B. der Wald im Westen haben gar keine Beobachtungen.
# Darum weitere Pseudobeobachtungen
stadtteile.pol <- as.data.frame(table(bb2$STADTTEIL)) # Alle Stadtteile mit räumlichen Informationen
stadtteile.dat <- as.data.frame(table(dataS$Stadtteil)) # Alle Stadtteile mit Beobachtungen
names(stadtteile.dat) <- c('Var1', 'Freq2')
unterschied <- merge(stadtteile.pol, stadtteile.dat, all.x = T)
unterschied <- filter(unterschied, is.na(Freq2)) # Leere Polygone wie z.B. der Wald
u <- c(rep(as.character(unterschied[,1]), 2)) # jeweils eine Beobachtung jeder Kategorie erstellen
# erstellen der Pseudobeobachtungen für Stadtteile mit gar keinen Beobachtungen
MM <- c(rep(1, nrow(unterschied)), rep(0, nrow(unterschied)))
P <- c(rep(1,length(u)))
M <- c(rep(1,length(u)))
A <- c(rep(1,length(u)))
B <- c(rep(1,length(u)))
N <- c(rep('Deutsch',length(u)))
G <- c(rep('Männlich' ,length(u)))  
FF <- c(rep('ledig',length(u)))
Sb <- c(rep('Mitte', length(u)))
X <- c(rep(3513518,length(u)))
Y <- c(rep(404074, length(u)))
D <- c(rep(0, length(u)))
pseudo.a <- as.data.frame(cbind(B, MM, P, M, A, G, FF, N, Sb, as.character(u), X, Y))
# Gewichte einführen, um bias zu verhindern
pseudo.a$Gewicht  <- 0
names(pseudo.a)<- names(dataS)
# erstellen der Pseudobeobachtungen für Stadtteile mit parziel fehlenden Beobachtungen
pseudo <- as.data.frame(cbind(fehlende.b$Var2, as.character(fehlende.b$Var1)))
P <- c(rep(1,nrow(pseudo)))
M <- c(rep(1,nrow(pseudo)))
A <- c(rep(1,nrow(pseudo)))
B <- c(rep(1,nrow(pseudo)))
N <- c( rep('Deutsch',nrow(pseudo)))
G <- c( rep('Männlich' ,nrow(pseudo)))  
FF <- c( rep('ledig',nrow(pseudo)))
Sb <- c(rep('Mitte', nrow(pseudo)))
X <- c( rep(3513518,nrow(pseudo)))
Y <- c(rep(404074, nrow(pseudo)))
D <- c(rep(0, nrow(pseudo)))
pseudo.b <- as.data.frame(cbind(B, B, P, M, A, G, FF, N, Sb, as.character(fehlende.b$Var1), X, Y))

# Pseudo Beobachtungen mit 0 gewichten
pseudo.b$Gewicht  <- 0
names(pseudo.b) <- names(dataS)
# Echte Beobachtungen mit 1 gewichten
dat.teile <- dataS
dat.teile$Gewicht <- 1
names(pseudo.a)<- names(dat.teile)
names(pseudo.b)<- names(dat.teile)
# Zusammmenfügen von echten und pseudo Beobachtungen
dat.teile <- rbind(dat.teile, pseudo.b, pseudo.a)
dat.teile <- dat.teile[order(dat.teile$Stadtteil),]

# Einige Variablen sind fälschlicherweise als Character gespeichert
dat.teile$Bewertung.Wohngegend <- as.integer(dat.teile$Bewertung.Wohngegend)
dat.teile$Personenzahl.im.Haushalt <- as.integer(dat.teile$Personenzahl.im.Haushalt)
dat.teile$Monatliches.Netto.Haushaltseinkommen <- as.integer(dat.teile$Monatliches.Netto.Haushaltseinkommen)
dat.teile$Altersklasse.Befragter <- as.integer(dat.teile$Altersklasse.Befragter)
dat.teile$Meinung.zu.Stuttgart.21 <- as.numeric(dat.teile$Meinung.zu.Stuttgart.21)

# Angleichen der Factor levels der Daten und des Markov random fields
dat.teile$Stadtteil <- factor(dat.teile$Stadtteil, levels = names(zt$polys))

# Neue raeumliche Information, der rest bleibt gleich
fixed <- "s(Stadtteil, bs=\"mrf\", xt = zt) + s(Personenzahl.im.Haushalt, Altersklasse.Befragter, bs= \"tp\")"
# Sample mit pseudo Beobachtungen einfügen
sample <- dat.teile

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

