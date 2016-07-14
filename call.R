#------------------------------------------------#
#### Laden der Daten und Aufrufen der Skripte ####
#------------------------------------------------#

# Erstellt: 07.07.16
# Aktualisiert: 07.07.16
# Letzter Bearbeiter: Kai

rm(list = ls())

## Working directory ##

bearbeiter = 'Kai@Home'

if(bearbeiter == 'Alex') {
  setwd('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Presi/Statistical-Practical')
  sample <- read.table("/home/alex/Schreibtisch/Uni/statistisches_praktikum/Auswertung/Neue_Daten/Stuttgart21_aufbereitet.csv", header=TRUE, sep=";")
} 
if(bearbeiter == 'Kai@Work') {
  setwd('/home/khusmann/mnt/U/Promotion/Kurse/Stat_Praktikum/Praesentation1_06062016/Statistical-Practical/')
}
if(bearbeiter == 'Kai@Home') {
  setwd('/home/kai/Dokumente/Master/Stat_Practical/Statistical-Practical/')
}


source("stepAIC.R")
source("evaluation.R")
#source("prediction.R")

library("ROCR")
library("mgcv")
library("splines")

#--------------------------------#
# Daten einlesen und vorbereiten #
#--------------------------------#

sample <- read.table("./Rohdaten/buergerumfrage_neu/Stuttgart21_aufbereitet.csv", header=TRUE, sep=";")

for(i in 1:nrow(sample)){
  if(sample$Meinung.zu.Stuttgart.21[i] == 6) {
      sample$Meinung.zu.Stuttgart.21[i] <- NA
    }
}

sample <- na.omit(sample)

for(i in 1:nrow(sample)){
  if(sample$Meinung.zu.Stuttgart.21[i] == 2){
    sample$Meinung.zu.Stuttgart.21[i] <- 1
  }
}
for(i in 1:nrow(sample)){
  if(sample$Meinung.zu.Stuttgart.21[i] == 3){
    sample$Meinung.zu.Stuttgart.21[i] <- 2
    }
}
for(i in 1:nrow(sample)){
  if(sample$Meinung.zu.Stuttgart.21[i] == 4){
    sample$Meinung.zu.Stuttgart.21[i] <- 3
  }
}
for(i in 1:nrow(sample)){
  if(sample$Meinung.zu.Stuttgart.21[i] == 5){
    sample$Meinung.zu.Stuttgart.21[i] <- 3
  }
}


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
#+ s(Personenzahl.im.Haushalt, Altersklasse.Befragter, bs= \"tp\")"

# Parametrisch zu modellierende Kovariablen
pars <- c("Familienstand", "Nationalität", "Geschlecht")

# Potenziell nichtparametrisch zu modellierende Kovariablen
nonpars <- c("Altersklasse.Befragter","Personenzahl.im.Haushalt")
# Dieser Teil kann von uns nicht uebernommen werden, da keine Population existiert
# # Vorhersagedatensatz (Informationen aus der Grundgesamtheit) und
# # zu verwendende r?umliche Aggregationsebene in der Vorhersage
# # Kodierung der Variablen analog zur Stichprobe
# population <- read.table("population_aufbereitet.txt", header=TRUE, sep=";")
# aggregation <- "Stadtteil"
# 
# # Validierungsdaten, falls vorhanden
# validate <- TRUE
# validation <- read.table("validation_aufbereitet.txt", header=TRUE, sep=";")
# aggregation.val <- "Stadtteil"
# response.val <- "Eigentuemer.Zensus"
# validation <- validation[order(validation[,aggregation.val]),]

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


## Step AIC ##
step.model <- stepAIC()
# 07.07: Lueppt. hat aber noch das Problem, dass die Knoten nicht angegeben werden koennen! Koennte man abfangen, indem die make.formula angepasst wird (liegt am ,)
# Diese Warnung sollte aber auch nichts ausmachen bei pen. B-Splines

AIC(step.model$model.spat)
AIC(step.model$model.nospat)
AIC(step.model$model.spatonly)

summary(step.model$model.spat)
plot(step.model$model.spat, all = T)
## lauft bis hier


## bis hier 
saveRDS(step.model$model.spat, file="step.model.rds")

evaluate(step.model)

#save(step.model, file="step.model.Rdata")


#load("step.model.Rdata")
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






# Ergebnisse:
# pred = Matrix mit den Vorhersagen und Vorhersageintervallen (falls berechnet)
#        wird auch in der Datei "response_vorhersage.txt" gespeichert
# step.model = Liste mit den Elementen
#     model.spat = Finales Modell der schrittweisen Modellselektiom
#     model.nospat = Wie model.spat aber ohne r?umlichem Effekt
#     model.spatonly = Modell mit ausschlie?lich r?umlichem Effekt
#     pars = ausgew?hlte parametrische Effekte
#     nonpars = ausgew?hlte nichtparametrische Effekte
#     fm = Formel des finalen Modells
# wird auch in step.model.Rdata gespeichert