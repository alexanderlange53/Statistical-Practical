#------------------------------------------------#
#### Laden der Daten und Aufrufen der Skripte ####
#------------------------------------------------#

# Erstellt: 07.07.16
# Aktualisiert: 07.07.16
# Letzter Bearbeiter: Kai

rm(list = ls())

## Working directory ##

bearbeiter = 'Kai@Work'

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

load_model <- TRUE
## Step AIC ##
if(!load_model){
  step.model <- stepAIC()
  saveRDS(step.model$model.spat, file="step.model.rds")
  saveRDS(step.model, file="step.model_all.rds")
} else {
  step.model <- readRDS(file = "step.model_all.rds")
}


## GAM Plots: Interpretation der Effekte ##
m1 <- step.model$model.spat

plot(m1, select = 1, all = TRUE, ylab = "GK Hochwert", xlab = "GK Rechtswert") # Cont. spat. effect

plot(m1, select = 3, all = TRUE, ylab = "s(Altersklasse)", xlab = "Altersklasse") # Alter

pdf("./final_Presentation/parametrische_effekte.pdf", w = 9, h = 9)
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
plot(step.model$model.spat, all = T)

evaluation.in(step.model$model.spat)
evaluation.in(step.model$model.nospat)
evaluation.in(step.model$model.spatonly)

erg <- list()
for(i in c(1 : 10)) {
eval_subset <- sample_n(sample, size = 2450)
eval.model <- list("model.spat" =
  gam(Meinung.zu.Stuttgart.21 ~ s(X, Y, bs = "tp") + s(Personenzahl.im.Haushalt, Altersklasse.Befragter, bs = "tp") + 
      Geschlecht + Nationalität + Familienstand + Personenzahl.im.Haushalt + s(Altersklasse.Befragter, bs = "ps"), 
    family=verteilung, method="REML", data = eval_subset)
  )


  erg[[i]] <- evaluation.in(eval.model$model.spat, data = sample)
}

# Es spielt fast keine Rolle, ob man einen reduzierten oder den gesamten Datensatz nimmmt
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