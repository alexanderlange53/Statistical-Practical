#####################################
# Estimation of Stuttgart 21 survey #
#####################################

rm(list=ls())
# loading packages
require('ROCR')
require('mgcv')
require('splines')
require(rgdal);require(rgeos)
require(ggplot2)
require(maptools);require(rvest);require(dplyr)

bearbeiter = 'Kai@Home'
# loading data
if(bearbeiter == 'Alex'){
  dataS <- read.csv2('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Auswertung/Neue_Daten/Stuttgart21_aufbereitet.csv',
                     dec = '.')
  bezirke <- readOGR(dsn = "/home/alex/Schreibtisch/Uni/statistisches_praktikum/Auswertung/Geodaten/bezirke", layer = "bezirke")
}

if(bearbeiter == 'Kai@Home'){
  dataS <- read.csv2('/home/kai/Dokumente/Master/Stat_Practical/Statistical-Practical/Rohdaten/Neue_Daten/Stuttgart21_aufbereitet.csv',
                     dec = '.')
  bezirke <- readOGR(dsn = "/home/kai/Dokumente/Master/Stat_Practical/Statistical-Practical/Rohdaten/Geodaten/bezirke/", layer = "bezirke")
}

if(bearbeiter == 'Kai@Work') {
  dataS <- read.csv2('/home/khusmann/mnt/U/Promotion/Kurse/Stat_Praktikum/Auswertung/Neue_Daten/Stuttgart21_aufbereitet.csv',
                     dec = '.')
  bezirke <- readOGR(dsn = "/home/khusmann/mnt/U/Promotion/Kurse/Stat_Praktikum/Auswertung/Geodaten/bezirke/", layer = "bezirke")
}



# Hilfsfunktionen
source('./erster_test_functions.R')

#--------------------------------# Schätzung mit stetiger räumlicher Variable #-------------------------------------------#

# Da die Kategorie 'Keine Angabe' nicht in das Schema der geordneten Kategorien passt und keine Informationen
# enthält wirde sie entfernt.
for(i in 1:nrow(dataS)){
if(dataS$Meinung.zu.Stuttgart.21[i] == 6){
  dataS$Meinung.zu.Stuttgart.21[i] <- NA
}}
dataS <- na.omit(dataS)

## gam parameter ##
# Zielgroesse & Verteilungsannahme
response <- "Meinung.zu.Stuttgart.21"
verteilung <- ocat(R=5)

# raeumlicher Effekt
seff <- "s(X, Y, bs=\"tp\")"

# Parametrisch zu modellierende Kovariablen
pars <- c("Familienstand", "Nationalität", "Geschlecht")

# Potenziell nichtparametrisch zu modellierende Kovariablen (Erstmal ohne Einkommen)
nonpars <- c("Altersklasse.Befragter, k = 6","Personenzahl.im.Haushalt, k = 5")

# Erstellen der Schätzfunktion
formel <- make.formula(response = response, fixed = seff, pars = pars, nonpars = nonpars)

# GAM Schätzung
model1 <- gam(formel, data = dataS, family= verteilung, method = 'REML')
summary(model1)
plot(model1, pages = 1)
gam.check(model1)
model1$family$getTheta(TRUE)

saveRDS(model1, 'model1.rds')

#-------------------------------------------------------------#
# Schätzung mit transformierter response Variable (3 Klassen) #
#-------------------------------------------------------------#

# Zusammenführen von 'Sehr gut' und 'Gut' zu Zustimmung und 'Sehr Schlecht' und 'Schlecht' zu Ablehnend 
for(i in 1:nrow(dataS)){
  if(dataS$Meinung.zu.Stuttgart.21[i] == 2){
    dataS$Meinung.zu.Stuttgart.21[i] <- 1
  }}
for(i in 1:nrow(dataS)){
  if(dataS$Meinung.zu.Stuttgart.21[i] == 3){
    dataS$Meinung.zu.Stuttgart.21[i] <- 2
  }}
for(i in 1:nrow(dataS)){
  if(dataS$Meinung.zu.Stuttgart.21[i] == 4){
    dataS$Meinung.zu.Stuttgart.21[i] <- 3
  }}
for(i in 1:nrow(dataS)){
  if(dataS$Meinung.zu.Stuttgart.21[i] == 5){
    dataS$Meinung.zu.Stuttgart.21[i] <- 3
  }}

# Nur noch drei Kategorien bei response
verteilung <- ocat(R=3)

# Erstellen der Schätzfunktion
formel <- make.formula(response = response, fixed = seff, pars = pars, nonpars = nonpars)

# GAM Schätzung
model2 <- gam(formel, data = dataS, family= verteilung, method = 'REML')
summary(model2)
plot(model2, pages = 1)
gam.check(model2)
model2$family$getTheta(TRUE)

saveRDS(model2, 'model2.rds')

#---------------------------------# Schätzung mit diskreter räumlicher information #---------------------------------------#

#-----------------------------------------#
# Bezirke als Informationen und 5 Klassen #
#-----------------------------------------#

bearbeiter = 'Alex'
# loading data
if(bearbeiter == 'Alex'){
  dataS <- read.csv2('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Auswertung/Neue_Daten/Stuttgart21_aufbereitet.csv',
                     dec = '.')
  bezirke <- readOGR(dsn = "/home/alex/Schreibtisch/Uni/statistisches_praktikum/Auswertung/Geodaten/bezirke", layer = "bezirke")
} else {
  dataS <- read.csv2('/home/khusmann/mnt/U/Promotion/Kurse/Stat_Praktikum/Auswertung/Neue_Daten/Stuttgart21_aufbereitet.csv',
                     dec = '.')
  bezirke <- readOGR(dsn = "/home/khusmann/mnt/U/Promotion/Kurse/Stat_Praktikum/Auswertung/Geodaten/bezirke/", layer = "bezirke")
  
}

# Da die Kategorie 'Keine Angabe' nicht in das Schema der geordneten Kategorien passt und keine Informationen
# enthält wirde sie entfernt.
for(i in 1:nrow(dataS)){
  if(dataS$Meinung.zu.Stuttgart.21[i] == 6){
    dataS$Meinung.zu.Stuttgart.21[i] <- NA
  }}
dataS <- na.omit(dataS)

## gam parameter ##
# Zielgroesse & Verteilungsannahme
response <- "Meinung.zu.Stuttgart.21"
verteilung <- ocat(R=5)

# Extrahieren der räumlichen Informationen der Stadtbezirke
bezirke@data$id <- rownames(bezirke@data)
helpdf <- fortify(bezirke, region = "id")
bb <- merge(helpdf, bezirke@data, by = 'id', all.x = T)
bb2 <- select(bb, long, lat, STADTBEZIR)
bb3 <- list()
for(i in levels(factor(bb2$STADTBEZIR))) {
  bb3[[i]] <- bb2[bb2$STADTBEZIR == i, c('long', 'lat')]
  
}
zt <- list(polys = bb3)

# raeumlicher Effekt
seff <- "s(Stadtbezirk, bs=\"mrf\", xt = zt)"

# Parametrisch zu modellierende Kovariablen
pars <- c("Familienstand", "Nationalität", "Geschlecht")

# Potenziell nichtparametrisch zu modellierende Kovariablen
nonpars <- c("Altersklasse.Befragter, k = 6","Personenzahl.im.Haushalt, k = 5","Monatliches.Netto.Haushaltseinkommen, k = 6")

# Erstellen der Schätzfunktion
formel <- make.formula(response = response, fixed = seff, pars = pars, nonpars = nonpars)

# GAM Schätzung
model3 <- gam(formel, data = dataS, family= verteilung, method = 'REML')
summary(model3)
plot(model3, pages = 1)
gam.check(model3)
model3$family$getTheta(TRUE)

saveRDS(model3, 'model3.rds')

#-----------------------------------------#
# Bezirke als Informationen und 3 Klassen #
#-----------------------------------------#

# Zusammenführen von 'Sehr gut' und 'Gut' zu Zustimmung und 'Sehr Schlecht' und 'Schlecht' zu Ablehnend 
for(i in 1:nrow(dataS)){
  if(dataS$Meinung.zu.Stuttgart.21[i] == 2){
    dataS$Meinung.zu.Stuttgart.21[i] <- 1
  }}
for(i in 1:nrow(dataS)){
  if(dataS$Meinung.zu.Stuttgart.21[i] == 3){
    dataS$Meinung.zu.Stuttgart.21[i] <- 2
  }}
for(i in 1:nrow(dataS)){
  if(dataS$Meinung.zu.Stuttgart.21[i] == 4){
    dataS$Meinung.zu.Stuttgart.21[i] <- 3
  }}
for(i in 1:nrow(dataS)){
  if(dataS$Meinung.zu.Stuttgart.21[i] == 5){
    dataS$Meinung.zu.Stuttgart.21[i] <- 3
  }}


## gam parameter ##
# Zielgroesse & Verteilungsannahme
response <- "Meinung.zu.Stuttgart.21"
verteilung <- ocat(R=3)

# Extrahieren der räumlichen Informationen der Stadtbezirke
bezirke@data$id <- rownames(bezirke@data)
helpdf <- fortify(bezirke, region = "id")
bb <- merge(helpdf, bezirke@data, by = 'id', all.x = T)
bb2 <- select(bb, long, lat, STADTBEZIR)
bb3 <- list()
for(i in levels(factor(bb2$STADTBEZIR))) {
  bb3[[i]] <- bb2[bb2$STADTBEZIR == i, c('long', 'lat')]
  
}
zt <- list(polys = bb3)

# raeumlicher Effekt
seff <- "s(Stadtbezirk, bs=\"mrf\", xt = zt)"

# Parametrisch zu modellierende Kovariablen
pars <- c("Familienstand", "Nationalität", "Geschlecht")

# Potenziell nichtparametrisch zu modellierende Kovariablen
nonpars <- c("Altersklasse.Befragter, k = 6","Personenzahl.im.Haushalt, k = 5","Monatliches.Netto.Haushaltseinkommen, k = 6")

# Erstellen der Schätzfunktion
formel <- make.formula(response = response, fixed = seff, pars = pars, nonpars = nonpars)

# GAM Schätzung
model4 <- gam(formel, data = dataS, family= verteilung, method = 'REML')
summary(model4)
plot(model4, pages = 1)
gam.check(model4)
model4$family$getTheta(TRUE)

saveRDS(model4, 'model4.rds')

#--------------------------------------------------------------------------#
# Schätzung mit 3 Kategorien und den Stadtteilen als räumliche Information #
#--------------------------------------------------------------------------#

bearbeiter = 'Alex'
# laden der Stadtteilinformationen
if(bearbeiter == 'Alex'){
  dataS <- read.csv2('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Auswertung/Neue_Daten/Stuttgart21_aufbereitet.csv',
                     dec = '.')
  stadtteile <- readOGR(dsn = "/home/alex/Schreibtisch/Uni/statistisches_praktikum/Daten_Kneib/Stick/Daten_Kneib/Stadtteile_netto", layer = "Stadtteile_netto")
} else {
  dataS <- read.csv2('/home/khusmann/mnt/U/Promotion/Kurse/Stat_Praktikum/Auswertung/Neue_Daten/Stuttgart21_aufbereitet.csv',
                     dec = '.')
  stadtteile <- readOGR(dsn = "/home/khusmann/mnt/U/Promotion/Kurse/Stat_Praktikum/Auswertung/Geodaten/bezirke/", layer = "bezirke")
}

# Da die Kategorie 'Keine Angabe' nicht in das Schema der geordneten Kategorien passt und keine Informationen
# enthält wirde sie entfernt.
for(i in 1:nrow(dataS)){
  if(dataS$Meinung.zu.Stuttgart.21[i] == 6){
    dataS$Meinung.zu.Stuttgart.21[i] <- NA
  }}
dataS <- na.omit(dataS)

# Zusammenführen von 'Sehr gut' und 'Gut' zu Zustimmung und 'Sehr Schlecht' und 'Schlecht' zu Ablehnend 
for(i in 1:nrow(dataS)){
  if(dataS$Meinung.zu.Stuttgart.21[i] == 2){
    dataS$Meinung.zu.Stuttgart.21[i] <- 1
  }}
for(i in 1:nrow(dataS)){
  if(dataS$Meinung.zu.Stuttgart.21[i] == 3){
    dataS$Meinung.zu.Stuttgart.21[i] <- 2
  }}
for(i in 1:nrow(dataS)){
  if(dataS$Meinung.zu.Stuttgart.21[i] == 4){
    dataS$Meinung.zu.Stuttgart.21[i] <- 3
  }}
for(i in 1:nrow(dataS)){
  if(dataS$Meinung.zu.Stuttgart.21[i] == 5){
    dataS$Meinung.zu.Stuttgart.21[i] <- 3
  }}

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

m <- as.data.frame(table(dataS$Stadtteil, dataS$Meinung.zu.Stuttgart.21)) # Um zu sehen wo Beobachtungen fehlen
fehlende.b <- filter(m, Freq == 0) # Stadtteile in denen Beobachtungen fehlen

# Weiteres Problem: Einige Stadtteile wie z.B. der Wald im Westen haben gar keine Beobachtungen.
# Darum weitere Pseudobeobachtungen
stadtteile.pol <- as.data.frame(table(bb2$STADTTEIL)) # Alle Stadtteile mit räumlichen Informationen
stadtteile.dat <- as.data.frame(table(dataS$Stadtteil)) # Alle Stadtteile mit Beobachtungen
names(stadtteile.dat) <- c('Var1', 'Freq2')
unterschied <- merge(stadtteile.pol, stadtteile.dat, all.x = T)
unterschied <- filter(unterschied, is.na(Freq2)) # Leere Polygone wie z.B. der Wald
u <- c(rep(as.character(unterschied[,1]), 3)) # jeweils eine Beobachtung jeder Kategorie erstellen
# erstellen der Pseudobeobachtungen für Stadtteile mit gar keinen Beobachtungen
MM <- c(rep(1, nrow(unterschied)), rep(2, nrow(unterschied)), rep(3, nrow(unterschied)))
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
names(pseudo.a)<- names(dataS)
# Gewichte einführen, um bias zu verhindern
pseudo.a$Gewicht  <- 0
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
pseudo.b <- as.data.frame(cbind(B, fehlende.b$Var2, P, M, A, G, FF, N, Sb, as.character(fehlende.b$Var1), X, Y))
names(pseudo.b)<- names(dataS)
# Pseudo Beobachtungen mit 0 gewichten
pseudo.b$Gewicht  <- 0
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

response <- "Meinung.zu.Stuttgart.21"
verteilung <- ocat(R=3)
# raeumlicher Effekt
seff <- "s(Stadtteil, bs=\"mrf\", xt = zt)"

# Parametrisch zu modellierende Kovariablen
pars <- c("Familienstand", "Nationalität", "Geschlecht")

# Zeigt die Anzahl der klassen der Variablen. Knots vom spline sind by default = 10, wenn die Anzahl der Klassen aber weniger 
# als 10 sind, kann dies zu Problemen führen.
unique(dat.teile$Altersklasse.Befragter)
unique(dat.teile$Personenzahl.im.Haushalt)
unique(dat.teile$Monatliches.Netto.Haushaltseinkommen)

# Potenziell nichtparametrisch zu modellierende Kovariablen
nonpars <- c("Altersklasse.Befragter, k = 6","Personenzahl.im.Haushalt, k= 5","Monatliches.Netto.Haushaltseinkommen, k = 6")

# Erstellen der Schätzfunktion
formel <- make.formula(response = response, fixed = seff, pars = pars, nonpars = nonpars)

# GAM Schätzung
model5 <- gam(formel,  weights = dat.teile$Gewicht, data = dat.teile, family= verteilung, method = 'REML')
summary(model5)
plot(model5, pages = 1)
gam.check(model5)
model5$family$getTheta(TRUE)

saveRDS(model5, 'model5.rds')
