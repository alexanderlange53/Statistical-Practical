#------------------------------------------------------#
#### Plausibilitätskontrolle der Intervall Funktion ####
#------------------------------------------------------#

#----------------#
## 3 Kategorien ##
#----------------#

bearbeiter <- 'Kai@Home'

## Daten einlesen ##
if(bearbeiter == 'Kai@Home') {
  intervall <- read.csv2('Boot_Results/S21_3_U_Ko_IntSB.csv', as.is= TRUE)
  pred_agg <- read.csv2('Prediction_Results/S21_3_U_Ko_AggSB.csv', as.is = TRUE)
}


## Plausibilitätschecks ##

# Sind in beiden Dateien alle Bezirke vorhanden?
all(intervall$Stadtbezirk == pred_agg$Stadtbezirk)

# Ist die Gesamtzahl der Beobachtungen je Bezirk gleich? (Runden wegen der bekannten R Ungenauigkeit beim '==' Vergleich)
all(round(apply(pred_agg[, c(2 : 4)], 1, sum)) == round(apply(intervall[, c(8 : 10)], 1, sum)))

# Anteile bilden, die diese besser vergleichbar sind als die absoluten Zahlen
anteile_pred_agg <- data.frame(Stadtbezirk = pred_agg$Stadtbezirk, V1 = NA, V2 = NA, V3 = NA)
anteile_int_mean <- data.frame(Stadtbezirk = pred_agg$Stadtbezirk, V1 = NA, V2 = NA, V3 = NA)
for(i in c (2 : 4)) {
  anteile_pred_agg[, i] <- pred_agg[, i] / apply(pred_agg[, c(2 : 4)], 1, sum)
  anteile_int_mean[, i] <- intervall[, i + 6] / apply(intervall[, c(8 : 10)], 1, sum)
}

# Modellparameter - Mittelwert aus boot. Sollte sehr nah aneinander liegen (falls nicht, ist dies ein hinweis darauf, dass die Parameteriserungsdaten gebiased sind)
par(mfrow = c( 2, 2))
for(i in c(2 : 4)) {
  hist(anteile_pred_agg[, i] - anteile_int_mean[, i])
}
# Es gibt tatsächlich offenbar einen Bias. Der ist aber zum Glück sehr sehr klein = mehr oder weniger Zufall


for(i in c(2 : 4)) {
  print(i) # Kategorie Nr.
  print(all(intervall[, i] < intervall[, i + 3])) # obere Int. immer größer als untere?
  print(all(intervall[, i + 6] > intervall[, i]) && all(intervall[, i + 6] < intervall[, i + 3])) # mean immer zwischen den Int.?
  print(all(intervall[, i + 9] > intervall[, i]) && all(intervall[, i + 9] < intervall[, i + 3])) # median immer zwischen den Int.?
}


#----------------#
## 5 Kategorien ##
#----------------#
intervall <- Int.U.ST.ST
pred_agg <- AggPred.U.S.ST

# Sind in beiden Dateien alle Bezirke vorhanden?
if(names(intervall)[1] == 'Stadtteil') {
  print(all(intervall$Stadtteil == pred_agg$Stadtteil))
}

# Ist die Gesamtzahl der Beobachtungen je Bezirk gleich? (Runden wegen der bekannten R Ungenauigkeit beim '==' Vergleich)
if(names(intervall)[1] == 'Stadtteil') {
  print(all(round(apply(pred_agg[, c(2 : 6)], 1, sum)) == round(apply(intervall[, c(12 : 16)], 1, sum))))
}

# Anteile bilden, die diese besser vergleichbar sind als die absoluten Zahlen
anteile_pred_agg <- data.frame(Stadtteil = pred_agg$Stadtteil, V1 = NA, V2 = NA, V3 = NA, V4 = NA, V5 = NA)
anteile_int_mean <- data.frame(Stadtteil = pred_agg$Stadtteil, V1 = NA, V2 = NA, V3 = NA, V4 = NA, V5 = NA)
for(i in c (2 : 6)) {
  anteile_pred_agg[, i] <- pred_agg[, i] / apply(pred_agg[, c(2 : 6)], 1, sum)
  anteile_int_mean[, i] <- intervall[, i + 6] / apply(intervall[, c(12 : 16)], 1, sum)
}

# Modellparameter - Mittelwert aus boot. Sollte sehr nah aneinander liegen (falls nicht, ist dies ein hinweis darauf, dass die Parameteriserungsdaten gebiased sind)
par(mfrow = c( 2, 3))
for(i in c(2 : 6)) {
  hist(anteile_pred_agg[, i] - anteile_int_mean[, i])
}
# Es gibt tatsächlich offenbar einen Bias. Der ist aber zum Glück sehr sehr klein = mehr oder weniger Zufall


for(i in c(2 : 6)) {
  print(i) # Kategorie Nr.
  print(all(intervall[, i] < intervall[, i + 5])) # obere Int. immer größer als untere?
  print(all(intervall[, i + 10] > intervall[, i]) && all(intervall[, i + 10] < intervall[, i + 5])) # mean immer zwischen den Int.?
  print(all(intervall[, i + 15] > intervall[, i]) && all(intervall[, i + 15] < intervall[, i + 5])) # median immer zwischen den Int.?
}

