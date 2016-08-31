#------------------------------------------------------------------------------------#
#### Hinzufuegen der diskreten Geoinformationen an Zensus und Bürgerumfrage Daten ####
#------------------------------------------------------------------------------------#

# Den Beobachtungen in den Zensus und Bürgerumfrage Daten Stadtteile und Bezirke sind vor allem am Rand
# z. T. andere Stadtteile und Bezirke zugeordnet als den Beobachtungen in Stuttgart21 Datei. Außerdem
# sind es bei der Stuttgart 21 Datei 3-Stellige Codes, bei den andern Dateien 4-stellige. Daher wurden 
# die diskreten räumlichen Informationen des 'Stadtteile_netto.shp' (welches sich mit der Lage der 
# Stuttgart 21 Datei 100 % deckt) per Union mit QGIS an die Bürgerumfrage und Zensus Daten angehängt.

# in diesem Skript werden die Geodateien eingelesen und die diskreten räumlichen Informationen an die 
# Rohdaten angehängt.

#------------------#
## Initialisieren ##
#------------------#
rm(list = ls())
library(foreign)
#setwd('/home/khusmann/mnt/U/Promotion/Kurse/Stat_Praktikum/Praesentation1_06062016/Statistical-Practical/Rohdaten/')
setwd('/home/kai/Dokumente/Master/Stat_Practical/Statistical-Practical/Rohdaten/')

#----------------------#
## Einlesen der Daten ##
#----------------------#
bu_geo <- read.dbf('./Geodaten/Buergerumfrage/Buergerumfrage_inkl_Stadtteile_netto/Buergerumfrage_Stadtteile.dbf', as.is = TRUE)
ze_geo <- read.dbf('./Geodaten/Zensus/Zensus_inkl_Stadtteile_netto/Zensus_Stadtteile.dbf', as.is = TRUE)

shape <- read.dbf('./Geodaten/Stadtteile_Shapefile/Stadtteile_netto.dbf', as.is = TRUE)

bu <- read.csv2('./buergerumfrage/population_aufbereitet.txt', as.is = TRUE)
ze <- read.csv2('./zensus/population_aufbereitet.txt', as.is = TRUE)

s21 <- read.csv2('./buergerumfrage_neu/Stuttgart21_aufbereitet.csv', as.is = TRUE)

#------------------------------#
## Schlüsseltabelle erstellen ##
#------------------------------#
key <- shape[order(shape$STADTBEZ_1), c('STADTBEZIR', 'STADTBEZ_1', 'STADTTEIL', 'STADTTEILN')]
names(key) <- c('Stadtbezirk.chr', 'Stadtbezirk.int', 'Stadtteil.chr', 'Stadtteil.int')

#-------------------------------#
## Datenvorbereitung und Merge ##
#-------------------------------#
key$Stadtteil.chr[key$Stadtteil.chr == 'Lemberg/Föhrich'] <- 'Lemberg-Föhrich' # Umbenennen, da es in den S21 Daten anders geschrieben wird
s21$Stadtteil[s21$Stadtteil == 'Klettplatz'] <- 'Hauptbahnhof' # In der Shapefile Datei wird nicht zwischenKlattplatz und Hauptbahnhof unterchieden.
names(bu_geo)[c(17, 19)] <- c('Stadtbezirk.neu', 'Stadtteil.neu')
names(ze_geo)[c(19, 21)] <- c('Stadtbezirk.neu', 'Stadtteil.neu')


bu_ret <- merge(bu, bu_geo[, c(1, 17, 19)], by = "ID", all.x = TRUE)
ze_ret <- merge(ze, ze_geo[, c(1, 19, 21)], by = "ID", all.x = TRUE)

s21_ret <- merge(s21, key[, c('Stadtteil.chr', 'Stadtteil.int', 'Stadtbezirk.int')], by.x = 'Stadtteil', by.y = 'Stadtteil.chr', all.x = TRUE)
#-------------------------#
## Plausibilitaetsckecks ##
#-------------------------#
## Alte Zuornung vs. neue zuordnung ##
# Dies meisten Bezeichnungen müssten noch gleich sein, da es nur Beobachtungen am Rand betrifft.

check1 <- list()
for(i in unique(bu_ret$Stadtteil.neu)) {
  check1[[i]] <- table(bu_ret$Stadtteil[bu_ret$Stadtteil.neu == i])
}
# check1

check2 <- list()
for(i in unique(ze_ret$Stadtteil.neu)) {
  check2[[i]] <- table(ze_ret$stadtteil[ze_ret$Stadtteil.neu == i])
}
# check2

# Da die S21 Daten genau in den Grenzen der Bezirke und Teile des Shapefiles liegen, muss der Bezirk aus den Rohdaten genau dem Bezirk aus dem Merge entsprechen
tt <- merge(s21, key[, c('Stadtteil.chr', 'Stadtteil.int', 'Stadtbezirk.int', 'Stadtbezirk.chr')], by.x = 'Stadtteil', by.y = 'Stadtteil.chr', all.x = TRUE)
tt[is.na(tt$Stadtbezirk.chr), ]
cbind(as.character(tt$Stadtbezirk), as.character(tt$Stadtbezirk.chr))
any(as.character(tt$Stadtbezirk) != as.character(tt$Stadtbezirk.chr))

#----------------#
## Datanausgabe ##
#----------------#
bu_ret$Stadtbezirk.neu <- as.integer(bu_ret$Stadtbezirk.neu); bu_ret$Stadtteil.neu <- as.integer(bu_ret$Stadtteil.neu)
ze_ret$Stadtbezirk.neu <- as.integer(ze_ret$Stadtbezirk.neu); ze_ret$Stadtteil.neu <- as.integer(ze_ret$Stadtteil.neu)
key$Stadtbezirk.int <- as.integer(key$Stadtbezirk.int); key$Stadtteil.int <- as.integer(key$Stadtteil.int)
  
write.csv2(bu_ret, './buergerumfrage/population_aufbereitet_stadtteile.txt')
write.csv2(ze_ret, './zensus/population_aufbereitet_stadtteile.txt')
write.csv2(key, 'key_Stadtteile_Bezirke.csv', row.names = FALSE, fileEncoding = 'UTF-8')
write.csv2(s21_ret, './buergerumfrage_neu/Stuttgart21_aufbereitet_stadtteile.csv')
