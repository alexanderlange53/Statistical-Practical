########################
# Spatial exploratives #
########################
rm(list=ls())
# loading packages
require(ggplot2);require(reshape2);require(colorspace);require(gridExtra);require(scales)
require(rgdal);require(rgeos);require(ggmap);require(sp);require(maptools);require(rvest)
require(dplyr);require(gstat);require(raster)


# loading data

bearbeiter = 'Alex'

if(bearbeiter == 'Alex'){
  dataS <- read.csv2('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Auswertung/Neue_Daten/Stuttgart21_aufbereitet_stadtteile.csv',
                     dec = '.')
  bezirke <- readOGR(dsn = "/home/alex/Schreibtisch/Uni/statistisches_praktikum/Auswertung/Geodaten/bezirke", layer = "bezirke")
  Stadtteile <- readOGR(dsn = "/home/alex/Schreibtisch/Uni/statistisches_praktikum/Daten_Kneib/Stick/Daten_Kneib/Stadtteile_netto/", layer = "Stadtteile_netto")
} 
if(bearbeiter == 'Kai@Home'){
  dataS <- read.csv2('/home/kai/Dokumente/Master/Stat_Practical/Statistical-Practical/Rohdaten/Neue_Daten/Stuttgart21_aufbereitet.csv',
            dec = '.')
  bezirke <- readOGR(dsn = "/home/kai/Dokumente/Master/Stat_Practical/Statistical-Practical/Rohdaten/Geodaten/bezirke/", layer = "bezirke")
}
if(bearbeiter == 'Kai@Work'){
  dataS <- read.csv2('/home/khusmann/mnt/U/Promotion/Kurse/Stat_Praktikum/Auswertung/Neue_Daten//Stuttgart21_aufbereitet.csv',
                     dec = '.')
  bezirke <- readOGR(dsn = "/home/khusmann/mnt/U/Promotion/Kurse/Stat_Praktikum/Auswertung/Geodaten/bezirke/", layer = "bezirke")
}

source('SpatialPlots.R')
source('DataPrep.R')

# Plotten der Response Variablen mit Gauss-Krüger Informationen
# Meinung Stuttgart 21
GKPlot(dataS, bezirke = bezirke, Kategorien = 5)
GKPlot(dataS, bezirke = bezirke, Kategorien = 3)

# Bewertung Wohngegend
GKPlot(dataS, response = 'Bewertung.Wohngegend', 
       bezirke = bezirke, Kategorien = 5)
GKPlot(dataS, response = 'Bewertung.Wohngegend', 
       bezirke = bezirke, Kategorien = 3)

# Plotten der Response Variablen mit diskreten Information (Bezirke)
# Meinung zu Stuttgart 21
SpatAntPlot(dataS, bezirke)
SpatAntPlot(dataS, bezirke, Kategorien = 3)

# Bewertung Wohngegend
SpatAntPlot(dataS, bezirke, response = 'Bewertung.Wohngegend')
SpatAntPlot(dataS, bezirke, response = 'Bewertung.Wohngegend', Kategorien = 3)

# Plotten der Response Variablen mit diskreten Information (Stadtteile)
# Meinung zu Stuttgart 21
SpatAntPlot(dataS, Stadtteile, Bezirke = F)
SpatAntPlot(dataS, Stadtteile, Kategorien = 3, Bezirke = F)

# Bewertung Wohngegend
SpatAntPlot(dataS, Stadtteile, response = 'Bewertung.Wohngegend', Bezirke = F)
SpatAntPlot(dataS, Stadtteile, response = 'Bewertung.Wohngegend', Kategorien = 3, Bezirke = F)

#-----------------------------------# Variogram zu Gauss Krüger Informationen #---------------------------------------#

dataC <- dataS
for(i in 1:nrow(dataS)){
  if(dataS$Meinung.zu.Stuttgart.21[i] == 6){
    dataS$Meinung.zu.Stuttgart.21[i] <- NA
  }}
dataS <- na.omit(dataS)
# Zusammenführen von 'Sehr gut' und 'Gut' zu Zustimmung und 'Sehr Schlecht' und 'Schlecht' zu Ablehnend 
for(i in 1:nrow(dataC)){
  if(dataC$Meinung.zu.Stuttgart.21[i] == 2){
    dataC$Meinung.zu.Stuttgart.21[i] <- 1
  }}
for(i in 1:nrow(dataS)){
  if(dataC$Meinung.zu.Stuttgart.21[i] == 3){
    dataC$Meinung.zu.Stuttgart.21[i] <- 2
  }}
for(i in 1:nrow(dataS)){
  if(dataC$Meinung.zu.Stuttgart.21[i] == 4){
    dataC$Meinung.zu.Stuttgart.21[i] <- 3
  }}
for(i in 1:nrow(dataS)){
  if(dataC$Meinung.zu.Stuttgart.21[i] == 5){
    dataC$Meinung.zu.Stuttgart.21[i] <- 3
  }}

coordinates(dataC) <- ~X+Y
proj4string(dataC) <- CRS("+init=epsg:31467")


vario <- variogram(Meinung.zu.Stuttgart.21~1, dataC)
variom <- variogram(Meinung.zu.Stuttgart.21~1, width=3,cutoff=100, dataC, map = T)

theme_set(theme_bw(15))
ggplot(vario,aes(x=dist,y=gamma,size=np)) + geom_point()

plot(vario)
plot(variom)


#------------------------------------------# Variogramme #------------------------------------------------------#

# Sind das equilvalent zur Korrelationsmatrix für räumliche Daten.

