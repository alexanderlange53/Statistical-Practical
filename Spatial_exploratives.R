########################
# Spatial exploratives #
########################
rm(list=ls())
# loading packages
require(ggplot2);require(reshape2);require(colorspace);require(gridExtra);require(scales)
require(rgdal);require(rgeos);require(ggmap);require(sp);require(maptools);require(rvest)
require(dplyr);require(gstat);require(raster)


# loading data

bearbeiter = 'Kai@Home'

if(bearbeiter == 'Alex'){
  dataS <- read.csv2('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Auswertung/Neue_Daten/Stuttgart21_aufbereitet_stadtteile.csv',
                     dec = '.')
  bezirke <- readOGR(dsn = "/home/alex/Schreibtisch/Uni/statistisches_praktikum/Auswertung/Geodaten/bezirke", layer = "bezirke")
  Stadtteile <- readOGR(dsn = "/home/alex/Schreibtisch/Uni/statistisches_praktikum/Daten_Kneib/Stick/Daten_Kneib/Stadtteile_netto/", layer = "Stadtteile_netto")
} 
if(bearbeiter == 'Kai@Home'){
  dataS <- read.csv2('/home/kai/Dokumente/Master/Stat_Practical/Statistical-Practical/Rohdaten/buergerumfrage_neu/Stuttgart21_aufbereitet_stadtteile.csv',
            dec = '.')
  bezirke <- readOGR(dsn = "/home/kai/Dokumente/Master/Stat_Practical/Statistical-Practical/Rohdaten/Geodaten/bezirke/", layer = "bezirke")
  Stadtteile <- readOGR(dsn = "/home/kai/Dokumente/Master/Stat_Practical/Statistical-Practical/Rohdaten/Geodaten/Stadtteile_netto/", layer = "Stadtteile_netto")
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

# Variogramme zur Meinung zu STuttgart 21 ----------------------------------------------
dataC <- dataS
for(i in 1:nrow(dataS)){
  if(dataS$Meinung.zu.Stuttgart.21[i] == 6){
    dataS$Meinung.zu.Stuttgart.21[i] <- NA
  }}
dataS <- na.omit(dataS)

dataC <- DataPrep(dataC, binom = F) 

# Mit 5 Klassen 
coordinates(dataC) <- ~X+Y
proj4string(dataC) <- CRS("+init=epsg:31467")

vario <- variogram(Meinung.zu.Stuttgart.21~1, dataC)
variom <- variogram(Meinung.zu.Stuttgart.21~1, width=3,cutoff=100, dataC, map = T)

theme_set(theme_bw(15))
ggplot(vario,aes(x=dist,y=gamma,size=np)) + geom_point()

plot(vario)
plot(variom)

# Mit 3 Klassen 
coordinates(dataS) <- ~X+Y
proj4string(dataS) <- CRS("+init=epsg:31467")

vario <- variogram(Meinung.zu.Stuttgart.21~1, dataS)
variom <- variogram(Meinung.zu.Stuttgart.21~1, width=3,cutoff=100, dataS, map = T)

theme_set(theme_bw(15))
ggplot(vario,aes(x=dist,y=gamma,size=np)) + geom_point()

plot(vario)
plot(variom)

# Variogramme zur Bewertung der Wohngegend --------------------------------------------------------
if(bearbeiter == 'Alex'){
  dataS <- read.csv2('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Auswertung/Neue_Daten/Stuttgart21_aufbereitet_stadtteile.csv',
                     dec = '.')
} 
if(bearbeiter == 'Kai@Home'){
  dataS <- read.csv2('/home/kai/Dokumente/Master/Stat_Practical/Statistical-Practical/Rohdaten/buergerumfrage_neu/Stuttgart21_aufbereitet_stadtteile.csv',
                     dec = '.')
}
if(bearbeiter == 'Kai@Work'){
  dataS <- read.csv2('/home/khusmann/mnt/U/Promotion/Kurse/Stat_Praktikum/Auswertung/Neue_Daten//Stuttgart21_aufbereitet.csv',
                     dec = '.')
}





dataC <- dataS
for(i in 1:nrow(dataS)){
  if(dataS$Bewertung.Wohngegend[i] == 6){
    dataS$Bewertung.Wohngegend[i] <- NA
  }}
dataS <- na.omit(dataS)

dataC <- DataPrep(dataC, binom = F, Stuttgart21 = F) 

# Mit 5 Klassen 
coordinates(dataC) <- ~X+Y
proj4string(dataC) <- CRS("+init=epsg:31467")

vario <- variogram(Bewertung.Wohngegend ~1, dataC)
variom <- variogram(Bewertung.Wohngegend ~1, width=3,cutoff=100, dataC, map = T)

theme_set(theme_bw(15))
ggplot(vario,aes(x=dist,y=gamma,size=np)) + geom_point()

plot(vario)
plot(variom)

# Mit 3 Klassen 
coordinates(dataS) <- ~X+Y
proj4string(dataS) <- CRS("+init=epsg:31467")

vario <- variogram(Bewertung.Wohngegend~1, dataS)
variom <- variogram(Bewertung.Wohngegend~1, width=3,cutoff=100, dataS, map = T)

theme_set(theme_bw(15))
ggplot(vario,aes(x=dist,y=gamma,size=np)) + geom_point()

plot(vario)
plot(variom)
