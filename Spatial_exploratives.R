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

#-----------------------------------------# Diskrete Informationen mit Stadtteilen #---------------------------------------#

# Einladen der Stadtteildaten

# loading data
if(bearbeiter == 'Alex'){
  Stadtteile <- readOGR(dsn = "/home/alex/Schreibtisch/Uni/statistisches_praktikum/Daten_Kneib/Stick/Daten_Kneib/Stadtteile_netto/", layer = "Stadtteile_netto")
}
if(bearbeiter == 'Kai@Home'){
  Stadtteile <- readOGR(dsn = "/home/kai/Dokumente/Master/Stat_Practical/Statistical-Practical/Rohdaten/Stadtteile_Shapefile/", layer = "Stadtteile_netto")
}
if(bearbeiter == 'Kai@Work'){
  Stadtteile <- readOGR(dsn = "", layer = "Stadtteile_netto")
}

#-------------------------#
# 3 Kategorien facet wrap #
#-------------------------#

# selcting variables of interest
myvar <- c('Meinung.zu.Stuttgart.21', 'Stadtteil', 'X', 'Y')
ST <- dataS[myvar]

# Ermittlung der Zustimmung in den Stadtteilen
beob.teile <- as.data.frame(table(ST$Stadtteil))
meinung.teile <-as.data.frame(table(ST$Stadtteil, ST$Meinung.zu.Stuttgart.21))

# Anteile berechnen 
Zustimmung <- ((meinung.teile$Freq[1:141] + meinung.teile$Freq[142:282])/beob.teile$Freq)*100
Zustimmung.a <- as.data.frame(cbind(as.character(beob.teile$Var1), Zustimmung))
Zustimmung.a$Meinung <- as.factor('Zustimmung')
Neutral <- ((meinung.teile$Freq[283:423])/beob.teile$Freq)*100
Neutral.a <- as.data.frame(cbind(as.character(beob.teile$Var1), Neutral))
Neutral.a$Meinung <- as.factor('Neutral')
Ablehnung <- ((meinung.teile$Freq[424:564] + meinung.teile$Freq[565:705])/beob.teile$Freq)*100
Ablehnung.a <- as.data.frame(cbind(as.character(beob.teile$Var1), Ablehnung))
Ablehnung.a$Meinung <- as.factor('Ablehnung')

# ID variable erzeugen um objecte zu verbinden
Stadtteile@data$id <- rownames(Stadtteile@data)
watershedPoints <- fortify(Stadtteile, region = "id")

# Data Frame und Spatial object verbinden
bbs <- merge(watershedPoints, Stadtteile@data, by = 'id', all.x = T)
colnames(Zustimmung.a) <- c('STADTTEIL', 'anteil', 'Meinung')
colnames(Neutral.a) <- c('STADTTEIL', 'anteil', 'Meinung')
colnames(Ablehnung.a) <- c('STADTTEIL', 'anteil', 'Meinung')
bbz <- merge(bbs, Zustimmung.a, by = 'STADTTEIL', all.x = T)
bbn <- merge(bbs, Neutral.a, by = 'STADTTEIL', all.x = T)
bba <- merge(bbs, Ablehnung.a, by = 'STADTTEIL', all.x = T)
s.facet <- rbind(bbz, bbn, bba)
s.facet$anteil <- as.numeric(as.character(s.facet$anteil))

# Sortieren um polygone richtig zu plotten
s.facet <- s.facet[order(s.facet$order),]
pol.na <- filter(s.facet, is.na(Meinung))
plo.na <- select(pol.na, STADTTEIL, id, long, lat, order, group)
s.facet <- na.omit(s.facet)

ggplot() +  geom_polygon(data = plo.na, aes(x = long, y = lat, group = group), fill = 'black') +
  geom_polygon(data=s.facet, aes(x=long, y=lat, group=group, fill = anteil, alpha = anteil), color = "black") +
  labs(x=NULL, y=NULL, title=NULL) +
  scale_fill_gradient(name = "Anteil\n in %", low = colo[2], high = 'darkblue', guide = "colorbar", na.value="black",
                       breaks = pretty_breaks(n = 5)) +
  scale_alpha(range = c(0.3,1), guide=FALSE) +
  coord_equal(1)+
  theme_bw(15) +
  theme(
    legend.position = 'right'
    ,axis.text.x=element_blank()
    ,axis.text.y=element_blank()
    ,axis.ticks.y=element_blank()
    ,axis.ticks.x=element_blank()
  ) + facet_wrap(~Meinung, nrow = 1)

#------------------------------------------# Variogramme #------------------------------------------------------#

# Sind das equilvalent zur Korrelationsmatrix für räumliche Daten.

