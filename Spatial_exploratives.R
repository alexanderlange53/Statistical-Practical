########################
# Spatial exploratives #
########################
rm(list=ls())
# loading packages
require(ggplot2);require(reshape2);require(colorspace);require(gridExtra);require(scales)
require(rgdal);require(rgeos);require(ggmap);require(sp);require(maptools);require(rvest)

bearbeiter = 'Alex'
# loading data
if(bearbeiter == 'Alex'){
  dataS <- read.csv2('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Auswertung/Neue_Daten/Stuttgart21_aufbereitet.csv',
                     dec = '.')
  bezirke <- readOGR(dsn = "/home/alex/Schreibtisch/Uni/statistisches_praktikum/Auswertung/Geodaten/bezirke", layer = "bezirke")
} else {
  dataS <- read.csv2('/home/khusmann/mnt/U/Promotion/Kurse/Stat_Praktikum/Auswertung/Neue_Daten//Stuttgart21_aufbereitet.csv',
                     dec = '.')
  bezirke <- readOGR(dsn = "/home/khusmann/mnt/U/Promotion/Kurse/Stat_Praktikum/Auswertung/Geodaten/bezirke/", layer = "bezirke")
  
}


# selcting variables of interest
myvar <- c('Meinung.zu.Stuttgart.21', 'X', 'Y')
ST22 <- dataS[myvar]
ST21 <- ST22
# numerical classes into factor classes
ST21$Meinung.zu.Stuttgart.21 <- ''
ST21$Meinung.zu.Stuttgart.21[ST22$Meinung.zu.Stuttgart.21 == 1] <- 'Sehr gut'
ST21$Meinung.zu.Stuttgart.21[ST22$Meinung.zu.Stuttgart.21 == 2] <- 'Gut'
ST21$Meinung.zu.Stuttgart.21[ST22$Meinung.zu.Stuttgart.21 == 3] <- 'Neutral'
ST21$Meinung.zu.Stuttgart.21[ST22$Meinung.zu.Stuttgart.21 == 4] <- 'Schlecht'
ST21$Meinung.zu.Stuttgart.21[ST22$Meinung.zu.Stuttgart.21 == 5] <- 'Sehr schlecht'
ST21$Meinung.zu.Stuttgart.21[ST22$Meinung.zu.Stuttgart.21 == 6] <- 'Keine Angabe'
# assigning names
names(ST21) <- c('Meinung', 'long', 'lat') 
# reshaping data frame for plotting
ST21.p <- melt(ST21, id = c('long', 'lat'))
# levels of factor
ST21.p$value <- factor(ST21.p$value, levels = c('Sehr gut', 'Gut', 'Neutral', 'Schlecht', 
                                                                                'Sehr schlecht', 'Keine Angabe'))

# Plotting points on Stuttgart polygon
ggplot() + geom_polygon(data=bezirke, aes(x=long, y=lat, group=group), fill="grey40",colour="white", alpha=0.5)+  
  labs(x=NULL, y=NULL, title=NULL)+
  geom_point(data=ST21.p, aes(x=long, y=lat, color = value, group = value)) +
  scale_color_manual(values = c(heat_hcl(5), 'black'))+
  coord_equal(1)+
  theme_bw(15) +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,legend.position = 'bottom'
    ,axis.text.x=element_blank()
    ,axis.text.y=element_blank()
    ,axis.ticks.y=element_blank()
    ,axis.ticks.x=element_blank()
    ,legend.title = element_blank()
  ) +
  guides(color = guide_legend(override.aes = list(size=5)))

######### Relative frequency plots ##############

# transform to spatial class
coordinates(ST21) <- ~ long + lat
# assign CRS
proj4string(ST21) <- CRS("+init=epsg:31467")
# reproject data
ST21 <- spTransform(ST21, CRS("+proj=longlat +datum=WGS84"))
# loading map
map <- get_map(location= rowMeans(bbox(ST21)), zoom=12, maptype = 'roadmap', scale = 2)
# transform back to data frame for ggplot
ST21.g <- as.data.frame(ST21)
# getting high ratings
SS <- ST21.g[which(ST21.g$Meinung=='Sehr gut'),]
# plotting map
g1 <- ggmap(map) + geom_density2d(data = SS, 
                                  aes(x = long, y = lat), size = 0.3) +
  stat_density2d(
    aes(x = long, y = lat, fill = ..level..,  alpha = ..level..),
    size = 2, bins = 4, data = SS,
    geom = "polygon"
  ) +
 theme(
    legend.position = 'none'
    ,axis.text.x=element_blank()
    ,axis.text.y=element_blank()
    ,axis.ticks.y=element_blank()
    ,axis.ticks.x=element_blank()
  ) + labs(title = 'Kontinuierlich')
g1 
# Plot ist auf jeden fall noch verbesserungswürdig, äußere Beobachtungen sind abgeschnitten.
# Alternative ist bisher nur eine Ansicht von sehr weit weg. Sinnvolle Legende einfügen

#---------------------# Diskrete Informationen mit Stadtbezirken #--------------------------#
# Für Farbpalette
colo <- diverge_hsv(3)

# selcting variables of interest
myvar <- c('Meinung.zu.Stuttgart.21', 'Stadtbezirk', 'X', 'Y')
ST <- dataS[myvar]

# Ermittlung der Zustimmung in den Stadtteilen ('Sehr gut')
beob.bez <- as.data.frame(table(ST$Stadtbezirk))
meinung.bez <-as.data.frame(table(ST$Stadtbezirk, ST$Meinung.zu.Stuttgart.21))

# Relative Anteile 'Sehr gut'
anteil <- (meinung.bez$Freq[1:23]/beob.bez$Freq)*100
M.bez.sg <- as.data.frame(cbind(as.character(beob.bez$Var1), anteil))
                          
# ID variable erzeugen um Data Frame und Spatial object zu verbinden
colnames(M.bez.sg) <- c('id', 'anteil')
bezirke@data$id <- rownames(bezirke@data)
watershedPoints <- fortify(bezirke, region = "id")

# Errechneten Anteile und räumliche Informationen verbinden
bb <- merge(watershedPoints, bezirke@data, by = 'id', all.x = T)
colnames(M.bez.sg) <- c('STADTBEZIR', 'anteil')
bb2 <- merge(bb, M.bez.sg, by = 'STADTBEZIR')
bb2$anteil <- as.numeric(as.character(bb2$anteil))
# Sortieren damit Poylogene richtig geplottet werden
bb2 <- bb2[order(bb2$order),]

# Plotten der Meinung 'Sehr gut' pro Bezirk
g2 <- ggplot(data=bb2, aes(x=long, y=lat, group=group, fill = anteil))+  
  geom_polygon(color = "black") +
  labs(x=NULL, y=NULL, title= 'Diskret Stadtbezirke') +
  scale_fill_gradient(name = "Prozent", low = colo[2], high = colo[1], guide = "colorbar",
                      breaks = pretty_breaks(n = 5)) +
  coord_equal(1)+
  theme_bw(15) +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,legend.position = 'right'
    ,axis.text.x=element_blank()
    ,axis.text.y=element_blank()
    ,axis.ticks.y=element_blank()
    ,axis.ticks.x=element_blank()
    )
g2
#---------------------# Diskrete Informationen mit Stadtteilen #--------------------------#

# Einladen der Stadtteildaten
bearbeiter = 'Alex'
# loading data
if(bearbeiter == 'Alex'){
  Stadtteile <- readOGR(dsn = "/home/alex/Schreibtisch/Uni/statistisches_praktikum/Daten_Kneib/Stick/Daten_Kneib/Stadtteile_netto/", layer = "Stadtteile_netto")
} else {
  Stadtteile <- readOGR(dsn = "/home/khusmann/mnt/U/Promotion/Kurse/Stat_Praktikum/Auswertung/Geodaten/bezirke/", layer = "bezirke")
}

# selcting variables of interest
myvar <- c('Meinung.zu.Stuttgart.21', 'Stadtteil', 'X', 'Y')
ST <- dataS[myvar]
# Ermittlung der Zustimmung in den Stadtteilen ('Sehr gut')
beob.teile <- as.data.frame(table(ST$Stadtteil))
meinung.teile <-as.data.frame(table(ST$Stadtteil, ST$Meinung.zu.Stuttgart.21))

# Anteile 'Sehr gut' berechnen 
anteil <- (meinung.teile$Freq[1:141]/beob.teile$Freq)*100
M.teile.sg <- as.data.frame(cbind(as.character(beob.teile$Var1), anteil))

# ID variable erzeugen um objecte zu verbinden
colnames(M.teile.sg) <- c('id', 'anteil')
Stadtteile@data$id <- rownames(Stadtteile@data)
watershedPoints <- fortify(Stadtteile, region = "id")

# Data Frame und Spatial object verbinden
bbs <- merge(watershedPoints, Stadtteile@data, by = 'id', all.x = T)
colnames(M.teile.sg) <- c('STADTTEIL', 'anteil')
bbs2 <- merge(bbs, M.teile.sg, by = 'STADTTEIL', all.x = T)
bbs2$anteil <- as.numeric(as.character(bbs2$anteil))

# Sortieren um polygone richtig zu plotten
bbs2 <- bbs2[order(bbs2$order),]

# Plotten der Meinung 'Sehr gut' pro Stadtteil
g3 <- ggplot(data=bbs2, aes(x=long, y=lat, group=group, fill = anteil))+  
  geom_polygon(color = "black") +
  labs(x=NULL, y=NULL, title='Diskret Stadtteile') +
  scale_fill_gradient(name = "Prozent", low = colo[2], high = colo[1], guide = "colorbar",na.value="black",
                      breaks = pretty_breaks(n = 5)) +
  coord_equal(1)+
  theme_bw(15) +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,legend.position = 'right'
    ,axis.text.x=element_blank()
    ,axis.text.y=element_blank()
    ,axis.ticks.y=element_blank()
    ,axis.ticks.x=element_blank()
  )
g3

# Alle 3 möglichkeiten der Darstellung zum Vergleich
grid.arrange(g1, g2, g3, ncol = 3)
# Alle drei plots zeigen in etwas das gleich: Gewisse cluster von Zustimmung sind vorhanden, Zustimmung eher im Nordosten.
# Plot 3 zeigt am genausten wo Zustimmung hoch und wo nicht