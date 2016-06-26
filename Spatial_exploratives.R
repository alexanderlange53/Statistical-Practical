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

#-------------------------------------------# Relative frequency plots #------------------------------------------------#
colo <- diverge_hsv(3)
# transform to spatial class
coordinates(ST21) <- ~ long + lat
# assign CRS
proj4string(ST21) <- CRS("+init=epsg:31467")
# reproject data
ST21 <- spTransform(ST21, CRS("+proj=longlat +datum=WGS84"))
# loading map
map <- get_map(location= rowMeans(bbox(ST21)), zoom=11, maptype = 'terrain', scale = 2)
# transform back to data frame for ggplot
ST21.g <- as.data.frame(ST21)

bezirke.t <- spTransform(bezirke, CRS("+proj=longlat"))

#-------------------------#
# 5 Kategorien facet wrap #
#-------------------------#

# getting ratings
SS <- ST21.g[which(ST21.g$Meinung=='Sehr gut'),]
SS2 <- ST21.g[which(ST21.g$Meinung=='Gut'),]
SS3 <- ST21.g[which(ST21.g$Meinung=='Neutral'),]
SS4 <- ST21.g[which(ST21.g$Meinung=='Schlecht'),]
SS5 <- ST21.g[which(ST21.g$Meinung=='Sehr schlecht'),]

# erstellen des neuen data frames
data.f <- rbind(SS, SS2, SS3, SS4, SS5)
data.f$Meinung <- factor(data.f$Meinung, levels = c('Sehr gut', 'Gut', 'Neutral', 'Schlecht', 'Sehr schlecht'))

# plotting map
ggmap(map, extent = 'device', legend = 'topright') + geom_polygon(data=bezirke.t, aes(x=long, y=lat, group=group),colour="black", alpha=0) +
  stat_density2d(
    aes(x = long, y = lat, fill = ..level..,  alpha = ..level..),
    size = 2, bins = 8, data = data.f,
    geom = "polygon"
  ) +
  theme_bw(15) +
  scale_fill_gradient(low=colo[2], high = 'darkblue')+
  scale_alpha(range = c(0.1,0.7), guide=FALSE) +
  labs(fill = 'Dichte') +
  xlim(9.035, 9.32) + ylim(48.69, 48.87) +
  facet_wrap(~ Meinung)


#-------------------------#
# 3 Kategorien facet wrap #
#-------------------------#

# getting ratings
S3 <- ST21.g[which(ST21.g$Meinung=='Sehr gut' | ST21.g$Meinung=='Gut'),]
S32 <- ST21.g[which(ST21.g$Meinung=='Neutral'),]
S4 <- ST21.g[which(ST21.g$Meinung=='Schlecht' | ST21.g$Meinung=='Sehr schlecht'),]

# erstellen des neuen data frames
S3$Meinung <- 'Zustimmung'
S4$Meinung <- 'Ablehnung'
data.facet <- rbind(S3, S32, S4)
data.facet$Meinung <- factor(data.facet$Meinung, levels = c('Zustimmung', 'Neutral', 'Ablehnung'))

# Plotting map
ggmap(map, extent = 'device', legend = 'topright') + geom_polygon(data=bezirke.t, aes(x=long, y=lat, group=group),colour="black", alpha=0) +
  stat_density2d(
    aes(x = long, y = lat, fill = ..level..,  alpha = ..level..),
    size = 2, bins = 8, data = data.facet,
    geom = "polygon"
  ) +
  theme_bw(15) +
  scale_fill_gradient(low=colo[2], high = 'darkblue')+
  scale_alpha(range = c(0.1,0.7), guide=FALSE) +
  labs(fill = 'Dichte') +
  xlim(9.035, 9.32) + ylim(48.69, 48.87) +
  facet_wrap(~ Meinung)


#-----------------------------------# Diskrete Informationen mit Stadtbezirken #--------------------------------------#

#--------------------------#
# 5 Kategorien facet wraps #
#--------------------------#

# selcting variables of interest
myvar <- c('Meinung.zu.Stuttgart.21', 'Stadtbezirk', 'X', 'Y')
ST <- dataS[myvar]

# Ermittlung der Zustimmung in den Stadtteilen ('Sehr gut')
beob.bez <- as.data.frame(table(ST$Stadtbezirk))
meinung.bez <-as.data.frame(table(ST$Stadtbezirk, ST$Meinung.zu.Stuttgart.21))

# Relative Anteile 
SG <- (meinung.bez$Freq[1:23]/beob.bez$Freq)*100
SGA <- as.data.frame(cbind(as.character(beob.bez$Var1), SG))
SGA$Meinung <- as.factor('Sehr gut')
G <- (meinung.bez$Freq[24:46]/beob.bez$Freq)*100
GA <- as.data.frame(cbind(as.character(beob.bez$Var1), G))
GA$Meinung <- as.factor('Gut')
N <- (meinung.bez$Freq[47:69]/beob.bez$Freq)*100
N.A <- as.data.frame(cbind(as.character(beob.bez$Var1), N))
N.A$Meinung <- as.factor('Neutral')
S <- (meinung.bez$Freq[70:92]/beob.bez$Freq)*100
SA <- as.data.frame(cbind(as.character(beob.bez$Var1), S))
SA$Meinung <- as.factor('Schlecht')
S.S <- (meinung.bez$Freq[93:115]/beob.bez$Freq)*100
S.SA <- as.data.frame(cbind(as.character(beob.bez$Var1), S.S))
S.SA$Meinung <- as.factor('Sehr schlecht')

# ID variable erzeugen um Data Frame und Spatial object zu verbinden
bezirke@data$id <- rownames(bezirke@data)
watershedPoints <- fortify(bezirke, region = "id")

# Errechneten Anteile und räumliche Informationen verbinden
bb <- merge(watershedPoints, bezirke@data, by = 'id', all.x = T)
colnames(SGA) <- c('STADTBEZIR', 'anteil', 'Meinung')
colnames(GA) <- c('STADTBEZIR', 'anteil', 'Meinung')
colnames(N.A) <- c('STADTBEZIR', 'anteil', 'Meinung')
colnames(SA) <- c('STADTBEZIR', 'anteil', 'Meinung')
colnames(S.SA) <- c('STADTBEZIR', 'anteil', 'Meinung')
bbSGA <- merge(bb, SGA, by = 'STADTBEZIR')
bbGA <- merge(bb, GA, by = 'STADTBEZIR')
bbNA <- merge(bb, N.A, by = 'STADTBEZIR')
bbSA <- merge(bb, SA, by = 'STADTBEZIR')
bbSSA <- merge(bb, S.SA, by = 'STADTBEZIR')

# erstellen des neuen data Frames
b.facet <- rbind(bbSGA, bbGA, bbNA, bbSA, bbSSA)
b.facet$anteil <- as.numeric(as.character(b.facet$anteil))

# Sortieren damit Poylogene richtig geplottet werden
b.facet <- b.facet[order(b.facet$order),]

# Plotten 
ggplot(data=b.facet, aes(x=long, y=lat, group=group, fill = anteil, alpha = anteil))+  
  geom_polygon(color = "black") +
  labs(x=NULL, y=NULL, title= NULL) +
  scale_fill_gradient(name = "Anteil \n in %", low = colo[2], high = 'darkblue', guide = "colorbar",
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
  ) + facet_wrap(~ Meinung)

#-------------------------#
# 3 Kategorien facet wrap #
#-------------------------#

# Relative Anteile 
SG <- ((meinung.bez$Freq[1:23] + meinung.bez$Freq[24:46]) /beob.bez$Freq)*100
SGA <- as.data.frame(cbind(as.character(beob.bez$Var1), SG))
SGA$Meinung <- as.factor('Zustimmung')
N <- (meinung.bez$Freq[47:69]/beob.bez$Freq)*100
N.A <- as.data.frame(cbind(as.character(beob.bez$Var1), N))
N.A$Meinung <- as.factor('Neutral')
S <- ((meinung.bez$Freq[70:92] + meinung.bez$Freq[93:115])/beob.bez$Freq)*100
SA <- as.data.frame(cbind(as.character(beob.bez$Var1), S))
SA$Meinung <- as.factor('Ablehnung')


# ID variable erzeugen um Data Frame und Spatial object zu verbinden
bezirke@data$id <- rownames(bezirke@data)
watershedPoints <- fortify(bezirke, region = "id")

# Errechneten Anteile und räumliche Informationen verbinden
bb <- merge(watershedPoints, bezirke@data, by = 'id', all.x = T)
colnames(SGA) <- c('STADTBEZIR', 'anteil', 'Meinung')
colnames(N.A) <- c('STADTBEZIR', 'anteil', 'Meinung')
colnames(SA) <- c('STADTBEZIR', 'anteil', 'Meinung')
bbSGA <- merge(bb, SGA, by = 'STADTBEZIR')
bbNA <- merge(bb, N.A, by = 'STADTBEZIR')
bbSA <- merge(bb, SA, by = 'STADTBEZIR')

# erstellen des neuen data Frames
b.facet <- rbind(bbSGA, bbNA, bbSA)
b.facet$anteil <- as.numeric(as.character(b.facet$anteil))

# Sortieren damit Poylogene richtig geplottet werden
b.facet <- b.facet[order(b.facet$order),]

# Plotten 
ggplot(data=b.facet, aes(x=long, y=lat, group=group, fill = anteil, alpha = anteil))+  
  geom_polygon(color = "black") +
  labs(x=NULL, y=NULL, title= NULL) +
  scale_fill_gradient(name = "Anteil \n in %", low = colo[2], high = 'darkblue', guide = "colorbar",
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
  ) + facet_wrap(~ Meinung)

#-----------------------------------------# Diskrete Informationen mit Stadtteilen #---------------------------------------#

# Einladen der Stadtteildaten
bearbeiter = 'Alex'
# loading data
if(bearbeiter == 'Alex'){
  Stadtteile <- readOGR(dsn = "/home/alex/Schreibtisch/Uni/statistisches_praktikum/Daten_Kneib/Stick/Daten_Kneib/Stadtteile_netto/", layer = "Stadtteile_netto")
} else {
  Stadtteile <- readOGR(dsn = "/home/khusmann/mnt/U/Promotion/Kurse/Stat_Praktikum/Auswertung/Geodaten/bezirke/", layer = "bezirke")
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
s.facet <- na.omit(s.facet)

ggplot(data=s.facet, aes(x=long, y=lat, group=group, fill = anteil, alpha = anteil))+  
  geom_polygon(color = "black") +
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
