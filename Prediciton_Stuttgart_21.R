###########################
# Prediction Stuttgart 21 #
###########################

rm(list=ls())
# loading packages
require('ROCR')
require('mgcv')
require('splines')
require(rgdal);require(rgeos)
require(ggplot2)
require(maptools);require(rvest);require(dplyr);require(colorspace)
require(ggplot2);require(reshape2);require(colorspace);require(gridExtra);require(scales)
require(rgdal);require(rgeos);require(sp);require(maptools);require(rvest)
require(ggmap);require(dplyr);require(gstat)#;require(raster) # package loading
colo <- diverge_hsv(3)
# Laden von Populationen

bearbeiter = 'Alex'
# loading data
if(bearbeiter == 'Alex'){
  Umfrage <- read.csv2('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Daten_Kneib/Stick/buergerumfrage/population_aufbereitet.txt')
  Zensus <- read.csv2('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Daten_Kneib/Stick/zensus/population_aufbereitet.txt')
  bezirke <- readOGR(dsn = "/home/alex/Schreibtisch/Uni/statistisches_praktikum/Auswertung/Geodaten/bezirke", layer = "bezirke")
  dataS <- read.csv2('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Auswertung/Neue_Daten/Stuttgart21_aufbereitet.csv',
                     dec = '.')
}
if(bearbeiter == 'Kai@Home'){
  Umfrage <- read.csv2('/home/kai/Dokumente/Master/Stat_Practical/Statistical-Practical/Rohdaten/buergerumfrage/population_aufbereitet.txt')
  Zensus <- read.csv2('/home/kai/Dokumente/Master/Stat_Practical/Statistical-Practical/Rohdaten/zensus/population_aufbereitet.txt')
}
if(bearbeiter == 'Kai@Work') {

}


#--------------------------------------------------#
# Populationen in die Form der Schätzungen bringen #
#--------------------------------------------------#

# Auswahl der passenden Variablen
u.p <- select(Umfrage, Altersklasse,Haushaltsgroesse, Geschlecht,Familienstand, Nationalitaet,GaussX, GaussY, Stadtteil )
names(u.p) <- c("Altersklasse.Befragter","Personenzahl.im.Haushalt", "Geschlecht","Familienstand","Nationalität","X" ,"Y", 'Stadtteil')

z.p <- select(Zensus, alter,haushaltsgroesse, geschlecht, familienstand, staatsangehoerigkeit, xcoord, ycoord, stadtteil )
names(z.p) <- c("Altersklasse.Befragter","Personenzahl.im.Haushalt","Geschlecht","Familienstand","Nationalität","X" ,"Y", 'Stadtteil')

# Anpassen der Akltersklassen
u.p2 <- u.p
u.p2$Altersklasse.Befragter <- ""
u.p2$Altersklasse.Befragter[u.p$Altersklasse.Befragter == 1] <- 1
u.p2$Altersklasse.Befragter[u.p$Altersklasse.Befragter == 2 | u.p$Altersklasse.Befragter == 3] <- 2
u.p2$Altersklasse.Befragter[u.p$Altersklasse.Befragter == 4 | u.p$Altersklasse.Befragter == 5] <- 3
u.p2$Altersklasse.Befragter[u.p$Altersklasse.Befragter == 6 | u.p$Altersklasse.Befragter == 7] <- 4
u.p2$Altersklasse.Befragter[u.p$Altersklasse.Befragter == 8 | u.p$Altersklasse.Befragter == 9] <- 5
u.p2$Altersklasse.Befragter[u.p$Altersklasse.Befragter == 10 | u.p$Altersklasse.Befragter == 11 
                            | u.p$Altersklasse.Befragter == 12 | u.p$Altersklasse.Befragter == 13
                            | u.p$Altersklasse.Befragter == 14] <- 6
z.p2 <- z.p
z.p2$Altersklasse.Befragter <- ""
z.p2$Altersklasse.Befragter[z.p$Altersklasse.Befragter == 1] <- 1
z.p2$Altersklasse.Befragter[z.p$Altersklasse.Befragter == 2 | z.p$Altersklasse.Befragter == 3] <- 2
z.p2$Altersklasse.Befragter[z.p$Altersklasse.Befragter == 4 | z.p$Altersklasse.Befragter == 5] <- 3
z.p2$Altersklasse.Befragter[z.p$Altersklasse.Befragter == 6 | z.p$Altersklasse.Befragter == 7] <- 4
z.p2$Altersklasse.Befragter[z.p$Altersklasse.Befragter == 8 | z.p$Altersklasse.Befragter == 9] <- 5

# Anpassen der Nationalität
u.p2$Nationalität <- ''
u.p2$Nationalität[u.p$Nationalität == 'Deutsch'] <- 'Deutsch'
u.p2$Nationalität[u.p$Nationalität == 'Nicht-Deutsch'] <- 'Nicht Deutsch'

z.p2$Nationalität <- ''
z.p2$Nationalität[z.p$Nationalität == 'Deutschland'] <- 'Deutsch'
z.p2$Nationalität[z.p$Nationalität == 'Ausland'] <- 'Nicht Deutsch'

# Anpassen der Geschlechter
u.p2$Geschlecht <- ''
u.p2$Geschlecht[u.p$Geschlecht == 'Mann'] <- 'Männlich'
u.p2$Geschlecht[u.p$Geschlecht == 'Frau'] <- 'Weiblich'

z.p2$Geschlecht <- ''
z.p2$Geschlecht[z.p$Geschlecht == 'maennlich'] <- 'Männlich'
z.p2$Geschlecht[z.p$Geschlecht == 'weiblich'] <- 'Weiblich'

# Klasse der Variablen festlegen
u.p2$Altersklasse.Befragter <- as.integer(u.p$Altersklasse.Befragter)
u.p2$Geschlecht <- as.factor(u.p2$Geschlecht)
u.p2$Nationalität <- as.factor(u.p2$Nationalität)

z.p2$Altersklasse.Befragter <- as.integer(z.p$Altersklasse.Befragter)
z.p2$Geschlecht <- as.factor(z.p2$Geschlecht)
z.p2$Nationalität <- as.factor(z.p2$Nationalität)

# Familienstand
z.p2$Familienstand <- ''
z.p2$Familienstand[z.p$Familienstand == 'geschieden'] <- 'geschieden'
z.p2$Familienstand[z.p$Familienstand == 'ledig'] <- 'ledig'
z.p2$Familienstand[z.p$Familienstand == 'verheiratet/eingetragene Partnerschaft'] <- 'verheiratet'
z.p2$Familienstand[z.p$Familienstand == 'verwitwet'] <- 'verwitwet'
z.p2$Familienstand <- as.factor(z.p2$Familienstand)

# Personzahl

z.p2$Personenzahl.im.Haushalt <- ''
z.p2$Personenzahl.im.Haushalt[z.p$Personenzahl.im.Haushalt == '1'] <- 1
z.p2$Personenzahl.im.Haushalt[z.p$Personenzahl.im.Haushalt == '2'] <- 2
z.p2$Personenzahl.im.Haushalt[z.p$Personenzahl.im.Haushalt == '3'] <- 3
z.p2$Personenzahl.im.Haushalt[z.p$Personenzahl.im.Haushalt == '4'] <- 4
z.p2$Personenzahl.im.Haushalt[z.p$Personenzahl.im.Haushalt == '5'] <- 5
z.p2$Personenzahl.im.Haushalt[z.p$Personenzahl.im.Haushalt == '6'] <- 5

z.p2$Personenzahl.im.Haushalt <- as.numeric(z.p2$Personenzahl.im.Haushalt)

#-----------------------------------------# Model mit stetigen räumlichen Informationen #------------------------------------#

# Laden der GAM Schätzungen
model2 <- readRDS('step.model.rds') # Gauss Krüger Informationen mit 3 Kategorien

# Predicten der Umfrage population
pred.pop.u.3 <- predict.gam(model2, newdata = u.p2, type = 'response')
pred.pop.z.3 <- predict.gam(model2, newdata = z.p2, type = 'response')

# Räumliche Informationen dazu fügen
pred.pop.u <- cbind(pred.pop.u.3, u.p2$X, u.p2$Y, u.p2$Stadtteil)
pred.pop.z <- cbind(pred.pop.z.3, z.p2$X, z.p2$Y, z.p2$Stadtteil)

colnames(pred.pop.u) <- c('1', '2', '3', 'X', 'Y', 'Stadtteil')
colnames(pred.pop.z) <- c('1', '2', '3', 'X', 'Y', 'Stadtteil')

# ermitterln der Gruppe mir höchster Wahrscheinlichkeit
Meinung <- rep(0, nrow(pred.pop.u))
for(i in 1:nrow(pred.pop.u)){
  if(pred.pop.u[i,1] > pred.pop.u[i,2] & pred.pop.u[i,1] > pred.pop.u[i,3]){
    Meinung[i] <- 1
  }
  if(pred.pop.u[i,2] > pred.pop.u[i,1] & pred.pop.u[i,2] > pred.pop.u[i,3]){
    Meinung[i] <- 2
  } 
  if(pred.pop.u[i,3] > pred.pop.u[i,1] & pred.pop.u[i,3] > pred.pop.u[i,2]){
    Meinung[i] <- 3
  }
}
pred.pop.u <- cbind(pred.pop.u, Meinung)

Meinung <- rep(0, nrow(pred.pop.z))
for(i in 1:nrow(pred.pop.z)){
  if(pred.pop.z[i,1] > pred.pop.z[i,2] & pred.pop.z[i,1] > pred.pop.z[i,3]){
    Meinung[i] <- 1
  }
  if(pred.pop.z[i,2] > pred.pop.z[i,1] & pred.pop.z[i,2] > pred.pop.z[i,3]){
    Meinung[i] <- 2
  } 
  if(pred.pop.z[i,3] > pred.pop.z[i,1] & pred.pop.z[i,3] > pred.pop.z[i,2]){
    Meinung[i] <- 3
  }
}
pred.pop.z <- cbind(pred.pop.z, Meinung)

# Speichern der Geschätzten Grundgesamtheit
write.table(pred.pop.u, file="Pop_geschätzt_u_3.csv", sep=";", col.names=TRUE, row.names=FALSE, quote=FALSE)
write.table(pred.pop.z, file="Pop_geschätzt_z_3.csv", sep=";", col.names=TRUE, row.names=FALSE, quote=FALSE)

# Häufigkeiten der geschätzten Klassen
count_u_cs <- as.data.frame(table(pred.pop.u[,7]))
write.table(count_u_cs, file = 'count_u_cs.csv', sep = ';', col.names = T, row.names = F)

count_z_cs <- as.data.frame(table(pred.pop.z[,7]))
write.table(count_z_cs, file = 'count_z_cs.csv', sep = ';', col.names = T, row.names = F)


#---------------------------------------------------------------------------------------------------------------------#

bearbeiter = 'Alex'
# loading data
if(bearbeiter == 'Alex'){
  pred.pop.u <- read.csv2('Pop_geschätzt_u_3.csv')
  pred.pop.z <- read.csv2('Pop_geschätzt_z_3.csv')
}
if(bearbeiter == 'Kai@Home'){
  pred.pop.u <- read.csv2('/home/kai/Dokumente/Master/Stat_Practical/Statistical-Practical/Rohdaten/buergerumfrage/population_aufbereitet.txt')
  pred.pop.u <- read.csv2('/home/kai/Dokumente/Master/Stat_Practical/Statistical-Practical/Rohdaten/zensus/population_aufbereitet.txt')
}
if(bearbeiter == 'Kai@Work') {
  
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
ST21$long <- as.numeric(as.character(ST21$long))
ST21$lat <- as.numeric(as.character(ST21$lat))
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
gk1 <- ggmap(map, extent = 'device', legend = 'topright') + geom_polygon(data=bezirke.t, aes(x=long, y=lat, group=group),colour="black", alpha=0) +
  stat_density2d(
    aes(x = long, y = lat, fill = ..level..,  alpha = ..level..),
    size = 2, bins = 8, data = data.facet,
    geom = "polygon"
  ) +
  theme_bw(10) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = 'bottom') +
  scale_fill_gradient(low=colo[2], high = 'darkblue')+
  scale_alpha(range = c(0.5,0.9), guide=FALSE) +
  labs(fill = 'Dichte') +
  xlim(9.035, 9.32) + ylim(48.69, 48.87) +
  facet_wrap(~ Meinung, nrow = 1)
gk1

saveRDS(gk1, 'gk1.rds')


gk11 <- ggmap(map, extent = 'device', legend = 'topright') + geom_polygon(data=bezirke.t, aes(x=long, y=lat, group=group),colour="black", alpha=0) +
  stat_density2d(
    aes(x = long, y = lat, fill = ..level..,  alpha = ..level..),
    size = 2, bins = 8, data = data.facet,
    geom = "polygon"
  ) +
  theme_bw(10) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = 'none') +
  scale_fill_gradient(low=colo[2], high = 'darkblue')+
  scale_alpha(range = c(0.5,0.9), guide=FALSE) +
  labs(fill = 'Dichte') +
  xlim(9.035, 9.32) + ylim(48.69, 48.87) +
  facet_wrap(~ Meinung, nrow = 1)
gk11

saveRDS(gk11, 'gk11.rds')

##########################################################
# Exp für Umfrage

pred.pop.u <- as.data.frame(pred.pop.u)
names(pred.pop.u) <- c('1', '2', '3', 'X', 'Y', 'Stadtteil', 'Meinung.zu.Stuttgart.21')
# selcting variables of interest
myvar <- c('Meinung.zu.Stuttgart.21', 'X', 'Y')
ST22 <- pred.pop.u[myvar]
ST21 <- ST22
# numerical classes into factor classes
ST21$Meinung.zu.Stuttgart.21 <- ''
ST21$Meinung.zu.Stuttgart.21[ST22$Meinung.zu.Stuttgart.21 == 1] <- 'Zustimmung'
ST21$Meinung.zu.Stuttgart.21[ST22$Meinung.zu.Stuttgart.21 == 3] <- 'Ablehnung'
# assigning names
names(ST21) <- c('Meinung', 'long', 'lat') 
ST21$long <- as.numeric(as.character(ST21$long))
ST21$lat <- as.numeric(as.character(ST21$lat))
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
ST21.g$Meinung <- factor(ST21.g$Meinung, levels = c('Zustimmung', 'Ablehnung'))
bezirke.t <- spTransform(bezirke, CRS("+proj=longlat"))

# erstellen des neuen data frames

# Plotting map
gk2 <- ggmap(map, extent = 'device', legend = 'topright') + geom_polygon(data=bezirke.t, aes(x=long, y=lat, group=group),colour="black", alpha=0) +
  stat_density2d(
    aes(x = long, y = lat, fill = ..level..,  alpha = ..level..),
    size = 2, bins = 8, data = ST21.g,
    geom = "polygon"
  ) +
  theme_bw(10) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = 'none') +
  scale_fill_gradient(low=colo[2], high = 'darkblue')+
  scale_alpha(range = c(0.7,0.1), guide=FALSE) +
  labs(fill = 'Dichte') +
  xlim(9.035, 9.32) + ylim(48.69, 48.87) +
  facet_wrap(~ Meinung, nrow = 1)
gk2

saveRDS(gk2, 'gk2.rds')

##########################################################
# Exp für Zensus

pred.pop.z <- as.data.frame(pred.pop.z)
names(pred.pop.z) <- c('1', '2', '3', 'X', 'Y', 'Stadtteil', 'Meinung.zu.Stuttgart.21')
# selcting variables of interest
myvar <- c('Meinung.zu.Stuttgart.21', 'X', 'Y')
ST22 <- pred.pop.z[myvar]
ST21 <- ST22
# numerical classes into factor classes
ST21$Meinung.zu.Stuttgart.21 <- ''
ST21$Meinung.zu.Stuttgart.21[ST22$Meinung.zu.Stuttgart.21 == 1] <- 'Zustimmung'
ST21$Meinung.zu.Stuttgart.21[ST22$Meinung.zu.Stuttgart.21 == 3] <- 'Ablehnung'
# assigning names
names(ST21) <- c('Meinung', 'long', 'lat') 
ST21$long <- as.numeric(as.character(ST21$long))
ST21$lat <- as.numeric(as.character(ST21$lat))
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
ST21.g$Meinung <- factor(ST21.g$Meinung, levels = c('Zustimmung', 'Ablehnung'))
bezirke.t <- spTransform(bezirke, CRS("+proj=longlat"))

# erstellen des neuen data frames

# Plotting map
gk3 <- ggmap(map, extent = 'device', legend = 'topright') + geom_polygon(data=bezirke.t, aes(x=long, y=lat, group=group),colour="black", alpha=0) +
  stat_density2d(
    aes(x = long, y = lat, fill = ..level..,  alpha = ..level..),
    size = 2, bins = 8, data = ST21.g,
    geom = "polygon"
  ) +
  theme_bw(10) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = 'none') +
  scale_fill_gradient(low=colo[2], high = 'darkblue')+
  scale_alpha(range = c(0.7,0.1), guide=FALSE) +
  labs(fill = 'Dichte') +
  xlim(9.035, 9.32) + ylim(48.69, 48.87) +
  facet_wrap(~ Meinung, nrow = 1)
gk3

saveRDS(gk3, 'gk3.rds')
