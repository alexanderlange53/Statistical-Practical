########################
# Spatial exploratives #
########################

# loading packages
require(ggplot2);require(reshape2);require(colorspace);require(gridExtra);require(scales)
require(rgdal);require(rgeos);require(ggmap)

# loading data
dataS <- read.csv2('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Auswertung/Neue_Daten/Stuttgart21_aufbereitet.csv',
                   dec = '.')
bezirke <- readOGR(dsn = "/home/alex/Schreibtisch/Uni/statistisches_praktikum/Auswertung/Geodaten/bezirke", layer = "bezirke")
# selcting variables of interest
myvar <- c('Meinung.zu.Stuttgart.21', 'X', 'Y')
ST21 <- dataS[myvar]
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
proj4string(ST21) <- CRS("+init=epsg:4839")
# reproject data
ST21 <- spTransform(ST21, CRS("+proj=longlat +datum=WGS84"))
# loading map
map <- get_map(location=rowMeans(bbox(ST21)), zoom=12, maptype = 'roadmap')
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
  )  + theme(
    legend.position = 'none'
    ,axis.text.x=element_blank()
    ,axis.text.y=element_blank()
    ,axis.ticks.y=element_blank()
    ,axis.ticks.x=element_blank()
  ) + labs(title = 'Stichprobe')
g1
