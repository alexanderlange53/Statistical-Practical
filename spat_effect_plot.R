# Call Stuttgart21 ausführen bis Z. 158

summary(m1)
str(m1$smooth[[1]]$S)

m1$smooth[[1]]$S
#persp(x = c(1 : 29), y = c(1 : 29), z = matrix(unlist(m1$smooth[[1]]$S), ncol = 29))


#plot(m1, persp = TRUE)

tt <- mgcv::plot.gam(m1, pers = TRUE)
str(tt[[1]])

matrix(tt[[1]]$fit, nrow = length(tt[[1]]$x))

persp(x = tt[[1]]$x, y = tt[[1]]$y, z = matrix(tt[[1]]$fit, nrow = length(tt[[1]]$x)))

spat <- data.frame(x = numeric() , y = numeric())
for(i in c(1 : length(tt[[1]]$x))) {
  
  for(j in c(1 : length(tt[[1]]$y))) {
    temp.df <- data.frame(x = tt[[1]]$x[i], y = tt[[1]]$y[j])
    spat <- rbind(spat, temp.df)
  }
  
}

spat <- cbind(spat, tt[[1]]$fit)
names(spat)[3] <- "para"

image(x = tt[[1]]$x, y = tt[[1]]$y, z = matrix(tt[[1]]$fit, nrow = length(tt[[1]]$x)))
contour(x = tt[[1]]$x, y = tt[[1]]$y, z = matrix(tt[[1]]$fit, nrow = length(tt[[1]]$x)), add = TRUE)

# Convert Spat into a spatial point object
spat.save <- spat

# transform to spatial class
spat$y <- as.numeric(as.character(spat$y)) # long
spat$x <- as.numeric(as.character(spat$x)) # lat
coordinates(spat) <- ~ y + x
# assign CRS
proj4string(spat) <- CRS("+init=epsg:31467")
# reproject data
spat <- spTransform(spat, CRS("+proj=longlat +datum=WGS84"))
# loading map
map <- get_map(location= rowMeans(bbox(ST21)), zoom=11, maptype = 'terrain', scale = 2)
# transform back to data frame for ggplot
spat.g <- as.data.frame(spat)
# spatial transformation
bezirke.t <- spTransform(bezirke, CRS("+proj=longlat"))


  # plotting map
  ggmap(map, extent = 'device', legend = 'bottom') + geom_polygon(data=bezirke.t, aes(x=long, y=lat, group=group),colour="black", alpha=0) +
    stat_density2d(
      aes(x = long, y = lat, fill = ..level..,  alpha = ..level..),
      size = 2, bins = 8, data = data.f,
      geom = "polygon"
    ) + geom_density2d(data = data.f, 
                       aes(x = long, y = lat, color = ..level..), size = 0.3, show.legend = F) +
    theme_bw(13) + theme(axis.text = element_blank(), legend.position = 'bottom',
                         axis.ticks = element_blank()) +
    scale_fill_gradient(low=colo[2], high = 'darkblue')+
    scale_color_gradient(low=colo[2], high = 'darkblue', guide = F)+
    scale_alpha(range = c(0.1,0.7), guide=FALSE) +
    labs(x = NULL, y = NULL, fill = 'Dichte') +
    xlim(9.035, 9.32) + ylim(48.69, 48.87) +
    facet_wrap(~ Response)
  
  # plotting map: spat. effect
  ggmap(map, extent = 'device', legend = 'bottom') +
    stat_density2d(
      aes(x = x, y = y, fill = ..level..,  alpha = ..level..),
      size = 2, bins = 8, data = spat.g,
      geom = "polygon"
    ) 
    scale_fill_gradient(low=colo[2], high = 'darkblue') +
    scale_color_gradient(low=colo[2], high = 'darkblue', guide = F) +
   
    scale_alpha(range = c(0.1,0.7), guide=FALSE) +
    labs(x = NULL, y = NULL, fill = 'Dichte') +
    xlim(9.035, 9.32) + ylim(48.69, 48.87)
#}
