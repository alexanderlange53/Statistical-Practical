GKPlot <- function(dataS,  bezirke, response = c('Meinung.zu.Stuttgart.21', 'X', 'Y'), Kategorien = 5){
  # selcting variables of interest
  require(colorspace)
  colo <- diverge_hsv(3)
  ST22 <- dataS[paste(response)]
  ST21 <- ST22
  # numerical classes into factor classes
  ST21[,1] <- ''
  ST21[,1][ST22[,1] == 1] <- 'Sehr gut'
  ST21[,1][ST22[,1] == 2] <- 'Gut'
  ST21[,1][ST22[,1] == 3] <- 'Neutral'
  ST21[,1][ST22[,1] == 4] <- 'Schlecht'
  ST21[,1][ST22[,1] == 5] <- 'Sehr schlecht'
  ST21[,1][ST22[,1] == 6] <- 'Keine Angabe'
  # assigning names
  names(ST21) <- c('Response', 'long', 'lat') 
  # reshaping data frame for plotting
  ST21.p <- melt(ST21, id = c('long', 'lat'))
  # levels of factor
  ST21.p$value <- factor(ST21.p$value, levels = c('Sehr gut', 'Gut', 'Neutral', 'Schlecht', 
                                                  'Sehr schlecht', 'Keine Angabe'))
  
  # transform to spatial class
  ST21$long <- as.numeric(as.character(ST21$long))
  ST21$lat <- as.numeric(as.character(ST21$lat))
  coordinates(ST21) <- ~ long + lat
  # assign CRS
  proj4string(ST21) <- CRS("+init=epsg:31467")
  # reproject data
  ST21 <- spTransform(ST21, CRS("+proj=longlat +datum=WGS84"))
  # loading map
  map <- get_map(location= rowMeans(bbox(ST21)), zoom=11, maptype = 'terrain', scale = 2)
  # transform back to data frame for ggplot
  ST21.g <- as.data.frame(ST21)
  # spatial transformation
  bezirke.t <- spTransform(bezirke, CRS("+proj=longlat"))
  
  
    # getting ratings
    SS <- ST21.g[which(ST21.g$Response=='Sehr gut'),]
    SS2 <- ST21.g[which(ST21.g$Response=='Gut'),]
    SS3 <- ST21.g[which(ST21.g$Response=='Neutral'),]
    SS4 <- ST21.g[which(ST21.g$Response=='Schlecht'),]
    SS5 <- ST21.g[which(ST21.g$Response=='Sehr schlecht'),]
    
    # erstellen des neuen data frames
    data.f <- rbind(SS, SS2, SS3, SS4, SS5)
    data.f$Response <- factor(data.f$Response, levels = c('Sehr gut', 'Gut', 'Neutral', 'Schlecht', 'Sehr schlecht'))
    if(Kategorien == 3){
      S3 <- ST21.g[which(ST21.g$Response=='Sehr gut' | ST21.g$Response=='Gut'),]
      S32 <- ST21.g[which(ST21.g$Response=='Neutral'),]
      S4 <- ST21.g[which(ST21.g$Response=='Schlecht' | ST21.g$Response=='Sehr schlecht'),]
      
      # erstellen des neuen data frames
      S3$Response <- 'Zustimmung'
      S4$Response <- 'Ablehnung'
      data.f <- rbind(S3, S32, S4)
      data.f$Response <- factor(data.f$Response, levels = c('Zustimmung', 'Neutral', 'Ablehnung'))
    }
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
      facet_wrap(~ Response)
}
  