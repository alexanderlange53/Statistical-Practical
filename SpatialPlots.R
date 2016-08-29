GKPlot <- function(dataS,  bezirke, response = 'Meinung.zu.Stuttgart.21', Kategorien = 5){
  # selcting variables of interest
  require(colorspace)
  colo <- diverge_hsv(3)
  myvar <- c(response, 'X', 'Y')
  ST22 <- dataS[myvar]
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
      if(response == 'Meinung.zu.Stuttgart.21'){
        S3 <- ST21.g[which(ST21.g$Response=='Sehr gut' | ST21.g$Response=='Gut'),]
        S32 <- ST21.g[which(ST21.g$Response=='Neutral'),]
        S4 <- ST21.g[which(ST21.g$Response=='Schlecht' | ST21.g$Response=='Sehr schlecht'),]
      }else{
        S3 <- ST21.g[which(ST21.g$Response=='Sehr gut'),]
        S32 <- ST21.g[which(ST21.g$Response=='Gut'),]
        S4 <- ST21.g[which(ST21.g$Response=='Neutral' | ST21.g$Response=='Schlecht' | ST21.g$Response=='Sehr schlecht'),] 
      }
      
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

SpatAntPlot <- function(dataS,  bezirke, response = 'Meinung.zu.Stuttgart.21', Kategorien = 5, Bezirke = T){
  require(colorspace)
  if(Bezirke == T){
  if(response ==  'Meinung.zu.Stuttgart.21'){
    if(Kategorien == 5){
    for(i in 1:nrow(dataS)){
      if(dataS$Meinung.zu.Stuttgart.21[i] == 6){
        dataS$Meinung.zu.Stuttgart.21[i] <- NA
      }}
      dataS <- na.omit(dataS)
    }else{
     dataS <- DataPrep(dataS, binom = F)
    }
  }else{
    if(Kategorien == 5){
    for(i in 1:nrow(dataS)){
      if(dataS$Bewertung.Wohngegend[i] == 6){
        dataS$Bewertung.Wohngegend[i] <- NA
      }}
    dataS <- na.omit(dataS)
    }else{
      dataS <- DataPrep(dataS, binom = F, Stuttgart21 = F)
    }
  }
  colo <- diverge_hsv(3)
  myvar <- c(response, 'Stadtbezirk', 'X', 'Y')
  ST <- dataS[myvar]
  
  # Ermittlung der Zustimmung in den Stadtteilen ('Sehr gut')
  beob.bez <- as.data.frame(table(ST[,2]))
  meinung.bez <-as.data.frame(table(ST$Stadtbezirk, ST[, 1]))
  
  # ID variable erzeugen um Data Frame und Spatial object zu verbinden
  bezirke@data$id <- rownames(bezirke@data)
  watershedPoints <- fortify(bezirke, region = "id")
  
  # Errechneten Anteile und räumliche Informationen verbinden
  bb <- merge(watershedPoints, bezirke@data, by = 'id', all.x = T)
  
  # Relative Anteile 
  Anteile <- by(meinung.bez$Freq, meinung.bez$Var2, 
                function(x){(x/beob.bez$Freq)*100})
  Anteile <- as.data.frame(lapply(Anteile, unlist))
  Anteile$STADTBEZIR <- levels(meinung.bez$Var1)
  if(Kategorien == 5){
    colnames(Anteile) <- c('Sehr gut', 'Gut', 'Neutral', 'Schlecht',
                         'Sehr schlecht', 'STADTBEZIR')
  }else{
    colnames(Anteile) <- c('Zustimmung', 'Neutral', 
                           'Ablehnung', 'STADTBEZIR')
  }
  AnteileM <- melt(Anteile, id = 'STADTBEZIR')
  bbA <- merge(bb, AnteileM, by = 'STADTBEZIR')
  
  # Sortieren damit Poylogene richtig geplottet werden
  bbA <- bbA[order(bbA$order),]
  b.facet <- bbA
  
  # Plotten 
  ggplot(data=b.facet, aes(x=long, y=lat, group=group, fill = value, alpha = value))+  
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
    ) + facet_wrap(~ variable)
  }else{
      if(response ==  'Meinung.zu.Stuttgart.21'){
        if(Kategorien == 5){
          for(i in 1:nrow(dataS)){
            if(dataS$Meinung.zu.Stuttgart.21[i] == 6){
              dataS$Meinung.zu.Stuttgart.21[i] <- NA
            }}
          dataS <- na.omit(dataS)
        }else{
          dataS <- DataPrep(dataS, binom = F)
        }
      }else{
        if(Kategorien == 5){
          for(i in 1:nrow(dataS)){
            if(dataS$Bewertung.Wohngegend[i] == 6){
              dataS$Bewertung.Wohngegend[i] <- NA
            }}
          dataS <- na.omit(dataS)
        }else{
          dataS <- DataPrep(dataS, binom = F, Stuttgart21 = F)
        }
      }
    
      Stadtteile <- bezirke
      colo <- diverge_hsv(3)
      myvar <- c(response, 'Stadtteil', 'X', 'Y')
      ST <- dataS[myvar]
    
    # Ermittlung der Zustimmung in den Stadtteilen
    beob.teile <- as.data.frame(table(ST[,2]))
    meinung.teile <-as.data.frame(table(ST[,2], ST[,1]))
    
    # ID variable erzeugen um objecte zu verbinden
    Stadtteile@data$id <- rownames(Stadtteile@data)
    watershedPoints <- fortify(Stadtteile, region = "id")
    
    # Errechneten Anteile und räumliche Informationen verbinden
    bb <- merge(watershedPoints, Stadtteile@data, by = 'id', all.x = T)
    
    # Relative Anteile 
    Anteile <- by(meinung.teile$Freq, meinung.teile$Var2, 
                  function(x){(x/beob.teile$Freq)*100})
    Anteile <- as.data.frame(lapply(Anteile, unlist))
    Anteile$STADTTEIL <- levels(meinung.teile$Var1)
    if(Kategorien == 5){
      colnames(Anteile) <- c('Sehr gut', 'Gut', 'Neutral', 'Schlecht',
                             'Sehr schlecht', 'STADTTEIL')
    }else{
      colnames(Anteile) <- c('Zustimmung', 'Neutral', 
                             'Ablehnung', 'STADTTEIL')
    }
    
    AnteileM <- melt(Anteile, id = 'STADTTEIL')
    bbA <- merge(bb, AnteileM, by = 'STADTTEIL', all.x = T)
    
    # Sortieren damit Poylogene richtig geplottet werden
    bbA <- bbA[order(bbA$order),]
    s.facet <- bbA
    pol.na <- filter(s.facet, is.na(variable))
    plo.na <- subset(pol.na, select = c(STADTTEIL, id, long, lat, order, group))
    s.facet <- na.omit(s.facet)
    
    ggplot() + geom_polygon(data = plo.na, aes(x = long, y = lat, group = group), fill = 'black') +
      geom_polygon(data=s.facet, aes(x=long, y=lat, group=group, fill = value, alpha = value), color = "black") +
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
      ) + facet_wrap(~ variable)
  }
}
  
PredSpatPlot <- function(predz, spatdata){
  require(colorspace)
  colo <- diverge_hsv(3)
  Agg <- Prediction.Aggregation(pred = predz[, c(1 : 3, 6)], agg = 'Stadtteil')
  count <- as.data.frame(table(predz$Stadtteil))
  Anteile <- as.data.frame(apply(Agg[,-1], 2, function(x){x/count$Freq}))
  
  # ID variable erzeugen um objecte zu verbinden
  spatdata@data$id <- rownames(spatdata@data)
  watershedPoints <- fortify(spatdata, region = "id")
  
  # Errechneten Anteile und räumliche Informationen verbinden
  bb <- merge(watershedPoints, spatdata@data, by = 'id', all.x = T)
  
  Anteile$Stadteile <- Agg$Stadtteil
  if(ncol(Anteile) == 6){
    colnames(Anteile) <- c('Sehr gut', 'Gut', 'Neutral', 'Schlecht',
                           'Sehr schlecht', 'STADTTEILN')
  }else{
    colnames(Anteile) <- c('Zustimmung', 'Neutral', 
                           'Ablehnung', 'STADTTEILN')
  }
  
  AnteileM <- melt(Anteile, id = 'STADTTEILN')
  bbA <- merge(bb, AnteileM, by = 'STADTTEILN', all.x = T)
  
  # Sortieren damit Poylogene richtig geplottet werden
  bbA <- bbA[order(bbA$order),]
  s.facet <- bbA
  pol.na <- filter(s.facet, is.na(variable))
  plo.na <- subset(pol.na, select = c(STADTTEILN, id, long, lat, order, group))
  s.facet <- na.omit(s.facet)
  
  ggplot() + geom_polygon(data = plo.na, aes(x = long, y = lat, group = group), fill = 'black') +
    geom_polygon(data=s.facet, aes(x=long, y=lat, group=group, fill = value, alpha = value), color = "black") +
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
    ) + facet_wrap(~ variable)
}  