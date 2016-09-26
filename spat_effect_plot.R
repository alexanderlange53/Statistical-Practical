# Ausführen von 'call_Stuttgart21' bis Zeile 404
spat.plot.disc <- function(m1, IFbezirk = TRUE) {
  require(colorspace)
  tt <- plot(m1, select = 1)
  dev.off()
  dat <- data.frame(coef = tt[[1]]$fit, region = as.character(levels(tt[[1]]$raw)))
  
  if(IFbezirk) { # Ab hier für Bezirke als räumlichem effekt
    geo <- bezirke # Hier Wahl Bez./ST
    colo <- diverge_hsv(3)
    dat$id <- seq(1, nrow(dat))
    colnames(dat) <- c('coef', 'STADTBEZIR', 'id')
    # ID variable erzeugen um objecte zu verbinden
    geo@data$id <- rownames(geo@data)
    watershedPoints <- fortify(geo, region = "id")
    geo <- merge(watershedPoints, geo@data, by = 'id', all.x = T)
    # Zusammenfügen von DAten und spatial object
    bbA <- merge(geo, dat, by = 'STADTBEZIR', all.x = T)
    bbA <- bbA[order(bbA$order),]
  } else {
    # Ab hier für Stadtteile 
    geo <- stadtteile # Hier Wahl Bez./ST
    colo <- diverge_hsv(3)
    dat$id <- seq(1, nrow(dat))
    colnames(dat) <- c('coef', 'STADTTEIL', 'id')
    # ID variable erzeugen um objecte zu verbinden
    geo@data$id <- rownames(geo@data)
    watershedPoints <- fortify(geo, region = "id")
    geo <- merge(watershedPoints, geo@data, by = 'id', all.x = T)
    # Zusammenfügen von DAten und spatial object
    bbA <- merge(geo, dat, by = 'STADTTEIL', all.x = T)
    bbA <- bbA[order(bbA$order),]
  }
  
  
  # Ab hier für beide wieder
  geo.df <- bbA
  # Plotten 
  ret.plot <- ggplot(data = geo.df, aes(x = long, y = lat, group = group, fill = coef, alpha = coef))+  
    geom_polygon(color = "black") +
    labs(x=NULL, y=NULL, title= NULL) +
    scale_fill_gradient(name = "Koef.", low = colo[2], high = 'darkblue', guide = "colorbar") +
    scale_alpha(range = c(0.8,1), guide=FALSE) +
    coord_equal(1)+
    theme_bw(12) +
    theme(
      legend.position = 'right'
      ,axis.text.x=element_blank()
      ,axis.text.y=element_blank()
      ,axis.ticks.y=element_blank()
      ,axis.ticks.x=element_blank()
    )
  return(ret.plot)
}
  
spat.plot.cont <- function(m1) {
# Call Stuttgart21 ausführen bis Z. 158
require(colorspace)
colo <- diverge_hsv(3)

tt <- mgcv::plot.gam(m1, pers = TRUE, pages = 1)
#str(tt[[1]])

#persp(x = tt[[1]]$x, y = tt[[1]]$y, z = matrix(tt[[1]]$fit, nrow = length(tt[[1]]$x)), zlim = c(-0.1,0.1))

mat <- matrix(tt[[1]]$fit, nrow = length(tt[[1]]$x))
dev.off()
colnames(mat) <- tt[[1]]$x
rownames(mat) <- tt[[1]]$y

ggmat <- as.data.frame(melt(mat))
names(ggmat) <- c("y", "x", "coef")

attr(m1$smooth[[1]], "qrc")

# Idea 1: ggplot
ret.plot <- ggplot(ggmat, aes(x = x, y = y, fill = coef), show.legend = FALSE) + geom_tile() + 
  scale_fill_gradient(low = colo[2],  high = "darkblue", na.value = 'transparent') + coord_equal() + coord_flip() + 
  stat_contour(data = ggmat, aes(x = x, y = y, z = coef), colour = 1) +
  theme_bw(13) + theme(axis.text = element_blank(),
                       axis.ticks = element_blank()) +
  scale_alpha(range = c(0.1,1), guide=FALSE) + 
  labs(x = NULL, y = NULL, fill = 'Koef.')

return(ret.plot)

}