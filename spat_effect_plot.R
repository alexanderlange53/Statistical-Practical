# Ausführen von 'call_Stuttgart21' bis Zeile 404

tt <- plot(m1, select = 1)
dat <- data.frame(coef = tt[[1]]$fit, region = as.character(levels(tt[[1]]$raw)))


require(colorspace)

  geo <- bezirke # Hier Wahl Bez./ST
  colo <- diverge_hsv(3)
  
  # Merge: Geo, gam coef.
  geo@data <- merge(geo@data, dat, by.x = "STADTBEZIR", by.y = "region")
  
  
  # Plotten 
  ggplot(data=b.facet, aes(x=long, y=lat, group=group, fill = value, alpha = value))+  
    geom_polygon(color = "black") +
    labs(x=NULL, y=NULL, title= NULL) +
    scale_fill_gradient(name = "Anteil \n in %", low = colo[2], high = 'darkblue', guide = "colorbar",
                        breaks = pretty_breaks(n = 5)) +
    scale_alpha(range = c(0.8,1), guide=FALSE) +
    coord_equal(1)+
    theme_bw(12) +
    theme(
      legend.position = 'bottom'
      ,axis.text.x=element_blank()
      ,axis.text.y=element_blank()
      ,axis.ticks.y=element_blank()
      ,axis.ticks.x=element_blank()
    ) + facet_wrap(~ variable)



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

# # Idea 2_ With Contour
# par(mar = c(0, 1.5, 1.5, 0) + 0.5)
# image(x = tt[[1]]$x, y = tt[[1]]$y, z = matrix(tt[[1]]$fit, nrow = length(tt[[1]]$x)), xlab = "", ylab = "", axes = F, col = colorRampPalette(brewer.pal(9, "Blues"))(500))
# contour(x = tt[[1]]$x, y = tt[[1]]$y, z = matrix(tt[[1]]$fit, nrow = length(tt[[1]]$x)), add = TRUE)
# mtext(side = 3, line = 0.5, "Kontinuierlicher räumlicher Effekt")
# box()
