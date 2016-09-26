
plot(m1, select = 1, scheme="heat")

tt[[1]]

m1[["smooth"]][[1]]$S

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
