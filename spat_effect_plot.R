# Call Stuttgart21 ausführen bis Z. 158
library(colorspace)
colo <- diverge_hsv(3)


summary(m1)
str(m1$smooth[[1]]$S)

m1$smooth[[1]]$S
#persp(x = c(1 : 29), y = c(1 : 29), z = matrix(unlist(m1$smooth[[1]]$S), ncol = 29))


#plot(m1, persp = TRUE)

tt <- mgcv::plot.gam(m1, pers = TRUE)
str(tt[[1]])

matrix(tt[[1]]$fit, nrow = length(tt[[1]]$x))

#persp(x = tt[[1]]$x, y = tt[[1]]$y, z = matrix(tt[[1]]$fit, nrow = length(tt[[1]]$x)), zlim = c(-0.1,0.1))

mat <- matrix(tt[[1]]$fit, nrow = length(tt[[1]]$x))
colnames(mat) <- tt[[1]]$x
rownames(mat) <- tt[[1]]$y

ggmat <- as.data.frame(melt(mat))
names(ggmat) <- c("y", "x", "coef")

# Idea 1: ggplot
ggplot(ggmat, aes(x = x, y = y, fill = coef,  alpha = coef), show.legend = FALSE) + geom_tile() + 
  scale_fill_gradient(low = colo[2],  high = "darkblue") +
  theme_bw(13) + theme(axis.text = element_blank(),
                       axis.ticks = element_blank()) +
  scale_alpha(range = c(0.1,0.7), guide=FALSE) +
  labs(x = NULL, y = NULL, fill = 'Koef.')



# Idea 2_ With Contour
spat <- cbind(spat, tt[[1]]$fit)

par(mar = c(0, 1.5, 1.5, 0) + 0.5)
image(x = tt[[1]]$x, y = tt[[1]]$y, z = matrix(tt[[1]]$fit, nrow = length(tt[[1]]$x)), xlab = "", ylab = "", axes = F, col = colorRampPalette(brewer.pal(9, "Blues"))(500))
contour(x = tt[[1]]$x, y = tt[[1]]$y, z = matrix(tt[[1]]$fit, nrow = length(tt[[1]]$x)), add = TRUE)
mtext(side = 3, line = 0.5, "Kontinuierlicher räumlicher Effekt")
box()
