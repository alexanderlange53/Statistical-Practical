validation <- function(pred, valid){
  if(!all(pred[,1] == valid[, 1])){cat('Achtung: Die Namen der Bezirke stimmen nicht überein!')}
  pred <- pred[,-1]
  pred <- pred / apply(pred, 1, sum) # Anteil bilden
  valid <- valid[,-1]*0.01
  mse <- rep(0, ncol(pred))
  MSE <- function(x, y){
    sum((x - y)^2)
  }
  
  for(i in 1:ncol(pred)){
    mse[i] <- MSE(pred[i], valid[i]) # Müsste hier bei pred nicht ein relativer Anteil statt der absoluten Einwohnerzahl übergeben werden? Sieh Z. 4 Oder halt den Anteil in eine abs. Zashl umrechnen.
  }
  
  x <- rep('Zustimmung', nrow(pred))
  Kategorie1 <- cbind(x, pred[,1], valid[,1])
  x <- rep('Ablehnung', nrow(pred))
  Kategorie2 <- cbind(x, pred[,2], valid[,2])
  D <- as.data.frame(rbind(Kategorie1, Kategorie2))
  D$V2 <- as.numeric(as.character(D$V2)); D$V3 <- as.numeric(as.character(D$V3));

  cat("\nMittlerer quadratischer Prognosefehler Kategorie 1: ", mse[1], "\n", sep="")
  cat("\nMittlerer quadratischer Prognosefehler Kategorie 3: ", mse[2], "\n", sep="")
  
  
  ggplot(D, aes(x = V2, y = V3)) + geom_point() + labs(x = 'Wahrheit', y = 'Geschätzt' ) +
    theme_bw(12) + coord_fixed(1) + geom_abline(intercept = 0, slope = 1) +
    xlim(0.2, 0.75) + 
    ylim(0.2, 0.75) +
    facet_wrap(~ x)
}