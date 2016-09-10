validation <- function(pred, valid){
  if(!all(pred[,1] == valid[, 1])){
    cat('Achtung: Die Namen der Bezirke stimmen nicht überein!')
    }
  if(ncol(pred) > 4){
  pred <- pred[,-1]
  medianr <- pred[,c((ncol(pred)-2), (ncol(pred)-1), (ncol(pred)))]
  medianr <- medianr /rowSums(medianr) # Anteil bilden
  valid <- valid[,-1] * 0.01
  valid <- cbind(valid[,2], valid[,1])
  medianr <- medianr[,-2]
  mse <- rep(0, ncol(medianr))
  MSE <- function(x, y){
    sum((x - y)^2)
  }
  
  for(i in 1:ncol(medianr)){
    mse[i] <- MSE(medianr[i], valid[i]) 
  }
  
  lowerb <- pred[,c(1,3)]/rowSums(pred[,c((ncol(pred)-2), (ncol(pred)-1), (ncol(pred)))])
  upperb <- pred[,c(4,6)]/rowSums(pred[,c((ncol(pred)-2), (ncol(pred)-1), (ncol(pred)))])
  
  coverage <- rep(0, 2)
  for(i in 1:2){
  coverage[i] <- sum((lowerb[,i] <= valid[,i])*
                    (upperb[,i] >= valid[,i]))/nrow(pred)
  }
  
  cat("\nMittlerer quadratischer Prognosefehler Zustimmung: ", mse[1], "\n", sep="")
  cat("\nMittlerer quadratischer Prognosefehler Ablehnung: ", mse[2], "\n", sep="")
  cat("\nUeberdeckungsw'keit der Vorhersagen:    ", coverage[1], "\n\n", sep="")
  cat("Ueberdeckungsw'keit der Vorhersagen:    ", coverage[2], "\n\n", sep="")
  }else{
    pred <- pred[,-1]
    if(is.vector(pred)){
      pred <- cbind((1 - pred), pred)
    }else{
      pred <- pred/rowSums(pred)
      pred <- pred[,-2]
    }
    valid <- valid[,-1] * 0.01
    valid <- cbind(valid[,2], valid[,1])

    mse <- rep(0, ncol(pred))
    MSE <- function(x, y){
      sum((x - y)^2)
    }
    
    for(i in 1:ncol(pred)){
      mse[i] <- MSE(pred[i], valid[i]) 
    }
    medianr <- pred
    cat("\nMittlerer quadratischer Prognosefehler Zustimmung: ", mse[1], "\n", sep="")
    cat("\nMittlerer quadratischer Prognosefehler Ablehnung: ", mse[2], "\n", sep="")
  }
  
  
  x <- rep('Zustimmung', nrow(medianr))
  Kategorie1 <- cbind(x, medianr[,1], valid[,1])
  x <- rep('Ablehnung', nrow(medianr))
  Kategorie2 <- cbind(x, medianr[,2], valid[,2])
  D <- as.data.frame(rbind(Kategorie1, Kategorie2))
  #names(D) <- c('x', 'V3', 'V2')
  D$V2 <- as.numeric(as.character(D$V2)); D$V3 <- as.numeric(as.character(D$V3));
  
  ggplot(D, aes(x = V3, y = V2)) + geom_point() + labs(x = 'Wahrheit', y = 'Geschätzt' ) +
    theme_bw(12) + coord_fixed(1) + geom_abline(intercept = 0, slope = 1) +
    xlim(0.2, 0.75) + 
    ylim(0.2, 0.75) +
    facet_wrap(~ x)
}