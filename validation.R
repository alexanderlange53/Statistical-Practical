validation <- function(pred, valid, pop = pop, errorbar = F){
  require(colorspace)
  colo <- diverge_hsv(3)
  if(!all(pred[,1] == valid[, 1])){
    cat('Achtung: Die Namen der Bezirke stimmen nicht überein!')
  }
  if(nrow(pred) > 30){
    popagg <- as.data.frame(table(pop$Stadtteil))
  }else{
    popagg <- as.data.frame(table(pop$Stadtbezirk))
  }
  if(ncol(pred) > 4){
  pred <- pred[,-1]
    if(ncol(pred) > 4){
      medianr <- pred[,c((ncol(pred)-2), (ncol(pred)-1), (ncol(pred)))]
      medianr <- medianr / rowSums(medianr) # Anteil bilden
      medianr <- medianr[,-2]
      lowerb  <- pred[,c(1,3)]/popagg[,2]
      upperb <- pred[,c(4,6)]/popagg[,2]
      #lowerb <- pred[,c(1,3)]/rowSums(pred[,c((ncol(pred)-2), (ncol(pred)-1), (ncol(pred)))])
      #upperb <- pred[,c(4,6)]/rowSums(pred[,c((ncol(pred)-2), (ncol(pred)-1), (ncol(pred)))])
    }else{
      medianr <- pred[,4]
      medianr <- as.data.frame(medianr / popagg[,2])
      medianr$Zustimmung <- 1 - medianr
      medianr <- cbind(medianr[,2], medianr[,1])
      lowerb <- as.data.frame(pred[,1]/popagg[,2])
      upperb <- as.data.frame(pred[,2]/popagg[,2])
      upperb$k <- 1 - lowerb
      lowerb$k <- 1 - upperb[,1]
      lowerb <- cbind(lowerb[,2], lowerb[,1])
      upperb <- cbind(upperb[,2], upperb[,1])
    }
  valid <- valid[,-1] * 0.01
  valid <- cbind(valid[,2], valid[,1])
  mse <- rep(0, ncol(medianr))
  MSE <- function(x, y){
    sum((x - y)^2)
  }
  
  for(i in 1:ncol(medianr)){
    mse[i] <- MSE(medianr[,i], valid[,i]) 
  }
  
  coverage <- rep(0, 2)
  for(i in 1:2){
  coverage[i] <- sum((lowerb[,i] <= valid[,i])*
                    (upperb[,i] >= valid[,i]))/nrow(pred)
  }
  
  cat("\nMittlerer quadratischer Prognosefehler Zustimmung: ", round(mse[1], 3), "\n", sep="")
  cat("\nMittlerer quadratischer Prognosefehler Ablehnung: ", round(mse[2], 3), "\n", sep="")
  cat("\nUeberdeckungsw'keit der Vorhersagen:    ", round(coverage[1], 3), "\n\n", sep="")
  cat("Ueberdeckungsw'keit der Vorhersagen:    ", round(coverage[2], 3), "\n\n", sep="")
  
  x <- rep('Zustimmung', nrow(medianr))
  Kategorie1 <- cbind(x, medianr[,1], valid[,1], lowerb[,1], upperb[,1])
  x <- rep('Ablehnung', nrow(medianr))
  Kategorie2 <- cbind(x, medianr[,2], valid[,2], lowerb[,2], upperb[,2])
  D <- as.data.frame(rbind(Kategorie1, Kategorie2))
  D$V2 <- as.numeric(as.character(D$V2)); D$V3 <- as.numeric(as.character(D$V3))
  D$V4 <- as.numeric(as.character(D$V4)); D$V5 <- as.numeric(as.character(D$V5))
  
  if(errorbar == T){
    D <- subset(D, x == 'Zustimmung')
   plotv <- ggplot(D, aes(x = V3, y = V2)) + 
           geom_errorbar(aes(x = V3, ymin=V4, ymax=V5), width=.05, 
                                                 position=position_dodge(.05)) +
           geom_point(shape = 21, fill = colo[1], size = 3) + 
           labs(x = 'Wahre Anteile', y = 'Geschätzte Anteile' ) +
           theme_bw(12) + geom_abline(intercept = 0, slope = 1) +
           xlim(0.25, 0.9) + 
           ylim(0.25, 0.9) +
           annotate("text", x = 0.33, y = 0.85, label = paste("MSE: ", round(mse[1], 3))) +
           annotate("text", x = 0.33, y = 0.75, label = paste("Übw.: ", round(coverage[1], 3)))
           #facet_wrap(~ x)
   plotv
   return(plotv)
  }else{
    plotv <- ggplot(D, aes(x = V3, y = V2)) + 
            geom_point(shape = 21, fill = colo[1], size = 3) + 
            labs(x = 'Wahre Anteile', y = 'Geschätzte Anteile' ) +
            theme_bw(12) + coord_fixed(1) + geom_abline(intercept = 0, slope = 1) +
            xlim(0.15, 0.75) + 
            ylim(0.15, 0.75) +
            facet_wrap(~ x)
    plotv
    return(plotv)
  }
  
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
    
    pred <- as.data.frame(pred)
    valid <- as.data.frame(valid)
    for(i in 1:ncol(pred)){
      mse[i] <- MSE(pred[,i], valid[,i]) 
    }
    medianr <- pred
    cat("\nMittlerer quadratischer Prognosefehler Zustimmung: ", round(mse[1], 3), "\n", sep="")
    cat("\nMittlerer quadratischer Prognosefehler Ablehnung: ", round(mse[2], 3), "\n", sep="")
  
  x <- rep('Zustimmung', nrow(medianr))
  Kategorie1 <- cbind(x, medianr[,1], valid[,1])
  x <- rep('Ablehnung', nrow(medianr))
  Kategorie2 <- cbind(x, medianr[,2], valid[,2])
  D <- as.data.frame(rbind(Kategorie1, Kategorie2))
  D$V2 <- as.numeric(as.character(D$V2)); D$V3 <- as.numeric(as.character(D$V3));
  
 plotv <- ggplot(D, aes(x = V3, y = V2)) + geom_point(shape = 21, fill = colo[1]) + 
         #geom_errorbar(aes(x = V3, ymin=, ymax=len+sd)) +
         labs(x = 'Wahre Anteile', y = 'Geschätzte Anteile' ) +
         theme_bw(12) + coord_fixed(1) + geom_abline(intercept = 0, slope = 1) +
         xlim(0.2, 0.75) + 
         ylim(0.2, 0.75) +
         facet_wrap(~ x)
  plotv
  return(plotv)
  }

}

ResultPlot <- function(predlist, sample, models){
  require(colorspace)
  colo <- diverge_hsv(3)
  pr <- lapply(predlist, colSums)
  #ps <- lapply(predst, colSums)
  pr <- t(sapply(pr,unlist))
  #ps <- t(sapply(ps,unlist))
  medianr <- pr[,c((ncol(pr)-2), (ncol(pr)-1), (ncol(pr)))]
  medianr <- medianr /rowSums(medianr) # Anteil bilden
  #ms <- ps/rowSums(ps)
  lowerb <- pr[,c(1,2,3)]/rowSums(pr[,c((ncol(pr)-2), (ncol(pr)-1), (ncol(pr)))])
  upperb <- pr[,c(4,5,6)]/rowSums(pr[,c((ncol(pr)-2), (ncol(pr)-1), (ncol(pr)))])
  
  #colnames(ms) <- c('Zustimmung', 'Neutral', 'Ablehnung')
  colnames(medianr) <- c('Zustimmung', 'Neutral', 'Ablehnung')
  #medianr <- rbind(medianr, ms)
  mmedian <- melt(medianr)
  colnames(lowerb) <- c('Zustimmung', 'Neutral', 'Ablehnung')
  #lowerb <- rbind(lowerb, ms)
  mlowerb<- melt(lowerb)
  colnames(upperb) <- c('Zustimmung', 'Neutral', 'Ablehnung')
  #upperb <- rbind(upperb, ms)
  mupperb <- melt(upperb)
  DATA <- cbind(as.factor(models),mlowerb[,-1], mmedian[,3], mupperb[,3])
  names(DATA) <- c('model', 'Klasse', 'lower', 'med', 'upper')
  
  count1 <- as.data.frame(table(sample$Meinung.zu.Stuttgart.21))
  count1$Freq <- count1$Freq/sum(count1$Freq)
  
  ggplot(DATA, aes(x = model, y = med, group = Klasse, color = Klasse)) +
    geom_point(shape = 18, size = 4) + coord_flip() +
    scale_y_continuous(breaks = pretty(DATA$med, n = 10)) +
    scale_color_manual(values = c(colo[1], 'black', colo[3]), name = NULL) +
    geom_errorbar(aes(x = model, ymin = lower, ymax = upper), width = 0.5) +
    geom_hline(yintercept = 0.471, color = colo[3], size = 1) +
    geom_hline(yintercept = 0.529, color = colo[1], size = 1) +
    geom_hline(yintercept = count1$Freq[2], color = 'black', linetype = "dashed") +
    geom_hline(yintercept = count1$Freq[1], color = colo[1], linetype = "dashed") +
    geom_hline(yintercept = count1$Freq[3], color = colo[3], linetype = "dashed") +
    theme_bw(12) + labs(y = 'Anteil', x = NULL) + 
    theme(legend.position = 'bottom')
}

ResultPlot5 <- function(predlist, sample, models){
  require(colorspace)
  colo <- diverge_hsv(3)
  pr <- lapply(predlist, colSums)
  #ps <- lapply(predst, colSums)
  pr <- t(sapply(pr,unlist))
  #ps <- t(sapply(ps,unlist))
  medianr <- pr[,c((ncol(pr)-4), (ncol(pr)-3), (ncol(pr)-2), (ncol(pr)-1), (ncol(pr)))]
  medianr <- medianr /rowSums(medianr) # Anteil bilden
  #ms <- ps/rowSums(ps)
  lowerb <- pr[,c(1:5)]/rowSums(pr[,c((ncol(pr)-4), (ncol(pr)-3), (ncol(pr)-2), (ncol(pr)-1), (ncol(pr)))])
  upperb <- pr[,c(6:10)]/rowSums(pr[,c((ncol(pr)-4), (ncol(pr)-3), (ncol(pr)-2), (ncol(pr)-1), (ncol(pr)))])
  
  #colnames(ms) <- c('Sehr gut', 'Gut', 'Neutral', 'Schlecht', 'Sehr schlecht')
  colnames(medianr) <- c('Sehr gut', 'Gut', 'Neutral', 'Schlecht', 'Sehr schlecht')
  #medianr <- rbind(medianr, ms)
  mmedian <- melt(medianr)
  colnames(lowerb) <- c('Sehr gut', 'Gut', 'Neutral', 'Schlecht', 'Sehr schlecht')
  #lowerb <- rbind(lowerb, ms)
  mlowerb<- melt(lowerb)
  colnames(upperb) <- c('Sehr gut', 'Gut', 'Neutral', 'Schlecht', 'Sehr schlecht')
  #upperb <- rbind(upperb, ms)
  mupperb <- melt(upperb)
  DATA <- cbind(as.factor(models),mlowerb[,-1], mmedian[,3], mupperb[,3])
  names(DATA) <- c('model', 'Klasse', 'lower', 'med', 'upper')
  
  count1 <- as.data.frame(table(sample$Bewertung.Wohngegend))
  count1$Freq <- count1$Freq/sum(count1$Freq)
  
  ggplot(DATA, aes(x = model, y = med, group = Klasse, color = Klasse)) +
    geom_point(shape = 18, size = 4) + coord_flip() +
    scale_y_continuous(breaks = pretty(DATA$med, n = 10)) +
    scale_color_manual(values = c(colo[1], 'black', colo[3], 'grey50', 'cyan3'), name = NULL) +
    geom_errorbar(aes(x = model, ymin = lower, ymax = upper), width = 0.5) +
    geom_hline(yintercept = count1$Freq[2], color = 'black', linetype = "dashed") +
    geom_hline(yintercept = count1$Freq[1], color = colo[1], linetype = "dashed") +
    geom_hline(yintercept = count1$Freq[3], color = colo[3], linetype = "dashed") +
    geom_hline(yintercept = count1$Freq[4], color = 'grey50', linetype = "dashed") +
    geom_hline(yintercept = count1$Freq[5], color = 'cyan3', linetype = "dashed") +
    theme_bw(12) + labs(y = 'Anteil', x = NULL) + 
    theme(legend.position = 'bottom')
}

