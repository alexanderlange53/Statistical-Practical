PredBarPlot <- function(sample, prediction, Variable = 'Meinung', x = c('Zustimmung', 'Ablehnung')){
  require(colorspace)
  colo <- diverge_hsv(5)
  count1 <- as.data.frame(table(sample$Meinung.zu.Stuttgart.21))
  count2 <- as.data.frame(table(prediction$Meinung))
  count1$Freq <- count1$Freq/sum(count1$Freq)
  count2$Freq <- count2$Freq/sum(count2$Freq)
  
  if(nrow(count1) == nrow(count2)){
    Bars <- as.data.frame(cbind(count1$Var1, count1$Freq, count2$Freq))
    Bars$V1 <- x
  }else{
    colnames(count2) <- c('Var1', 'Freq2')
    Bars <- merge(count1, count2, all.x = T)
    Bars[is.na(Bars)] <- 0
    Bars$Var1 <- x
  }
  
  colnames(Bars) <- c('V1', 'sample', 'prediction')
  Bars <- melt(Bars, id = 'V1')
  
    ggplot(Bars, aes(x = V1, y = value, fill = variable)) + xlab(paste(Variable)) +
    geom_bar(stat="identity", position=position_dodge(), col = 'black', alpha = .5) +
    scale_fill_manual(values=c(colo[1], colo[5])) + ylab('Relative Häufigkeit') +
    theme_bw(15) +
    theme(legend.position = 'bottom', legend.title = element_blank())
} 