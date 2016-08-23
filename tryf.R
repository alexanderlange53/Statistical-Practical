tryf <- function(pred.pop){
  Meinung <- rep(0, nrow(pred.pop))
  for(i in 1:nrow(pred.pop)){
    if(pred.pop[i,1] > 0.65){
      Meinung[i] <- 3
    }else if(pred.pop[i,1] < 0.45){
      Meinung[i] <- 1
    }else{
      Meinung[i] <- 2
    }
  }
  pred.pop[,5] <- Meinung
  return(pred.pop)
}