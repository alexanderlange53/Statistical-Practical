DataPrep <- function(sample, binom = T){
 if(binom == T){
   for(i in 1:nrow(sample)){
     if(sample$Meinung.zu.Stuttgart.21[i] == 6) {
       sample$Meinung.zu.Stuttgart.21[i] <- NA
     }
   }
   
   sample <- na.omit(sample)
   
   for(i in 1:nrow(sample)){
     if(sample$Meinung.zu.Stuttgart.21[i] == 2){
       sample$Meinung.zu.Stuttgart.21[i] <- 1
     }
   }
   for(i in 1:nrow(sample)){
     if(sample$Meinung.zu.Stuttgart.21[i] == 3){
       sample$Meinung.zu.Stuttgart.21[i] <- NA
     }
   }
   sample <- na.omit(sample)
   for(i in 1:nrow(sample)){
     if(sample$Meinung.zu.Stuttgart.21[i] == 4){
       sample$Meinung.zu.Stuttgart.21[i] <- 0
     }
   }
   for(i in 1:nrow(sample)){
     if(sample$Meinung.zu.Stuttgart.21[i] == 5){
       sample$Meinung.zu.Stuttgart.21[i] <- 0
     }
   }
 }else{
   for(i in 1:nrow(sample)){
     if(sample$Meinung.zu.Stuttgart.21[i] == 6) {
       sample$Meinung.zu.Stuttgart.21[i] <- NA
     }
   }
   
   sample <- na.omit(sample)
   
   for(i in 1:nrow(sample)){
     if(sample$Meinung.zu.Stuttgart.21[i] == 2){
       sample$Meinung.zu.Stuttgart.21[i] <- 1
     }
   }
   for(i in 1:nrow(sample)){
     if(sample$Meinung.zu.Stuttgart.21[i] == 3){
       sample$Meinung.zu.Stuttgart.21[i] <- 2
     }
   }
   for(i in 1:nrow(sample)){
     if(sample$Meinung.zu.Stuttgart.21[i] == 4){
       sample$Meinung.zu.Stuttgart.21[i] <- 3
     }
   }
   for(i in 1:nrow(sample)){
     if(sample$Meinung.zu.Stuttgart.21[i] == 5){
       sample$Meinung.zu.Stuttgart.21[i] <- 3
     }
   }
 }
  return(sample)
}