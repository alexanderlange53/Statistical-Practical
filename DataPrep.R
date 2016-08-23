DataPrep <- function(sample, binom = T, Stuttgart21 = T){
 if(binom == T){
   for(i in 1:nrow(sample)){
     if(sample$Meinung.zu.Stuttgart.21[i] == 6) {
       sample$Meinung.zu.Stuttgart.21[i] <- NA
     }
   }
   
   sample <- na.omit(sample)
   for(i in 1:nrow(sample)){
     if(sample$Meinung.zu.Stuttgart.21[i] == 1){
       sample$Meinung.zu.Stuttgart.21[i] <- 0
     }
   }
   for(i in 1:nrow(sample)){
     if(sample$Meinung.zu.Stuttgart.21[i] == 2){
       sample$Meinung.zu.Stuttgart.21[i] <- 0
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
       sample$Meinung.zu.Stuttgart.21[i] <- 1
     }
   }
   for(i in 1:nrow(sample)){
     if(sample$Meinung.zu.Stuttgart.21[i] == 5){
       sample$Meinung.zu.Stuttgart.21[i] <- 1
     }
   }
# For the multinomial case
 }else{
# For Stuttgart 
   if(Stuttgart21 == T){
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
# For 'Bewertung Wohngegend'
   }else{
     for(i in 1:nrow(sample)){
       if(sample$Bewertung.Wohngegend[i] == 6) {
         sample$Bewertung.Wohngegend[i] <- NA
       }
     }
     
     sample <- na.omit(sample)
     
     for(i in 1:nrow(sample)){
       if(sample$Bewertung.Wohngegend[i] == 4){
         sample$Bewertung.Wohngegend[i] <- 3
       }
     }
     for(i in 1:nrow(sample)){
       if(sample$Bewertung.Wohngegend[i] == 5){
         sample$Bewertung.Wohngegend[i] <- 3
       }
     }
   }
 }
  return(sample)
}