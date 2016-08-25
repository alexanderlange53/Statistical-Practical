Prediction <- function(Population, model, Umfrage = T, binom = T){
  
  if(Umfrage == T){
    # hier koennte eine dynamische Abfrage rein
    u.p <- select(Population, Altersklasse,Haushaltsgroesse, Geschlecht,Familienstand, Nationalitaet,GaussX, GaussY, Stadtteil, Stadtbezirk)
    names(u.p) <- c("Altersklasse.Befragter","Personenzahl.im.Haushalt", "Geschlecht","Familienstand","Nationalität","X" ,"Y", "Stadtteil", "Stadtbezirk")
    
    u.p2 <- u.p
    u.p2$Altersklasse.Befragter <- ""
    u.p2$Altersklasse.Befragter[u.p$Altersklasse.Befragter == 1] <- 1
    u.p2$Altersklasse.Befragter[u.p$Altersklasse.Befragter == 2 | u.p$Altersklasse.Befragter == 3] <- 2
    u.p2$Altersklasse.Befragter[u.p$Altersklasse.Befragter == 4 | u.p$Altersklasse.Befragter == 5] <- 3
    u.p2$Altersklasse.Befragter[u.p$Altersklasse.Befragter == 6 | u.p$Altersklasse.Befragter == 7] <- 4
    u.p2$Altersklasse.Befragter[u.p$Altersklasse.Befragter == 8 | u.p$Altersklasse.Befragter == 9] <- 5
    u.p2$Altersklasse.Befragter[u.p$Altersklasse.Befragter == 10 | u.p$Altersklasse.Befragter == 11 
                                | u.p$Altersklasse.Befragter == 12 | u.p$Altersklasse.Befragter == 13
                                | u.p$Altersklasse.Befragter == 14] <- 6
    u.p2$Nationalität <- ''
    u.p2$Nationalität[u.p$Nationalität == 'Deutsch'] <- 'Deutsch'
    u.p2$Nationalität[u.p$Nationalität == 'Nicht-Deutsch'] <- 'Nicht Deutsch'
    
    u.p2$Geschlecht <- ''
    u.p2$Geschlecht[u.p$Geschlecht == 'Mann'] <- 'Männlich'
    u.p2$Geschlecht[u.p$Geschlecht == 'Frau'] <- 'Weiblich'
    
    u.p2$Altersklasse.Befragter <- as.integer(u.p$Altersklasse.Befragter)
    u.p2$Geschlecht <- as.factor(u.p2$Geschlecht)
    u.p2$Nationalität <- as.factor(u.p2$Nationalität)
    
    pred.pop.u.3 <- predict.gam(model, newdata = u.p2, type = 'response')
    
    pred.pop <- cbind(pred.pop.u.3, u.p2$X, u.p2$Y, u.p2$Stadtteil)
    
  }else{ # Hier morgen weitermahcen
    z.p <- select(Population, alter,haushaltsgroesse, geschlecht, familienstand, staatsangehoerigkeit, xcoord, ycoord, stadtteil )
    names(z.p) <- c("Altersklasse.Befragter","Personenzahl.im.Haushalt","Geschlecht","Familienstand","Nationalität","X" ,"Y", 'Stadtteil')
    
    z.p2 <- z.p
    z.p2$Altersklasse.Befragter <- ""
    z.p2$Altersklasse.Befragter[z.p$Altersklasse.Befragter == 1] <- 1
    z.p2$Altersklasse.Befragter[z.p$Altersklasse.Befragter == 2 | z.p$Altersklasse.Befragter == 3] <- 2
    z.p2$Altersklasse.Befragter[z.p$Altersklasse.Befragter == 4 | z.p$Altersklasse.Befragter == 5] <- 3
    z.p2$Altersklasse.Befragter[z.p$Altersklasse.Befragter == 6 | z.p$Altersklasse.Befragter == 7] <- 4
    z.p2$Altersklasse.Befragter[z.p$Altersklasse.Befragter == 8 | z.p$Altersklasse.Befragter == 9] <- 5
    
    z.p2$Nationalität <- ''
    z.p2$Nationalität[z.p$Nationalität == 'Deutschland'] <- 'Deutsch'
    z.p2$Nationalität[z.p$Nationalität == 'Ausland'] <- 'Nicht Deutsch'
    
    z.p2$Geschlecht <- ''
    z.p2$Geschlecht[z.p$Geschlecht == 'maennlich'] <- 'Männlich'
    z.p2$Geschlecht[z.p$Geschlecht == 'weiblich'] <- 'Weiblich'
    
    z.p2$Altersklasse.Befragter <- as.integer(z.p$Altersklasse.Befragter)
    z.p2$Geschlecht <- as.factor(z.p2$Geschlecht)
    z.p2$Nationalität <- as.factor(z.p2$Nationalität)
    
    z.p2$Familienstand <- ''
    z.p2$Familienstand[z.p$Familienstand == 'geschieden'] <- 'geschieden'
    z.p2$Familienstand[z.p$Familienstand == 'ledig'] <- 'ledig'
    z.p2$Familienstand[z.p$Familienstand == 'verheiratet/eingetragene Partnerschaft'] <- 'verheiratet'
    z.p2$Familienstand[z.p$Familienstand == 'verwitwet'] <- 'verwitwet'
    z.p2$Familienstand <- as.factor(z.p2$Familienstand)
    
    z.p2$Personenzahl.im.Haushalt <- ''
    z.p2$Personenzahl.im.Haushalt[z.p$Personenzahl.im.Haushalt == '1'] <- 1
    z.p2$Personenzahl.im.Haushalt[z.p$Personenzahl.im.Haushalt == '2'] <- 2
    z.p2$Personenzahl.im.Haushalt[z.p$Personenzahl.im.Haushalt == '3'] <- 3
    z.p2$Personenzahl.im.Haushalt[z.p$Personenzahl.im.Haushalt == '4'] <- 4
    z.p2$Personenzahl.im.Haushalt[z.p$Personenzahl.im.Haushalt == '5'] <- 5
    z.p2$Personenzahl.im.Haushalt[z.p$Personenzahl.im.Haushalt == '6'] <- 5
    
    z.p2$Personenzahl.im.Haushalt <- as.numeric(z.p2$Personenzahl.im.Haushalt)
    
    pred.pop.z.3 <- predict.gam(model, newdata = z.p2, type = 'response')
    
    pred.pop <- cbind(pred.pop.z.3, z.p2$X, z.p2$Y, z.p2$Stadtteil)
  }
  
  if(binom == T){
    colnames(pred.pop) <- c('1', 'X', 'Y', 'Stadtteil')
    Meinung <- rep(0, nrow(pred.pop))
    for(i in 1:nrow(pred.pop)){
      if(pred.pop[i,1] > 0.5){
        Meinung[i] <- 1
      }else{
        Meinung[i] <- 0
      }
    }
  }else{
    colnames(pred.pop) <- c('1', '2', '3', 'X', 'Y', 'Stadtteil')
    
    Meinung <- rep(0, nrow(pred.pop))
    for(i in 1:nrow(pred.pop)){
      if(pred.pop[i,1] > pred.pop[i,2] & pred.pop[i,1] > pred.pop[i,3]){
        Meinung[i] <- 1
      }
      if(pred.pop[i,2] > pred.pop[i,1] & pred.pop[i,2] > pred.pop[i,3]){
        Meinung[i] <- 2
      } 
      if(pred.pop[i,3] > pred.pop[i,1] & pred.pop[i,3] > pred.pop[i,2]){
        Meinung[i] <- 3
      }
    }
  }
  pred.pop <- cbind(pred.pop, Meinung)
  
} 
