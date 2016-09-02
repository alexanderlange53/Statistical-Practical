#------------------#
#### Prediction ####
#------------------#

# Funktion zur Vorhersage der Wahrscheinlichkeiten einer Ausprägung auf Individuenebene

Prediction <- function(Population, model, IFUmfrage = TRUE, binom = TRUE, SpatTyp = 'kontinuierlich'){
  
  if(IFUmfrage == T){
    # hier koennte eine dynamische Abfrage oder zumindest ein Übergabe der erklärenden Variablen hilfreich sein.
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
    
    pred.pop.u.3 <- data.frame(pred.pop.u.3 = predict.gam(model, newdata = u.p2, type = 'response'))
    pred.pop <- cbind(pred.pop.u.3, u.p2$X, u.p2$Y, as.character(u.p2$Stadtteil), as.character(u.p2$Stadtbezirk))
    
    
  }else{
    z.p <- select(Population, alter,haushaltsgroesse, geschlecht, familienstand, staatsangehoerigkeit, xcoord, ycoord, Stadtteil, Stadtbezirk)
    names(z.p) <- c("Altersklasse.Befragter","Personenzahl.im.Haushalt","Geschlecht","Familienstand","Nationalität","X" ,"Y", "Stadtteil", "Stadtbezirk")
    
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
    
    pred.pop.z.3 <- as.data.frame(predict.gam(model, newdata = z.p2, type = 'response'))
    # Muss ein dataframe sein, da bein cbind sonst eine Matrix entsteht und die Faktoren gelöscht werden
    pred.pop <- cbind(pred.pop.z.3, z.p2$X, z.p2$Y, as.character(z.p2$Stadtteil), as.character(z.p2$Stadtbezirk))
  }
  
  if(binom == T){
    pred.pop[, 1] <- as.numeric(pred.pop[, 1])
    colnames(pred.pop) <- c('Kategorie_1', 'X', 'Y', 'Stadtteil', 'Stadtbezirk')
    # Meinung <- rep(0, nrow(pred.pop))
    # for(i in 1:nrow(pred.pop)){
    #   if(pred.pop[i,1] > 0.5){
    #     Meinung[i] <- 1
    #   }else{
    #     Meinung[i] <- 0
    #   }
    # }
    Meinung <- data.frame(Meinung = ifelse(pred.pop[, 1] < 0.5, 0, 1))
  } else{
    colnames(pred.pop) <- c('Kategorie_1', 'Kategorie_2', 'Kategorie_3', 'X', 'Y', 'Stadtteil', 'Stadtbezirk')
    
    Meinung <- data.frame(Meinung = unlist(apply(pred.pop[, c(1 : 3)], 1, which.max)))
    #Meinung <- rep(0, nrow(pred.pop))
    # for(i in 1:nrow(pred.pop)){
    #   if(pred.pop[i,1] > pred.pop[i,2] & pred.pop[i,1] > pred.pop[i,3]){
    #     Meinung[i] <- 1
    #   }
    #   if(pred.pop[i,2] > pred.pop[i,1] & pred.pop[i,2] > pred.pop[i,3]){
    #     Meinung[i] <- 2
    #   }
    #   if(pred.pop[i,3] > pred.pop[i,1] & pred.pop[i,3] > pred.pop[i,2]){
    #     Meinung[i] <- 3
    #   }
    # }
  }
  pred.pop_ret <- cbind(pred.pop, Meinung)
  return(as.data.frame(pred.pop_ret))
}


# Funktion zu Aggregation der Vorhersage auf regionaler Ebene = kleinräumige Extrapolation
Prediction.Aggregation <- function (pred, agg, model) {
  # pred: dataframe returned by Prediction()
  # agg: Aggregation level (Stadtteil/Stadtbezirk)
  pred.sum <- aggregate(x = pred[, c(1 : dim(pred)[2] - 1)], by = list(as.character(pred[, agg])), FUN = sum)
  pred.sum <- pred.sum[order(pred.sum[,1]),]
  names(pred.sum) <- c(names(pred)[dim(pred)[2]], paste('Vorhersage', names(pred)[1 : dim(pred)[2] -1], sep = '_'))
  return(pred.sum)
}

Prediction.Intervall <- function (pred, agg, model, seed) {
  # pred: dataframe returned by Prediction()
  # agg: Aggregation level (Stadtteil/Stadtbezirk)
  set.seed(seed)
  
  pred.boot <- data.frame(V1=pred.sum[,aggregation])
  names(pred.boot) <- aggregation
  
  n <- nrow(sample)
  
  cat("\nBerechnung der Bootstrap Konfidenzintervalle\n")
  cat("Achtung: Computerintensive Berechnungen\n\n")
  # if(parallel) Lieber immer parallel: Nur Linux
  {
    library("foreach")
    library("doMC")
    registerDoMC(cores=10)
    
    #      indmat <- matrix(0, nrow=n, ncol=nboot)
    #      wmat <- matrix(0, nrow=n, ncol=nboot)
    #      for(b in 1:nboot)
    #        {
    #        indmat[,b] <- sample(1:n, size=n, replace=TRUE)
    #        wmat[,b] <- sample[indmat[,b],gewichte]
    #        }
    
    bootfun <- function(b)
    {
      frac <- nboot%/%10
      if(frac<1)
        frac<-1
      if((b%%frac)==0)
        cat("Bootstrap sample ",b," (von ",nboot,")\n",sep="")
      helpmodel <- gam(step$fm, weights=wmat[,b], family=verteilung, method="REML", data=sample[indmat[,b],])
      helppred <- predict(helpmodel, newdata=population, type="response")
      helpsum <- by(helppred, population[,aggregation], sum)
      return(as.numeric(helpsum))
    }
    
    boot <- foreach(r=1:nboot, .combine="rbind") %dopar%
      bootfun(r)
    
    for(b in 1:nboot)
    {
      pred.boot$b <- round(boot[b,],2)
      names(pred.boot)[b+1] <- paste("b",b,sep="")
    }
  }
  else
  {
    for(b in 1:nboot)
    {
      frac <- nboot%/%10
      if(frac<1)
        frac<-1
      if((b%%frac)==0)
        cat("Bootstrap sample ",b," (von",nboot,")\n",sep="")
      ind <- sample(1:n, size=n, replace=TRUE)
      helpdata <- sample[ind,]
      helpmodel <- gam(step$fm, weights=helpdata[,gewichte], family=verteilung, method="REML", data=helpdata)
      helppred <- predict(helpmodel, newdata=population, type="response")
      helpsum <- by(helppred, population[,aggregation], sum)
      pred.boot$b <- round(as.numeric(helpsum),2)
      names(pred.boot)[b+1] <- paste("b",b,sep="")
    }
  }
  
  pred.sum$lower <- apply(pred.boot[,-1], 1, quantile, probs=(1-coverage)/2)
  pred.sum$upper <- apply(pred.boot[,-1], 1, quantile, probs=1-(1-coverage)/2)
}