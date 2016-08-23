prediction <- function(step)
  {
  pred.pop <- predict(step$model.spat, newdata=population, type="response")
  pred.sum <- by(pred.pop, population[,aggregation], sum)

  pred.sum <- data.frame(vorhersage=as.numeric(pred.sum), V2=names(pred.sum))
  names(pred.sum)[2] <- aggregation
  pred.sum$vorhersage <- round(pred.sum$vorhersage, 2)

  if(intervalle)
    {
    set.seed(seed)

    pred.boot <- data.frame(V1=pred.sum[,aggregation])
    names(pred.boot) <- aggregation

    n <- nrow(sample)

    cat("\nBerechnung der Bootstrap Konfidenzintervalle\n")
    cat("Achtung: Computerintensive Berechnungen\n\n")
    if(parallel)
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
    
  pred.sum <- pred.sum[order(pred.sum[,aggregation]),]
  write.table(pred.sum, file=paste(response,"_vorhersage.txt", sep=""), row.names=FALSE,
              col.names=TRUE, sep=" ", quote=FALSE)

  return(pred.sum)
  }


