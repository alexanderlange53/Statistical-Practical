stepAIC <- function()
  {
  make.formula <- function(response, fixed, pars, nonpars)
    {
    fm <- paste(response, " ~ ", sep="")
    psstring <- ""
    if(length(fixed)>0)
      {
      fm <- paste(fm, paste(fixed, collapse= "+"), sep="")
      psstring <- " + "
      }
    if(length(pars)>0)
      {
      fm <- paste(fm, psstring, paste(pars, collapse= "+"), sep="")
      psstring <- " + "
      }
    if(length(nonpars)>0)
      {
      fm <- paste(fm, psstring, paste("s(",nonpars, ", bs=\"ps\")", collapse= "+", sep=""), sep="")
      }
    return(as.formula(fm))
    }

  model.current <- make.formula(response, fixed, c(), c())
  AIC.current <- AIC(gam(model.current, weights=sample[,gewichte], family=verteilung, method="REML", data=sample))
  pars.current <- c()
  nonpars.current <- c()

  pars.candidate <- pars
  nonpars.candidate <- nonpars
  trace <- matrix(NA, nrow = 10, ncol = 2)
  if(modellwahl)
    {
    test <- TRUE
    cat("Schrittweise Modellwahl\n\n")
    }
  else
    {
    test <- FALSE
    pars.current <- pars
    nonpars.current <- nonpars
    }
  test.add <- TRUE
  test.drop <- TRUE
  it <- 1

  while(test)
    {
    cat("Iteration ",it,"\n\n", sep="")

    if(test.add)
      {
      # Adding variables
      cat("Ergaenzung von Variablen\n")
      cat("Aktuelles Modell: ",response,"~", as.character(model.current)[3], "\n\n", sep="")
      candidate.models.add <- vector(mode="list",
             length=length(pars.candidate) + 2*length(nonpars.candidate))
      AIClist.add <- rep(NA, length(candidate.models.add))
      if(length(pars.candidate)>0)
        {
        for(i in 1:length(pars.candidate))
          {
          cat("  Ergaenze ",pars.candidate[i],"\n",sep="")
          candidate.models.add[[i]] <- make.formula(response, fixed, c(pars.current, pars.candidate[i]),
                                                  nonpars.current)
          AIClist.add[i] <- AIC(gam(candidate.models.add[[i]], weights=sample[,gewichte], family=verteilung, method="REML", data=sample))
          }
        }
      if(length(nonpars.candidate)>0)
        {
        for(i in 1:length(nonpars.candidate))
          {
          cat("  Ergaenze ",nonpars.candidate[i],"\n",sep="")
          offset <- length(pars.candidate)
          candidate.models.add[[offset + i]] <- make.formula(response,
               fixed, c(pars.current, nonpars.candidate[i]), nonpars.current)
          AIClist.add[offset + i] <- AIC(gam(candidate.models.add[[offset + i]], weights=sample[,gewichte], family=verteilung, method="REML", data=sample))

          cat("  Ergaenze s(",nonpars.candidate[i],")\n",sep="")
          offset <- length(pars.candidate)+length(nonpars.candidate)
          candidate.models.add[[offset + i]] <- make.formula(response,
               fixed, pars.current, c(nonpars.current, nonpars.candidate[i]))
          AIClist.add[offset + i] <- AIC(gam(candidate.models.add[[offset + i]], weights=sample[,gewichte], family=verteilung, method="REML", data=sample))
          }
        }
      
      best.add <- which.min(AIClist.add)
      cat("\nMinimales AIC: ", AIClist.add[best.add], "\n\n", sep="")
      if(AIClist.add[best.add]<AIC.current)
        {
        model.current <- candidate.models.add[[best.add]]
        AIC.current <- AIClist.add[best.add]
        test.drop <- TRUE
        if(best.add <= length(pars.candidate))
          {
          pars.current <- c(pars.current, pars.candidate[best.add])
          pars.candidate <- pars.candidate[-best.add]
          }
        else if(best.add <= length(pars.candidate) + length(nonpars.candidate))
          {
          pars.current <- c(pars.current, nonpars.candidate[best.add-length(pars.candidate)])
          nonpars.candidate <- nonpars.candidate[-(best.add-length(pars.candidate))]
          }
        else
          {
          nonpars.current <- c(nonpars.current, nonpars.candidate[best.add-length(pars.candidate)-length(nonpars.candidate)])
          nonpars.candidate <- nonpars.candidate[-(best.add-length(pars.candidate)-length(nonpars.candidate))]
          }
        }
      else
        {
        test.add <- FALSE
        }
      if(length(pars.candidate) + length(nonpars.candidate) == 0)
        {
        test.add <- FALSE
        }
      }

    if(test.drop)
      {
      # Dropping variables
      cat("Loeschen von Variablen\n")
      cat("Aktuelles Modell: ",response,"~", as.character(model.current)[3], "\n\n", sep="")
      candidate.models.drop <- vector(mode="list",
              length=length(pars.current) + 2*length(nonpars.current))
      AIClist.drop <- rep(NA, length(candidate.models.drop))
      if(length(pars.current)>0)
        {
        for(i in 1:length(pars.current))
          {
          cat("  Loesche ",pars.current[i],"\n",sep="")
          candidate.models.drop[[i]] <- make.formula(response, fixed, c(pars.current[-i]),
                                                nonpars.current)
          AIClist.drop[i] <- AIC(gam(candidate.models.drop[[i]], weights=sample[,gewichte], family=verteilung, method="REML", data=sample))
          cat("  AIC ohne ",pars.current[i]," ", AIClist.drop[i], "\n",sep="")
         }
       }
      if(length(nonpars.current)>0)
        {
        for(i in 1:length(nonpars.current))
          {
          cat("  Loesche ",nonpars.current[i],"\n",sep="")
          offset <- length(pars.current)
          candidate.models.drop[[offset + i]] <- make.formula(response,
               fixed, pars.current, nonpars.current[-i])
          AIClist.drop[offset + i] <- AIC(gam(candidate.models.drop[[offset + i]], weights=sample[,gewichte], family=verteilung, method="REML", data=sample))

          cat("  Loesche s(",nonpars.current[i],")\n",sep="")
          offset <- length(pars.current)+length(nonpars.current)
          candidate.models.drop[[offset + i]] <- make.formula(response,
               fixed, c(pars.current,nonpars.current[i]), nonpars.current[-i])
          AIClist.drop[offset + i] <- AIC(gam(candidate.models.drop[[offset + i]], weights=sample[,gewichte], family=verteilung, method="REML", data=sample))
          cat("  AIC ohne ", nonpars.current[i]," ", AIClist.drop[offset + i], "\n",sep="")
          }
        }

      best.drop <- which.min(AIClist.drop)
      cat("\nMinimales AIC: ", AIClist.drop[best.drop], "\n\n", sep="")
      if(AIClist.drop[best.drop]<AIC.current)
        {
        model.current <- candidate.models.drop[[best.drop]]
        AIC.current <- AIClist.add[best.drop]
        test.add <- TRUE
        if(best <= length(pars.current))
          {
          if(pars.current[best] %in% pars)
            pars.candidate <- c(pars.candidate, pars.current[best])
          else
            pars.candidate <- c(nonpars.candidate, pars.current[best])
          pars.current <- pars.current[-best.drop]
          }
        else if(best <= length(pars.candidate) + length(nonpars.candidate))
          {
          nonpars.candidate <- c(nonpars.candidate,nonpars.current[best-length(pars.current)])
          nonpars.current <- nonpars.current[-(best-length(pars.candidate))]
          }
        else
          {
          pars.current <- c(pars.current, nonpars.current[best-length(pars.current)-length(nonpars.current)])
          nonpars.current <- nonpars.current[-(best-length(pars.current)-length(nonpars.current))]
          }
        }
      else
        {
        test.drop <- FALSE
        }
      if(length(pars.current) + length(nonpars.current) == 0)
        {
        test.drop <- FALSE
        }
      }
      
    if(test.add==FALSE & test.drop==FALSE)
      {
      test <- FALSE
      }
    it <- it + 1
    }
    
  model.nospat <- make.formula(response, c(), pars.current, nonpars.current)
  model.spatonly <- make.formula(response, fixed, c(), c())
  res <- list(model.spat=gam(model.current, weights=sample[,gewichte], family=verteilung, method="REML", data=sample),
              model.nospat=gam(model.nospat, weights=sample[,gewichte], family=verteilung, method="REML", data=sample),
              model.spatonly=gam(model.spatonly, weights=sample[,gewichte], family=verteilung, method="REML", data=sample),
              pars=pars.current, nonpars=nonpars.current,
              fm=model.current)
  return(res)
  }
  
