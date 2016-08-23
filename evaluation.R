evaluate <- function(model, data, Bewertung = F) {
  pred.temp <- predict(model, newdata = data, type = "response") # model y is maybe faster
  pred <- data.frame(y = unlist(apply(pred.temp, 1, which.max)))
  
  if(nrow(data) != nrow(pred)){
    data <- data[1:nrow(pred),]
  }
  tab.temp <- table(data[,response], pred$y)
  if(Bewertung == T){
    if(nrow(tab.temp) == 5){
    if(ncol(tab.temp) == 3){
    tab <- cbind(tab.temp[, 1], tab.temp[, 2], tab.temp[, 3],
                 c(rep(0, times = nrow(tab.temp))),
                 c(rep(0, times = nrow(tab.temp))))
    }else{
      tab <- cbind(tab.temp[, 1], tab.temp[, 2], c(rep(0, times = nrow(tab.temp))),
                   c(rep(0, times = nrow(tab.temp))),
                   c(rep(0, times = nrow(tab.temp))))
    }
    }else{
        if(ncol(tab.temp) == 3){
          tab <- cbind(tab.temp[, 1], tab.temp[, 2], tab.temp[, 3])
        }else{
          tab <- cbind(tab.temp[, 1], tab.temp[, 2], c(rep(0, times = nrow(tab.temp))))
        }
      }
  }else{
    tab <- cbind(tab.temp[, 1], c(rep(0, times = nrow(tab.temp))), tab.temp[, 2])
  }
  classification <- round(sum(diag(tab))/sum(tab),4)
  
  #  plot(rocr)
  return(list(classification=classification, tab = tab))
}

evaluateAll <- function(step.model, data, Bewertung = F){
  cat("\nVorhersagequalitaet des Modells:\n\n")
  eval.spat <- evaluate(step.model$model.spat, data, Bewertung)
  eval.nospat <- evaluate(step.model$model.nospat, data, Bewertung)
  eval.spatonly <- evaluate(step.model$model.spatonly, data, Bewertung)
  cat("Geoadditives Modell:\n")
  cat("  Reklassifikation: ", eval.spat$classification, "\n", sep="")
  cat("Modell ohne raeumlichem Effekt:\n")
  cat("  Reklassifikation: ", eval.nospat$classification, "\n", sep="")
  cat("Modell nur mit raeumlichem Effekt:\n")
  cat("  Reklassifikation: ", eval.spatonly$classification, "\n", sep="")
  return(invisible())
}


evaluate.bivariate <- function(model, data) {
  # pred.temp <- data.frame(mu=as.matrix(predict(model, type="response")))
  pred.temp <- predict(model, newdata = data, type = "response")
  pred <- ifelse(pred.temp < 0.5, 0, 1)
  tab <- table(data[,response], pred)
  classification <- round(sum(diag(tab))/sum(tab),4)
  
  #  plot(rocr)
  return(list(classification=classification, tab = tab))
}


CrossEvaluation <- function (model, sample, repeatitions  = 10) {
  leave_out <- sample.int(n = dim(sample)[1], size = repeatitions)
  ret <- data.frame(Observation.No = integer(), Observed.y = integer(), Predicted.y = integer())
  
  for (i in c(1 : repeatitions)) {
    all <- c(1 : dim(sample)[1])
    subset_i <- all[-leave_out]
    print(paste('Model', i, 'of', repeatitions))
    gam_i <- gam(model$formula, family = model$family, method="REML", data = sample, weights = unlist(sample[, "Gewicht"]), subset = unlist(subset_i)) # Fit a GAM
    ret_i <- cbind(leave_out[i], sample$Meinung.zu.Stuttgart.21[leave_out[i]], apply(predict(model, newdata = sample[leave_out[i],], type = "response"), 1, which.max)) # Compare true and estiamted y.
    ret <- rbind(ret, ret_i)
  }
  names(ret) = c("Observation.No", "Observed.y", "Predicted.y")
  return(ret)
}

