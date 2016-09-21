evaluate <- function(model, data) {
  pred.temp <- predict(model, newdata = data, type = "response")
  pred <- data.frame(y = unlist(apply(pred.temp, 1, which.max)))
  
  if(nrow(data) != nrow(pred)){
    data <- data[1:nrow(pred),]
  }
  data[,response] <- as.factor(data[,response])
  pred$y <- factor(pred$y, levels = levels(data[,response]))
  tab <- as.matrix(table(data[,response], pred$y))
  classification <- round(sum(diag(tab))/sum(tab),4)
  
  #  plot(rocr)
  return(list(classification=classification, tab = tab))
}

evaluateAll <- function(step.model, data){
  cat("\nVorhersagequalitaet des Modells:\n\n")
  eval.spat <- evaluate(step.model$model.spat, data)
  eval.nospat <- evaluate(step.model$model.nospat, data)
  eval.spatonly <- evaluate(step.model$model.spatonly, data)
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

evaluateAll.bivariate <- function(step.model, data){
  cat("\nVorhersagequalitaet des Modells:\n\n")
  eval.spat <- evaluate.bivariate(step.model$model.spat, data)
  eval.nospat <- evaluate.bivariate(step.model$model.nospat, data)
  eval.spatonly <- evaluate.bivariate(step.model$model.spatonly, data)
  cat("Geoadditives Modell:\n")
  cat("  Reklassifikation: ", eval.spat$classification, "\n", sep="")
  cat("Modell ohne raeumlichem Effekt:\n")
  cat("  Reklassifikation: ", eval.nospat$classification, "\n", sep="")
  cat("Modell nur mit raeumlichem Effekt:\n")
  cat("  Reklassifikation: ", eval.spatonly$classification, "\n", sep="")
  return(invisible())
}

crossval <- function(x, sample){
  x <- x[order(x$Observation.No),]
  if(!nrow(x) == nrow(sample)){
    sample <- sample[1:nrow(x),]
  }
  x <- cbind(x[,c(2,3)], sample[,-c(1,2)])
  x$Observed.y <- as.factor(x$Observed.y)
  x$Predicted.y <- factor(x$Predicted.y, levels = levels(x$Observed.y))
  
  tab <- as.matrix(table(x$Observed.y, x$Predicted.y))
  tab2 <- round(tab/rowSums(tab), 3)
  classification <- round(sum(diag(tab))/sum(tab),4)
  
  return(list(classification=classification, tab = tab2))
}

