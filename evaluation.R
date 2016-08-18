evaluate <- function(model, data) {
  # pred.temp <- data.frame(mu=as.matrix(predict(model, type="response")))
  pred.temp <- predict(model, newdata = data, type = "response") # model y is maybe faster
  
  pred <- data.frame(y = as.vector(apply(pred.temp, 1, which.max)))
  # pred$y <- 1*(pred$mu>=0.5)
  tab.temp <- table(data[,response], pred$y)
  tab <- cbind(tab.temp[, 1], c(0, 0, 0), tab.temp[, 2])
  classification <- round(sum(diag(tab))/sum(tab),4)
  
  #  plot(rocr)
  return(list(classification=classification, tab = tab))
}

evaluate.bivariate <- function(model, data) {
  # pred.temp <- data.frame(mu=as.matrix(predict(model, type="response")))
  pred.temp <- predict(model, newdata = data, type = "response") # model y is maybe faster
  pred <- data.frame(y = as.vector(apply(pred.temp, 1, which.max)))
  # pred$y <- 1*(pred$mu>=0.5)
  tab.temp <- table(data[,response], pred$y)
  tab <- cbind(tab.temp[, 1], c(0, 0, 0), tab.temp[, 2])
  classification <- round(sum(diag(tab))/sum(tab),4)
  
  #  plot(rocr)
  return(list(classification=classification, tab = tab))
}


cross.evaluation <- function (model, data, n  = 10) {
  leave_out <- sample.int(n = dim(data)[1], size = n)
  ret <- data.frame(Observation.No = integer(), Observed.y = integer(), Predicted.y = integer())
  #tab_ret <- list(c(1 : n)) # Create List with n empty elements
  for (i in c(1 : n)) {
    print(paste('Model', i, 'of', n))
    # subsetdata <- data[-leave_out[i],] # leave one out
    gam_i <- gam(model$formula, weights = data[-leave_out[i],"gewichte"], family=model$family, method="REML", data=data[-leave_out[i],]) # Fit a GAM
    ret_i <- cbind(leave_out[i], data$Meinung.zu.Stuttgart.21[leave_out[i]], apply(predict(model, newdata = data[leave_out[i],], type = "response"), 1, which.max)) # Compare true and estiamted y.
    
    ret <- rbind(ret, as.data.frame(ret_i))
    #class_ret[i] <- evaluate(gam_i, subsetdata)$classification
  }
  names(ret) = c("Observation.No", "Observed.y", "Predicted.y")
  return(ret)
}