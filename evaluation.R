evaluation.in <- function(model)
  {
  #pred <- data.frame(mu=as.vector(predict(model, type="response")))
  pred <- as.data.frame(predict(model, type="response"))
  pred$y <- apply(pred, 1, which.max)
  #pred$y <- 1*(pred$mu>=0.5)
  tab <- table(sample[,response], pred$y)
  classification <- round(sum(diag(tab))/sum(tab),4)

  pred1 <- ROCR::prediction(pred$mu, sample[,response])
  rocr <- performance(pred1, "tpr", "fpr")
  auc <- round(performance(pred1, measure="auc")@y.values[[1]],4)

#  plot(rocr)
  return(list(classification=classification, auc=auc))
  }

evaluate <- function()
  {
  cat("\nVorhersagequalitaet des Modells:\n\n")
  eval.spat <- evaluation.in(step.model$model.spat)
  eval.nospat <- evaluation.in(step.model$model.nospat)
  eval.spatonly <- evaluation.in(step.model$model.spatonly)
  cat("Geoadditives Modell:\n")
  cat("  Reklassifikation: ", eval.spat$classification, "\n", sep="")
  cat("  AUC:              ", eval.spat$auc, "\n\n", sep="")
  cat("Modell ohne raeumlichem Effekt:\n")
  cat("  Reklassifikation: ", eval.nospat$classification, "\n", sep="")
  cat("  AUC:              ", eval.nospat$auc, "\n\n", sep="")
  cat("Modell nur mit raeumlichem Effekt:\n")
  cat("  Reklassifikation: ", eval.spatonly$classification, "\n", sep="")
  cat("  AUC:              ", eval.spatonly$auc, "\n\n", sep="")

  if(validate)
    {
    pred$wahrheit <- validation[,response.val]
    plot(pred$wahrheit, pred$vorhersage)
    mse <- sum((pred$vorhersage-pred$wahrheit)^2)
    coverage <- sum((pred$lower<=pred$wahrheit)*
                    (pred$upper>=pred$wahrheit))/nrow(pred)
    cat("\nMittlerer quadratischer Prognosefehler: ", mse, "\n", sep="")
    cat("Ueberdeckungsw'keit der Vorhersagen:    ", coverage, "\n\n", sep="")

    write.table(pred, file=paste(response,"_vorhersage.txt", sep=""), row.names=FALSE,
                col.names=TRUE, sep=" ", quote=FALSE)
    }

  return(invisible())
  }

