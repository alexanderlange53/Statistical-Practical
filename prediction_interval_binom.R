
# Skript zur Berechnung der Bootstrap-Konfidenzintervalle für die aggregierten Schätzungen

set.seed(seed)



# pred.boot <- data.frame(V1=pred.sum[,aggregation])
# names(pred.boot) <- aggregation

n <- nrow(sample)

cat("\nBerechnung der Bootstrap Konfidenzintervalle\n")
cat("Achtung: Computerintensive Berechnungen\n\n")

library("foreach")
library("doMC")
registerDoMC(cores = ncores)

indmat <- matrix(0, nrow = n, ncol = nboot)
wmat <- matrix(0, nrow = n, ncol = nboot)
for(b in 1:nboot) {
  indmat[,b] <- sample(1:n, size=n, replace=TRUE)
  wmat[,b] <- sample[indmat[,b],gewichte]
}
bootfun <- function(b) {
  cat("Bootstrap sample ",b," (von ",nboot,")\n",sep="")
  # if(all(unique(sample[, aggregation] %in% unique(sample[indmat[,b], aggregation])))) {
  helpmodel <- gam(model$formula, weights=wmat[,b], family=verteilung, method="REML", data=sample[indmat[,b],])
  helppred <- Prediction(population, helpmodel, IFUmfrage = IFUmfrage, binom = TRUE)
  predData <- data.frame(V1 = helppred[, c(1 : max(sample[,response]))], V2 = as.character(population[, aggregation]))
  names(predData)[dim(predData)[2]] <- aggregation
  helpsum <- Prediction.Aggregation(pred = predData, agg = aggregation)
  # } else {
  # helpsum <- data.frame(V1 = as.character(unique(sample[,aggregation])), Vorhersage_Kategorie_1 = NA, Vorhersage_Kategorie_2 = NA, Vorhersage_Kategorie_3 = NA)
  # names(helpsum)[1] <- aggregation
  # helpsum <- helpsum[order(helpsum[,1]),]
  # }
  
  return((helpsum))
  
}
boot <- foreach(r=1:nboot, .combine = "cbind") %dopar%
  bootfun(r)

if(max(sample[,response]) == 1) {
  # lower
  pred.boot.lower <- data.frame(
    V1 = pred.sum[,aggregation],
    V2 = apply(boot[, seq(2,nboot * 2, 2)], 1, quantile, probs = (1 - coverage) / 2),
    stringsAsFactors = FALSE
  )
  # upper
  pred.boot.upper <- data.frame(
    V1 = pred.sum[,aggregation],
    V2 = apply(boot[, seq(2,nboot * 4 -2, 4)], 1, quantile, probs = 1 - (1 - coverage) / 2),
    V3 = apply(boot[, seq(3,nboot * 4 -1, 4)], 1, quantile, probs = 1 - (1 - coverage) / 2),
    V4 = apply(boot[, seq(4,nboot * 4 -0, 4)], 1, quantile, probs = 1 - (1 - coverage) / 2),
    stringsAsFactors = FALSE
  )
  # mean
  pred.boot.mean <- data.frame(
    V1 = pred.sum[,aggregation],
    V2 = apply(boot[, seq(2,nboot * 4 -2, 4)], 1, mean),
    V3 = apply(boot[, seq(3,nboot * 4 -1, 4)], 1, mean),
    V4 = apply(boot[, seq(4,nboot * 4 -0, 4)], 1, mean),
    stringsAsFactors = FALSE
  )
  # median
  pred.boot.median <- data.frame(
    V1 = pred.sum[,aggregation],
    V2 = apply(boot[, seq(2,nboot * 4 -2, 4)], 1, median),
    V3 = apply(boot[, seq(3,nboot * 4 -1, 4)], 1, median),
    V4 = apply(boot[, seq(4,nboot * 4 -0, 4)], 1, median),
    stringsAsFactors = FALSE
  )
  
  names(pred.boot.upper)[1] <- aggregation
  names(pred.boot.lower)[1] <- aggregation
  names(pred.boot.mean)[1] <- aggregation
  names(pred.boot.median)[1] <- aggregation
  
  for(i in c(2 : 4)){
    names(pred.boot.upper)[i] <- paste("o_intervall", i - 1, sep = "_")
    names(pred.boot.lower)[i] <- paste("u_intervall", i - 1, sep = "_")
    names(pred.boot.mean)[i] <- paste("mean", i - 1, sep = "_")
    names(pred.boot.median)[i] <- paste("median", i - 1, sep = "_")
  }
  
  pred.interval <- list(o_intervall = pred.boot.upper, u_intervall = pred.boot.lower, mean = pred.boot.mean, median = pred.boot.median)
} else {
  cat('Fehler')
}
