PseudoB2 <- function(sample, SpatOb, binom = T, response){
  
  SpatOb@data$id <- rownames(SpatOb@data)
  helpdf <- fortify(SpatOb, region = "id")
  bb <- merge(helpdf, SpatOb@data, by = 'id', all.x = T)
  bb2 <- select(bb, long, lat, STADTTEIL)
  
  # Problem ist, dass die GAM funktion immer mindestens eine Beobachtung für jede Kategorie in jedem Polygon erwaretet
  # Darum ist die Regression mit 5 Kategorien praktisch nicht möglich. Aber auch mit 3 Kategorien funktioniert es so nicht.
  # Lösung: Pseudobeobachtung erstellen und danach mit 0 gewichten.
  dataS <- sample
  m <- as.data.frame(table(dataS$Stadtteil, dataS[,response])) # Um zu sehen wo Beobachtungen fehlen
  fehlende.b <- filter(m, Freq == 0) # Stadtteile in denen Beobachtungen fehlen
  
  # Weiteres Problem: Einige Stadtteile wie z.B. der Wald im Westen haben gar keine Beobachtungen.
  # Darum weitere Pseudobeobachtungen
  stadtteile.pol <- as.data.frame(table(bb2$STADTTEIL)) # Alle Stadtteile mit räumlichen Informationen
  stadtteile.dat <- as.data.frame(table(dataS$Stadtteil)) # Alle Stadtteile mit Beobachtungen
  names(stadtteile.dat) <- c('Var1', 'Freq2')
  unterschied <- merge(stadtteile.pol, stadtteile.dat, all.x = T)
  unterschied <- filter(unterschied, is.na(Freq2)) # Leere Polygone wie z.B. der Wald
  if(binom == T){
    u <- c(rep(as.character(unterschied[,1]), 2)) # jeweils eine Beobachtung jeder Kategorie erstellen
    MM <- c(rep(1, nrow(unterschied)), rep(0, nrow(unterschied)))
  }else if(response == 'Bewertung.Wohngegend'){
    u <- c(rep(as.character(unterschied[,1]), 5))
    MM <- c(rep(1, nrow(unterschied)), rep(2, nrow(unterschied)), rep(3, nrow(unterschied)),
            rep(4, nrow(unterschied)), rep(5, nrow(unterschied)))
  }else{
    u <- c(rep(as.character(unterschied[,1]), 3))
    MM <- c(rep(1, nrow(unterschied)), rep(2, nrow(unterschied)), rep(3, nrow(unterschied)))
  }
  # erstellen der Pseudobeobachtungen für Stadtteile mit gar keinen Beobachtungen
  P <- c(rep(1,length(u)))
  M <- c(rep(1,length(u)))
  A <- c(rep(1,length(u)))
  N <- c(rep('Deutsch',length(u)))
  G <- c(rep('Männlich' ,length(u)))  
  FF <- c(rep('ledig',length(u)))
  Sb <- c(rep('Mitte', length(u)))
  X <- c(rep(3513518,length(u)))
  Y <- c(rep(404074, length(u)))
  D <- c(rep(0, length(u)))
  pseudo.a <- as.data.frame(cbind(MM, MM, P, M, A, G, FF, N, Sb, as.character(u), X, Y))
  # Gewichte einführen, um bias zu verhindern
  
  if(nrow(pseudo.a) >= 1){
  pseudo.a$Gewicht  <- 0
  names(pseudo.a) <- names(dataS)
  }
  # erstellen der Pseudobeobachtungen für Stadtteile mit parziel fehlenden Beobachtungen
  pseudo <- as.data.frame(cbind(as.numeric(as.character(fehlende.b$Var2)), as.character(fehlende.b$Var1)))
  P <- c(rep(1,nrow(pseudo)))
  M <- c(rep(1,nrow(pseudo)))
  A <- c(rep(1,nrow(pseudo)))
  N <- c( rep('Deutsch',nrow(pseudo)))
  G <- c( rep('Männlich' ,nrow(pseudo)))  
  FF <- c( rep('ledig',nrow(pseudo)))
  Sb <- c(rep('Mitte', nrow(pseudo)))
  X <- c( rep(3513518,nrow(pseudo)))
  Y <- c(rep(404074, nrow(pseudo)))
  D <- c(rep(0, nrow(pseudo)))
  pseudo.b <- as.data.frame(cbind(as.numeric(as.character(fehlende.b$Var2)),as.numeric(as.character(fehlende.b$Var2)),
                                  P, M, A, G, FF, N, Sb,as.character(fehlende.b$Var1), X, Y))
  
  # Pseudo Beobachtungen mit 0 gewichten
  if(nrow(pseudo.b) >= 1){
  pseudo.b$Gewicht  <- 0
  names(pseudo.b) <- names(dataS)
  }
  # Echte Beobachtungen mit 1 gewichten
  dat.teile <- dataS
  dat.teile$Gewicht <- 1
  
  # Zusammmenfügen von echten und pseudo Beobachtungen
  if(nrow(pseudo.a) >= 1){
    names(pseudo.a)<- names(dat.teile)
  }
  if(nrow(pseudo.b) >= 1){
    names(pseudo.b)<- names(dat.teile)
  }
  if(nrow(pseudo.a) >= 1 & nrow(pseudo.b) >= 1){
    dat.teile <- rbind(dat.teile, pseudo.b, pseudo.a)
  }else if(nrow(pseudo.a) >= 1 & nrow(pseudo.b) == 0){
    dat.teile <- rbind(dat.teile, pseudo.a)
  }else if(nrow(pseudo.a) == 0 & nrow(pseudo.b) >= 1){
    dat.teile <- rbind(dat.teile, pseudo.b)
  }
    dat.teile <- dat.teile[order(dat.teile$Stadtteil),]
  
  # Einige Variablen sind fälschlicherweise als Character gespeichert
  dat.teile$Bewertung.Wohngegend <- as.integer(dat.teile$Bewertung.Wohngegend)
  dat.teile$Personenzahl.im.Haushalt <- as.integer(dat.teile$Personenzahl.im.Haushalt)
  dat.teile$Monatliches.Netto.Haushaltseinkommen <- as.integer(dat.teile$Monatliches.Netto.Haushaltseinkommen)
  dat.teile$Altersklasse.Befragter <- as.integer(dat.teile$Altersklasse.Befragter)
  dat.teile$Meinung.zu.Stuttgart.21 <- as.numeric(dat.teile$Meinung.zu.Stuttgart.21)
  
  # Angleichen der Factor levels der Daten und des Markov random fields
  dat.teile$Stadtteil <- factor(dat.teile$Stadtteil, levels = names(zt$polys))
  return(dat.teile)
}