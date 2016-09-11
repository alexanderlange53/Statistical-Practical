MarkovRandomField <- function(SpatOb, Bezirke = T){
  SpatOb@data$id <- rownames(SpatOb@data)
  helpdf <- fortify(SpatOb, region = "id")
  bb <- merge(helpdf, SpatOb@data, by = 'id', all.x = T)
  bb3 <- list()
  if(Bezirke == T){
  bb2 <- dplyr::select(bb, long, lat, STADTBEZIR)
  for(i in levels(factor(bb2$STADTBEZIR))) {
    bb3[[i]] <- bb2[bb2$STADTBEZIR == i, c('long', 'lat')]
  }}else{
    bb2 <- select(bb, long, lat, STADTTEIL)
    for(i in levels(factor(bb2$STADTTEIL))) {
      bb3[[i]] <- bb2[bb2$STADTTEIL == i, c('long', 'lat')]
      
    }
  }
  zt <- list(polys = bb3)
  return(zt)
}