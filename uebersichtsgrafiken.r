rm(list=ls())

#library(geoR)
library(maptools)


## Geodaten einlesen ##
# Bezirke Polygone
b <- readShapeSpatial("/home/khusmann/mnt/U/Promotion/Kurse/Stat_Praktikum/Auswertung/GIS/Geodaten/bezirke/bezirke.shp")
# Buergerumfrage
bf_s <-readShapePoints("/home/khusmann/mnt/U/Promotion/Kurse/Stat_Praktikum/Auswertung/GIS/Geodaten/Buergerumfrage/buergerumfrage_stichprobe.shp")
bf_g <-readShapePoints("/home/khusmann/mnt/U/Promotion/Kurse/Stat_Praktikum/Auswertung/GIS/Geodaten/Buergerumfrage/buergerumfrage_grundgesamtheit.shp")

# Zensus
ze_s <-readShapePoints("/home/khusmann/mnt/U/Promotion/Kurse/Stat_Praktikum/Auswertung/GIS/Geodaten/Zensus/zensus_stichprobe.shp")
ze_g <-readShapePoints("/home/khusmann/mnt/U/Promotion/Kurse/Stat_Praktikum/Auswertung/GIS/Geodaten/Zensus/zensus_grundgesamtheit.shp")


## Abbildungen erstellen ##

# Einstellungen #
bgcol <- 'gray90'
setwd('/home/khusmann/mnt/U/Promotion/Kurse/Stat_Praktikum/Auswertung/GIS/Uebersichtsgrafiken/')

# Plots #

# Buergerumfrage, Natiuonalitaet
pdf('bf_s_nationalitaet.pdf')
par(mar=c(0,0,0,0))
plot(b, lwd=1, col= bgcol)

points(bf_s[bf_s$field_6 == 1, ], pch=16, cex=0.4, lwd=0.1, col = 'purple3')
points(bf_s[bf_s$field_6 == 2, ], pch=16, cex=0.4, lwd=0.1, col = 'springgreen')

legend("topleft", c('Deutsch', 'Nicht Deutsch'), pch=16, inset=0.05, bg="white", col = c('purple3', 'springgreen'), pt.cex=1.3)
dev.off()

pdf('bf_g_nationalitaet.pdf')
par(mar=c(0,0,0,0))
plot(b, lwd=1, col= bgcol)

points(bf_g[bf_g$field_6 == 1, ], pch=16, cex=0.1, lwd=0.1, col = 'purple3')
points(bf_g[bf_g$field_6 == 2, ], pch=16, cex=0.1, lwd=0.1, col = 'springgreen')

legend("topleft", c('Deutsch', 'Nicht Deutsch'), pch=16, inset=0.05, bg="white", col = c('purple3', 'springgreen'), pt.cex=1.3)
dev.off()

# Zensus, Natiuonalitaet
pdf('ze_s_nationalitaet.pdf')
par(mar=c(0,0,0,0))
plot(b, lwd=1, col= bgcol)

points(ze_s[ze_s$STAATSANGE == 1, ], pch=16, cex=0.4, lwd=0.1, col = 'purple3')
points(ze_s[ze_s$STAATSANGE == 2, ], pch=16, cex=0.4, lwd=0.1, col = 'springgreen')

legend("topleft", c('Deutsch', 'Nicht Deutsch'), pch=16, inset=0.05, bg="white", col = c('purple3', 'springgreen'), pt.cex=1.3)
dev.off()

pdf('ze_g_nationalitaet.pdf')
par(mar=c(0,0,0,0))
plot(b, lwd=1, col= bgcol)

points(ze_g[ze_g$STAATSANGE == 1, ], pch=16, cex=0.1, lwd=0.1, col = 'purple3')
points(ze_g[ze_g$STAATSANGE == 2, ], pch=16, cex=0.1, lwd=0.1, col = 'springgreen')

legend("topleft", c('Deutsch', 'Nicht Deutsch'), pch=16, inset=0.05, bg="white", col = c('purple3', 'springgreen'), pt.cex=1.3)
dev.off()

