#######################################
# Descriptive Statistics Stuttgart 21 #
#######################################

# loading packages
require(ggplot2);require(reshape2);require(colorspace);require(gridExtra);require(scales)
require(rgdal);require(rgeos);
require(ggmap)

# loading data
bearbeiter <- 'Alex'
if(bearbeiter == 'Alex') {
  dataS <- read.csv2('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Auswertung/Neue_Daten/Stuttgart21_aufbereitet_stadtteile.csv')
} else {
  dataS <- read.csv2('/home/khusmann/mnt/U/Promotion/Kurse/Stat_Praktikum/Auswertung/Neue_Daten/Stuttgart21_aufbereitet.csv')
}

# Ordinal/Nominal Variables

# Creating Bar plots of Variables
# counting frequencies
count1 <- as.data.frame(table(dataS$Bewertung.Wohngegend))
count2 <- as.data.frame(table(dataS$Meinung.zu.Stuttgart.21))
count3 <- as.data.frame(table(dataS$Personenzahl.im.Haushalt))
count4 <- as.data.frame(table(dataS$Monatliches.Netto.Haushaltseinkommen))
count5 <- as.data.frame(table(dataS$Altersklasse.Befragter))
count6 <- as.data.frame(table(dataS$Geschlecht))
count7 <- as.data.frame(table(dataS$Familienstand))
count8 <- as.data.frame(table(dataS$Nationalität))

# plots
colo <- diverge_hsv(3) # different color palette
FreqPlot <- function(count, labx){
  ggplot(count, aes(x = Var1, y = Freq)) + 
    geom_bar(stat="identity", position=position_dodge(), col = 'black', alpha = .5, fill = colo[1]) +
    theme(legend.position = 'bottom', legend.title = element_blank()) + theme_light(12) +
    labs(x = paste(labx), y = 'Häufigkeit', title = NULL)
}

# Bewertung Wohngegend
# rearranging of groups
count1$Var1 <- c('Sehr gut', 'Gut', 'Neutral', 'Schlecht', 
                 'Sehr schlecht', 'Keine Angabe')
count1$Var1 <- factor(count1$Var1, levels = c('Sehr gut', 'Gut', 'Neutral', 'Schlecht', 
                                                          'Sehr schlecht', 'Keine Angabe'))
# Plotting              
Wohngegend <- FreqPlot(count1, 'Bewertung Wohngegend')
Wohngegend

# Meinung über Stuttgart 21
# rearranging of groups
count2$Var1 <- c('Sehr gut', 'Gut', 'Neutral', 'Schlecht', 
                 'Sehr schlecht', 'Keine Angabe')
count2$Var1 <- factor(count2$Var1, levels = c('Sehr gut', 'Gut', 'Neutral', 'Schlecht', 
                                              'Sehr schlecht', 'Keine Angabe'))
# Plotting  
Stuttgart21 <- FreqPlot(count2, 'Meinung zu Stuttgart 21')
Stuttgart21

# Personen im Haushalt
# rearranging of groups
count3$Var1 <- c('1', '2', '3', '4', 
                 '>5')
count3$Var1 <- factor(count3$Var1, levels = c('1', '2', '3', '4', 
                                              '>5'))
# Plotting              
Personen <- FreqPlot(count3, 'Personenanzahl im Haushalt')
Personen

# Monatliches Netto haushaltseinkommen
# # rearranging of groups
count4$Var1 <- c('<900', '900-<2000', '2000-<2900', '2900-<4000', 
                 '4000-<5000', '>5000')
count4$Var1 <- factor(count4$Var1, levels = c('<900', '900-<2000', '2000-<2900', '2900-<4000', 
                                              '4000-<5000', '>5000'))
# Plotting              
Einkommen <- FreqPlot(count4, 'Netto Einkommen der Haushalte')
Einkommen

# Altersklasse
# # rearranging of groups
count5$Var1 <- c('15-<25', '25-<35', '35-<45', '45-<55', 
                 '55-<65', '>65')
count5$Var1 <- factor(count5$Var1, levels = c('15-<25', '25-<35', '35-<45', '45-<55', 
                                              '55-<65', '>65'))
# Plotting              
Altersklasse <- FreqPlot(count5, 'Altersklassen')
Altersklasse 

# Geschlecht
# rearranging of groups
count6$Var1 <- factor(count6$Var1, levels = c('Männlich', 'Weiblich'))

# Plotting              
Geschlecht <- FreqPlot(count6, 'Geschlecht')
Geschlecht

# Familienstand
# Plotting              
Familienstand <- FreqPlot(count7, 'Familienstand')
Familienstand 

# Nationalität
# Plotting              
Nationalität <- FreqPlot(count8, 'Nationalität')
Nationalität

# grid plot
pdf('./Essay/Pictures/BarData.pdf', height = 8, width = 8)
bb <- grid.arrange(Altersklasse, Einkommen, Familienstand, Geschlecht, Nationalität, 
             Personen, Stuttgart21, Wohngegend)
dev.off()
