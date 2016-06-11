#######################################
# Descriptive Statistics Stuttgart 21 #
#######################################

# loading packages
require(ggplot2);require(reshape2);require(colorspace);require(gridExtra);require(scales)
require(rgdal);require(rgeos);
require(ggmap)

# loading data
dataS <- read.csv2('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Auswertung/Neue_Daten/Stuttgart21_aufbereitet.csv')

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
count8 <- as.data.frame(table(dataS$Nationanlität))

# plots
theme_set(theme_bw(12)) # Grafik theme
colo <- diverge_hsv(3) # different color palette

# Bewertung Wohngegend
# rearranging of groups
count1$Var1 <- factor(count1$Var1, levels = c('Sehr gut', 'Gut', 'Neutral', 'Schlecht', 
                                                          'Sehr schlecht', 'Keine Angabe'))
# Plotting              
Wohngegend <- ggplot(count1, aes(x = Var1, y = Freq)) + 
              geom_bar(stat="identity", position=position_dodge(), col = 'black', alpha = .5, fill = colo[1]) +
              theme(legend.position = 'bottom', legend.title = element_blank()) +
              labs(x = 'Bewertung Wohngegend', y = 'Häufigkeit', title = NULL)
Wohngegend

# Meinung über Stuttgart 21
# rearranging of groups
count2$Var1 <- factor(count2$Var1, levels = c('Sehr gut', 'Gut', 'Neutral', 'Schlecht', 
                                              'Sehr schlecht', 'Keine Angabe'))
# Plotting  
Stuttgart21 <- ggplot(count2, aes(x = Var1, y = Freq)) + 
               geom_bar(stat="identity", position=position_dodge(), col = 'black', alpha = .5, fill = colo[1]) +
               theme(legend.position = 'bottom', legend.title = element_blank()) +
               labs(x = 'Meinung zu Stuttgart 21', y = 'Häufigkeit', title = NULL)      
Stuttgart21

# Personen im Haushalt
# rearranging of groups
count3$Var1 <- factor(count3$Var1, levels = c('1', '2', '3', '4', 
                                              '>5'))
# Plotting              
Personen <- ggplot(count3, aes(x = Var1, y = Freq)) + 
            geom_bar(stat="identity", position=position_dodge(), col = 'black', alpha = .5, fill = colo[1]) +
            theme(legend.position = 'bottom', legend.title = element_blank()) +
            labs(x = 'Personenanzahl im Haushalt', y = 'Häufigkeit', title = NULL)
Personen

# Monatliches Netto haushaltseinkommen
# # rearranging of groups
count4$Var1 <- factor(count4$Var1, levels = c('<900', '900-<2000', '2000-<2900', '2900-<4000', 
                                              '4000-<5000', '>5000'))
# Plotting              
Einkommen <- ggplot(count4, aes(x = Var1, y = Freq)) + 
             geom_bar(stat="identity", position=position_dodge(), col = 'black', alpha = .5, fill = colo[1]) +
             theme(legend.position = 'bottom', legend.title = element_blank()) +
             labs(x = 'Netto Einkommen der Haushalte', y = 'Häufigkeit', title = NULL)
Einkommen

# Altersklasse
# # rearranging of groups
count5$Var1 <- factor(count5$Var1, levels = c('15-<25', '25-<35', '35-<45', '45-<55', 
                                              '55-<65', '>65'))
# Plotting              
Altersklasse <- ggplot(count5, aes(x = Var1, y = Freq)) + 
                geom_bar(stat="identity", position=position_dodge(), col = 'black', alpha = .5, fill = colo[1]) +
                theme(legend.position = 'bottom', legend.title = element_blank()) +
                labs(x = 'Altersklassen', y = 'Häufigkeit', title = NULL)
Altersklasse

# Geschlecht
# rearranging of groups
count6$Var1 <- factor(count6$Var1, levels = c('Männlich', 'Weiblich'))

# Plotting              
Geschlecht <- ggplot(count6, aes(x = Var1, y = Freq)) + 
              geom_bar(stat="identity", position=position_dodge(), col = 'black', alpha = .5, fill = colo[1]) +
              theme(legend.position = 'bottom', legend.title = element_blank()) +
              labs(x = 'Geschlecht', y = 'Häufigkeit', title = NULL)
Geschlecht

# Familienstand
# Plotting              
Familienstand <- ggplot(count7, aes(x = Var1, y = Freq)) + 
                 geom_bar(stat="identity", position=position_dodge(), col = 'black', alpha = .5, fill = colo[1]) +
                 theme(legend.position = 'bottom', legend.title = element_blank()) +
                 labs(x = 'Familienstand', y = 'Häufigkeit', title = NULL)
Familienstand

# Nationalität
# Plotting              
Nationalität <- ggplot(count8, aes(x = Var1, y = Freq)) + 
                geom_bar(stat="identity", position=position_dodge(), col = 'black', alpha = .5, fill = colo[1]) +
                theme(legend.position = 'bottom', legend.title = element_blank()) +
                labs(x = 'Nationalität', y = 'Häufigkeit', title = NULL)
Nationalität

# grid plot
grid.arrange(Altersklasse, Einkommen, Familienstand, Geschlecht, Nationalität, 
             Personen, Stuttgart21, Wohngegend)
