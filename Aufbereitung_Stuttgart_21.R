#############################
# Aufbereitung Stuttgart 21 #
#############################

library(Hmisc)

bearbeiter = 'Alex'

if(bearbeiter == 'Alex') {
  setwd('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Presi/Statistical-Practical')

# loading data
  Neu <- spss.get('/home/alex/Schreibtisch/Uni/statistisches_praktikum/Ukneib/Buergerumfrage_Kneib.SAV',
                 use.value.labels=TRUE)
} else {
  setwd('/home/khusmann/mnt/U/Promotion/Kurse/Stat_Praktikum/Auswertung/Neue_Daten/')
  Neu <- spss.get('/home/khusmann/mnt/U/Promotion/Kurse/Stat_Praktikum/Daten_Kneib2/Daten_Kneib/Buergerumfrage_Kneib.SAV',
                use.value.labels=TRUE)
}



# names
names(Neu) <- c('Bewertung Wohngegend', 'Meinung zu Stuttgart 21', 'Personenzahl im Haushalt',
                'Monatliches Netto Haushaltseinkommen', 'Altersklasse Befragter', 'Geschlecht',
                'Familienstand', 'Nationalität', 'Stadtbezirk', 'Stadtteil', 'X', 'Y')

# Omitting NA's
Neu <- na.omit(Neu)

# Cloning data
Neu2 <- Neu

# Assigning new factor names
# Da die 'gam' function keine geordneten factors verarbeiten kann müssen die Kategorien als 
# Zahlen übersetzt werden. Wobei 6 = Keine Angabe, 5 = Sehr schlecht, ..., 1 = Sehr gut.
Neu2$`Bewertung Wohngegend` <- ''
Neu2$`Bewertung Wohngegend`[Neu$`Bewertung Wohngegend` == 'Sehr zufrieden'] <- 1
Neu2$`Bewertung Wohngegend`[Neu$`Bewertung Wohngegend` == 'Zufrieden'] <- 2
Neu2$`Bewertung Wohngegend`[Neu$`Bewertung Wohngegend` == 'Teils / teils'] <- 3
Neu2$`Bewertung Wohngegend`[Neu$`Bewertung Wohngegend` == 'Unzufrieden'] <- 4
Neu2$`Bewertung Wohngegend`[Neu$`Bewertung Wohngegend` == 'Sehr unzufrieden'] <- 5
Neu2$`Bewertung Wohngegend`[Neu$`Bewertung Wohngegend` == 'Weiß nicht' | Neu$`Bewertung Wohngegend` =='Keine Angabe'] <- 6

Neu2$`Meinung zu Stuttgart 21` <- ''
Neu2$`Meinung zu Stuttgart 21` [Neu$`Meinung zu Stuttgart 21` == 'Sehr gute Meinung'] <- 1
Neu2$`Meinung zu Stuttgart 21` [Neu$`Meinung zu Stuttgart 21` == 'Gute Meinung'] <- 2
Neu2$`Meinung zu Stuttgart 21` [Neu$`Meinung zu Stuttgart 21` == 'Teils / teils'] <- 3
Neu2$`Meinung zu Stuttgart 21` [Neu$`Meinung zu Stuttgart 21` == 'Schlechte Meinung'] <- 4
Neu2$`Meinung zu Stuttgart 21` [Neu$`Meinung zu Stuttgart 21` == 'Sehr schlechte Meinung'] <- 5
Neu2$`Meinung zu Stuttgart 21` [Neu$`Meinung zu Stuttgart 21` == 'Weiß nicht' | Neu$`Meinung zu Stuttgart 21` =='Keine Angabe'] <- 6

Neu2$Nationalität <- ''
Neu2$Nationalität[Neu$Nationalität == 'Deutsch <einschl. deutsch und andere'] <- 'Deutsch'
Neu2$Nationalität[Neu$Nationalität == 'Andere'] <- 'Nicht Deutsch'

Neu2$`Personenzahl im Haushalt` <- ''
Neu2$`Personenzahl im Haushalt`[Neu$`Personenzahl im Haushalt` == '1 Person'] <- 1
Neu2$`Personenzahl im Haushalt`[Neu$`Personenzahl im Haushalt` == '2 Personen'] <- 2
Neu2$`Personenzahl im Haushalt`[Neu$`Personenzahl im Haushalt` == '3 Personen'] <- 3
Neu2$`Personenzahl im Haushalt`[Neu$`Personenzahl im Haushalt` == '4 Personen'] <- 4
Neu2$`Personenzahl im Haushalt`[Neu$`Personenzahl im Haushalt` == '5 Personen und mehr'] <- 5

Neu2$`Monatliches Netto Haushaltseinkommen` <- ''
Neu2$`Monatliches Netto Haushaltseinkommen`[Neu$`Monatliches Netto Haushaltseinkommen` == 'unter 900€'] <- 1
Neu2$`Monatliches Netto Haushaltseinkommen`[Neu$`Monatliches Netto Haushaltseinkommen` == '900 bis unter 2000€'] <- 2
Neu2$`Monatliches Netto Haushaltseinkommen`[Neu$`Monatliches Netto Haushaltseinkommen` == '2000 bis unter 2900€'] <- 3
Neu2$`Monatliches Netto Haushaltseinkommen`[Neu$`Monatliches Netto Haushaltseinkommen` == '2900 bis unter 4000€'] <- 4
Neu2$`Monatliches Netto Haushaltseinkommen`[Neu$`Monatliches Netto Haushaltseinkommen` == '4000 bis unter 5000€'] <- 5
Neu2$`Monatliches Netto Haushaltseinkommen`[Neu$`Monatliches Netto Haushaltseinkommen` == '5000€ und mehr'] <- 6

Neu2$`Altersklasse Befragter` <- ''
Neu2$`Altersklasse Befragter`[Neu$`Altersklasse Befragter` == '15 bis unter 25'] <- 1
Neu2$`Altersklasse Befragter`[Neu$`Altersklasse Befragter` == '25 bis unter 35'] <- 2
Neu2$`Altersklasse Befragter`[Neu$`Altersklasse Befragter` == '35 bis unter 45'] <- 3
Neu2$`Altersklasse Befragter`[Neu$`Altersklasse Befragter` == '45 bis unter 55'] <- 4
Neu2$`Altersklasse Befragter`[Neu$`Altersklasse Befragter` == '55 bis unter 65'] <- 5
Neu2$`Altersklasse Befragter`[Neu$`Altersklasse Befragter` == '65 Jahre und älter'] <- 6

Neu2$Familienstand <- ''
Neu2$Familienstand[Neu$Familienstand == 'ledig'] <- 'ledig'
Neu2$Familienstand[Neu$Familienstand == 'verheiratet; eingetragene Lebenspartnerschaft'] <- 'verheiratet'
Neu2$Familienstand[Neu$Familienstand == 'verwitwet'] <- 'verwitwet'
Neu2$Familienstand[Neu$Familienstand == 'geschieden'] <- 'geschieden'

write.table(Neu2, file="Stuttgart21_aufbereitet.csv", sep=";", col.names=TRUE, row.names=FALSE, quote=FALSE)
