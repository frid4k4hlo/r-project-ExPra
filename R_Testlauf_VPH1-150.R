#30.3 - 14:40 geladen

read.csv("expradaten2.csv")

#packages installieren

install.packages("psych")
install.packages("tidyverse")
install.packages("haven")
library("psych")
library("tidyverse")
library("haven")

#Datensatz machen

dataframe <- read.csv("expradaten2.csv")

#Überblick

str(dataframe)
head(dataframe) # Zeigt die ersten 6 Zeilen an
View(dataframe) # Das gleiche, wie wenn man rechts im environment auf ein Daten-Objekt klickt
colnames(dataframe) #Variablennamen ausgeben lassen...

# Überschreibt alle Spaltennamen des dataframes auf einmal 
#alle Spaltennamen umgeschrieben und die unnötigen als unnötige benannt

dataframe_renamed <- setNames(dataframe, c("id", "Datum_Abgeschickt", "Schlussseite", "Sprache", "Zufallsstartwert", "Datum_erste_aktivität", "Datum_letzte_aktivität", "unnoetig1", "unnoetig2", "Alter", "Deutschkenntnisse", "Hauptwohnsitz", "Sehbeeinträchtigung", "unnoetig3", "Bezug_zum_Gesundheitssystem", "Geschlecht", "Bildungsabschluss", "Bildungsabschluss_Sonstiges", "Wo_Medizinische_Leistung", "Wo_Krankenversicherung", "pol_Orientierung", "Anzahl_pers_Kontakte_mit_Gesundheitssystem", "Systemabhängigkeit_Gruppe", "unnoetig4", "unnoetig5", "unnoetig6", "MC_Anteil", "MC_Quelle", "MC_Jahr_falsch", "MC_Jahr_korrekt", "M_Check_pers_Auswirkung", "M_Check_Erfolg", "M_Check_Kontrollfrage", "Studienziel", "unnoetig7", "unnoetig8", "Gesamtzeit", "unnoetig9","unnoetig10","unnoetig11","unnoetig12","unnoetig13","unnoetig14","unnoetig15","unnoetig16","unnoetig17","unnoetig18","unnoetig19","unnoetig20","unnoetig21","unnoetig22","unnoetig23","unnoetig24","unnoetig25","unnoetig26","unnoetig27","unnoetig28","unnoetig29","unnoetig30","unnoetig31","unnoetig32","unnoetig33","unnoetig34","unnoetig35","unnoetig36","unnoetig37","unnoetig38","unnoetig39","unnoetig40","unnoetig41","unnoetig42","unnoetig43","unnoetig44","unnoetig45","unnoetig46"))

#Vektorspalten zählen

ncol(dataframe)

#neuen Datensatz abrufen

dataframe_renamed

#Datensatz abgespeichert - Befehl von Björn Walther - Daten aus R exportieren

write.csv2(dataframe_renamed, file="Daten_VPH1-150_renamed.csv")

#alle unnötigen Spalten aus dem Datensatz löschen

str(dataframe_renamed)
summary(dataframe_renamed)
head(dataframe_renamed)

#neuer Datensatz auch noch die unnötigen Fragen weggenommen

dataframe_clean_and_renamed <- dataframe_renamed[, c("id", "Datum_Abgeschickt", "Schlussseite", "Sprache", "Zufallsstartwert", "Datum_erste_aktivität", "Datum_letzte_aktivität", "Alter", "Deutschkenntnisse", "Hauptwohnsitz", "Sehbeeinträchtigung", "Bezug_zum_Gesundheitssystem", "Geschlecht", "Bildungsabschluss", "Bildungsabschluss_Sonstiges", "Wo_Medizinische_Leistung", "Wo_Krankenversicherung", "pol_Orientierung", "Anzahl_pers_Kontakte_mit_Gesundheitssystem", "Systemabhängigkeit_Gruppe", "MC_Anteil", "MC_Quelle", "MC_Jahr_falsch", "MC_Jahr_korrekt", "M_Check_pers_Auswirkung", "M_Check_Erfolg", "M_Check_Kontrollfrage", "Studienziel", "Gesamtzeit")]
dataframe_clean_and_renamed 
View(dataframe)
View(dataframe_renamed)
View(dataframe_clean_and_renamed)

#package für neuen Datensatz ohne Zeilen mit VPS die nicht fertig gemacht haben

install.packages("dplyr")
library(dplyr)

#hier diesen Datensatz erstellen

neuer_datensatz <- dataframe_clean_and_renamed %>%
  filter(!is.na(Schlussseite) & Schlussseite >= 11)

#abrufen und ansehen

neuer_datensatz
View(neuer_datensatz)

#Daten abspeichern

write.csv2(neuer_datensatz, file="Daten_VPH1-150_clean.csv")

#2 Gruppen Datensaetze je nach Systemabhängigkeit

dat_System_hoch <- neuer_datensatz[neuer_datensatz$Systemabhängigkeit_Gruppe == 2, ]
dat_System_tief <- neuer_datensatz[neuer_datensatz$Systemabhängigkeit_Gruppe == 1, ]
dat_System_hoch
dat_System_tief
View(dat_System_hoch)
View(dat_System_tief)

#Häufigkeitstabellen

table(neuer_datensatz$MC_Anteil)
table(neuer_datensatz$MC_Quelle)
table(neuer_datensatz$MC_Jahr_falsch)
table(neuer_datensatz$MC_Jahr_korrekt)

#Häufigkeitstabellen für die zwei Gruppen

#eine gruppe

table(dat_System_hoch$MC_Anteil)
table(dat_System_hoch$MC_Quelle)
table(dat_System_hoch$MC_Jahr_falsch)
table(dat_System_hoch$MC_Jahr_korrekt)

#andere gruppe

table(dat_System_tief$MC_Anteil)
table(dat_System_tief$MC_Quelle)
table(dat_System_tief$MC_Jahr_falsch)
table(dat_System_tief$MC_Jahr_korrekt)

#Zahl vorne nehmen (Recoding) - weil sonst ist bei 1 (stimme überhaupt nicht zu) und bei 7 (stimme voll und ganz zu)

neuer_datensatz$M_Check_pers_Auswirkung <- as.numeric(substr(neuer_datensatz$M_Check_pers_Auswirkung, 1, 1))

#Geschlecht daraus Faktoren gemacht

neuer_datensatz$Geschlecht <- factor(neuer_datensatz$Geschlecht, levels = c(1, 2, 3), labels = c("männlich", "weiblich", "divers"))

#aus (ob man Bezug zum Gesundheitssystem hat einen Faktor kodieren)

neuer_datensatz$Bezug_zum_Gesundheitssystem <- factor(neuer_datensatz$Bezug_zum_Gesundheitssystem, levels = c(1, 2), labels = c("Ja", "Nein"))

#Häufigkeitstabelle für Bildungsabschluss

table(neuer_datensatz$Bildungsabschluss)

#Häufigkeitstabelle für Medizinische Leistung

table(neuer_datensatz$Wo_Medizinische_Leistung)

#Häufigkeitstabelle für Krankenversicherung

table(neuer_datensatz$Wo_Krankenversicherung)

#pol_Orientierung Faktor kodieren

#dafür zuerst keine Angabe entfernen

neuer_datensatz_faktor_kodierung_pol_einstellung <- neuer_datensatz[neuer_datensatz$pol_Orientierung != "Keine Angabe",]

#pol_Orientierung Faktor kodieren

neuer_datensatz_faktor_kodierung_pol_einstellung$pol_Orientierung <- factor(neuer_datensatz_faktor_kodierung_pol_einstellung$pol_Orientierung, levels = c("1 – Sehr links","2 – Links","3 – Eher links","4 – Mitte","5 – Eher rechts","6 – Rechts","7 – Sehr rechts"))

#einige Berechnungen mit Alter

min(neuer_datensatz$Alter) # falls noch missings in den Daten sind,  na.rm = TRUE angeben
max(neuer_datensatz$Alter)
mean(neuer_datensatz$Alter) # arithmetisches Mittel
median(neuer_datensatz$Alter) # Median
sd(neuer_datensatz$Alter) # Standardabweichung
var(neuer_datensatz$Alter) # Varianz

#F. Stichprobe berichten

#•	finale Stichprobengröße N
#•	Alter: Mittelwert, SD, Range
#•	Geschlecht: n und %
#•	Bildungsabschluss: n und %
#•	evtl. Sprachlevel, Hauptwohnsitz etc.
#•	pro Experimentalgruppe dieselben Infos
#•	prüfen, ob Gruppen vor Analyse grob vergleichbar sind

#Test um ein Boxplot mit den politischen Einstellungen zu machen

#Pakete laden
library(ggplot2)
#Likert-Werte (ohne "Keine Angabe")
werte <- 1:6
haeufigkeit <- c(6, 30, 20, 16, 7, 2)
#Daten "aufklappen" (jede Antwort einzeln)
likert_daten_pol_einstellung <- rep(werte, haeufigkeit)
#Dataframe erstellen
df <- data.frame(Antwort = likert_daten_pol_einstellung)
df
#Base R Boxplot
boxplot(df$Antwort, main = "Boxplot Verteilungen politische Einstellung", ylab = "Antwortkategorie", horizontal = TRUE)

# Hauptanalysen

###

#•	Chi-Quadrat-Tests für Gruppenunterschiede bei 4 Multiple-Choice-Fragen
#•	t-Test für Manipulationscheck

#Dann zusätzlich:
  
#•	Voraussetzungen prüfen:
  #o	bei Chi-Quadrat: erwartete Häufigkeiten
  #o	bei t-Test: Die abhängige Variable sollte in jeder Gruppe ungefähr normalverteilt sein, Varianzhomogenität, Histogramm, GGPlot, Shapiro-Wilk? - schauen in VL ReDat

  #o	wenn Voraussetzungen nicht passen:
  #o	Fisher-Test statt Chi-Quadrat?
  #o	Welch-t-Test statt klassischer t-Test?
    
###
    
#Häufigkeiten der Antwortalternativen in den MC Fragen für Gruppe 1

# Gruppe 1
table(neuer_datensatz$MC_Anteil[neuer_datensatz$Systemabhängigkeit_Gruppe == 1])
table(neuer_datensatz$MC_Quelle[neuer_datensatz$Systemabhängigkeit_Gruppe == 1])
table(neuer_datensatz$MC_Jahr_falsch[neuer_datensatz$Systemabhängigkeit_Gruppe == 1])
table(neuer_datensatz$MC_Jahr_korrekt[neuer_datensatz$Systemabhängigkeit_Gruppe == 1])


#Häufigkeiten der Antwortalternativen in den MC Fragen für Gruppe 2

# Gruppe 2
table(neuer_datensatz$MC_Anteil[neuer_datensatz$Systemabhängigkeit_Gruppe == 2])
table(neuer_datensatz$MC_Quelle[neuer_datensatz$Systemabhängigkeit_Gruppe == 2])
table(neuer_datensatz$MC_Jahr_falsch[neuer_datensatz$Systemabhängigkeit_Gruppe == 2])
table(neuer_datensatz$MC_Jahr_korrekt[neuer_datensatz$Systemabhängigkeit_Gruppe == 2])

#Chi-Quadrat Test

# MC_Anteil
tab1 <- table(neuer_datensatz$MC_Anteil, neuer_datensatz$Systemabhängigkeit_Gruppe)
chisq.test(tab1)

# MC_Quelle
tab2 <- table(neuer_datensatz$MC_Quelle, neuer_datensatz$Systemabhängigkeit_Gruppe)
chisq.test(tab2)

# MC_Jahr_falsch
tab3 <- table(neuer_datensatz$MC_Jahr_falsch, neuer_datensatz$Systemabhängigkeit_Gruppe)
chisq.test(tab3)

# MC_Jahr_korrekt
tab4 <- table(neuer_datensatz$MC_Jahr_korrekt, neuer_datensatz$Systemabhängigkeit_Gruppe)
chisq.test(tab4)


#T-Test Manipulationscheck (Frage M_Check_pers_Auswirkung)

#Zahl vorne nehmen (Recoding) - weil sonst ist bei 1 (stimme überhaupt nicht zu) und bei 7 (stimme voll und ganz zu)
neuer_datensatz$M_Check_pers_Auswirkung <- as.numeric(substr(neuer_datensatz$M_Check_pers_Auswirkung, 1, 1))

t.test(M_Check_pers_Auswirkung ~ Systemabhängigkeit_Gruppe, data = neuer_datensatz, alternative = "less")


#T-Test Manipulationscheck (Frage M_Check_Erfolg)

# Zahl vorne nehmen (Recoding)
neuer_datensatz$M_Check_Erfolg <- as.numeric(substr(neuer_datensatz$M_Check_Erfolg, 1, 1))

t.test(M_Check_Erfolg ~ Systemabhängigkeit_Gruppe, data = neuer_datensatz, alternative = "less")
