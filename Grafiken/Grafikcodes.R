#Hier ein paar Plots für die Fragen in Erhebungstechniken 


#Library installieren 
library(tidyverse)
library(psych)

#Reinladen der Umfrage ins global Enviorment
survey.data <- read.csv("results-survey_cleaned.csv")

#Frage 1 - Bevorzugen Studenten Strukturiertes oder Unstrukturiertes Lernen? 

#Unterscheidung: Wer ist sturkturiert und wer schiebt auf?


survey.data <- survey.data %>%
  mutate(
    Lerntyp = case_when(
      #1. Bedingung  Strukturierte Menschen bevorzugen Pflichtaufgaben
      Einstellung_Pflicht_Ball_Num >= 4 ~ "Strukturiert",
      
      #2. Bedingung Strukturierte die neutral zu Pflicht aufgaben 
      # stehen aber trotzdem nicht aufschieben
      Einstellung_Pflicht_Ball_Num == 3 & Einstellung_Aufschieben_Num <= 2 ~ "Strukturiert",
      
      #Alle anderen gelten als weniger/unstrukturiert 
      TRUE ~ "Unstrukturiert"
    )
  )

ggplot(data = survey.data, aes(x = Lerntyp)) +
  geom_bar() 

## Kommentar: Die Items korrelieren nicht wirklich miteinander (Cronbachs alpha <0.7)
# items_strukturiert <- survey.data %>%
#    select(
#        Einstellung_Pflicht_Ball_Num,
#        Einstellung_Aufschieben_Num
#      )
# alpha(items_strukturiert, check.keys = TRUE)
## Reliability analysis   
## Call: alpha(x = items_strukturiert, check.keys = TRUE)
## 
## raw_alpha
## 0.5
## --> vielleicht lieber die Items einzeln betrachten?


#Frage 2 - Welche Materialien bringen die höchste Zufriedenheit?


#Aufbereiten der Daten 
material.data <- survey.data %>% 
  #Aus dem Datensatz den zufriendenheitsscore und alle Lernmaterialen extrahieren 
  select(Zufriedenheit_Score, starts_with("Nutzung_"))  %>%
  
  #Verschiebung der Daten damit andere Funktionen später besser Funktionieren
  pivot_longer(
    cols = starts_with("Nutzung_"),
    names_to = "Material",
    values_to = "Nutzungshaeufigkeit"
  )

#"Aufhübschen" der Namensdaten für die Finale Darstellung im Plot
material.data <- material.data %>% 
  mutate(
    Material = str_remove(Material, "Nutzung_"),
    Material = str_remove(Material, "_Num")
  )

#Jetzt werten wir die Daten aus 
#Wir sortieren nach Leuten mit intensiver Nutzung (Score >= 4) 
#Nehmen deren Zufriedenheitswerte und errechnen daraus einen Durchschnitt
#Dann vergleichen wir diese 
satisfaction.analysis <- material.data %>%
  
  #Filtern nach leuten die das Material häufig nutzen
  filter(Nutzungshaeufigkeit >= 4) %>%
  
  #Gruppieren dieser Personen nach Gruppen
  group_by(Material) %>% 
  
  #Für jede Gruppe dann die Durchschnittliche Zufriedenheit ausrechnen
  summarise(
    avg.satisfaction = mean(Zufriedenheit_Score, na.rm = TRUE)
  ) %>%
  
  #Sortieren nach größe (Sieht später schöner aus)
  arrange(desc(avg.satisfaction))

#####################################
# Lollipopdiagramm

ggplot(data = satisfaction.analysis, 
       aes(x = reorder(Material, avg.satisfaction), y = avg.satisfaction)) +
  
  #Lollipopdarstellung
  geom_segment(aes(xend = Material, yend = 0), color = "grey") +
  geom_point(aes(color = avg.satisfaction), size = 5) +
  
  #Beschriftung
  geom_text(aes(label = round(avg.satisfaction, 1)), 
            color = "white", size = 2) + # Zahl im Punkt
  
  coord_flip() +
  
  scale_color_viridis_c(option = "D") + # Farbeinstellung
  
  labs(title = "Ranking der Materialzufriedenheit",
       x = "", y = "Score") +
  
  theme_minimal() +
  theme(legend.position = "none") +
  

###########################
# Boxplotdiagramm


# Rohdaten vor Zusammenfassung für genauere Darstellung
raw_plot_data <- material.data %>%
  filter(Nutzungshaeufigkeit >= 4) 

ggplot(raw_plot_data, 
       aes(x = reorder(Material, Zufriedenheit_Score, FUN = mean), 
           y = Zufriedenheit_Score)) +
  
  # Zeigt die Verteilung (Box)
  geom_boxplot(outlier.shape = NA, fill = "lightblue", alpha = 0.5) +
  
  # Zeigt die echten Datenpunkte
  geom_jitter(width = 0.2, height = 0.1, alpha = 0.3, color = "darkblue") +
  
  # Durschnittspunkt (rot)
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "red") +
  
  coord_flip() +
  
  labs(title = "Wie einig sind sich die Studierenden?",
       subtitle = "Boxplot zeigt Verteilung, roter Punkt ist der Durchschnitt",
       x = "",
       y = "Zufriedenheitsscore") +
  theme_minimal() + 



########################################


#Frage 3 - Welche Materialien werden als am effizientesten/besten
#für die Prüfungsvorbereitung empfunden´

#Dazu werden wir einen "Erfolgsscore" erstellen wir addieren die Werte zu den Themen 
#Effizienz, Motivation, Sicherheit und Zeitaufwand und kategorisieren die Leute nach
#Erfolg und schauen dann welche Gruppen welche Materialien am Häufigsten Nutzen

survey.data <- survey.data %>%
  mutate(
    #Drehen des Zeitaufwand Scores damit er richtig  eingerechnet wird:
    Effizienz_Score = 6 - Effekt_Zeitaufwand_Num,
    
    #Jetzt einfach alle Zusammenaddieren(Höchste PunktZahl ist 20)
    Erfolgs_Score = Effekt_Sicherheit_Num + Effekt_Motivation_Num + Effizienz_Score + Effekt_Relevanz_Num
  )

## Kommentar:
# items_effekt <- survey.data %>%
#   mutate(Effizienz_Score = 6 - Effekt_Zeitaufwand_Num) %>%
#   select(
#     Effekt_Sicherheit_Num,
#     Effekt_Motivation_Num,
#     Effizienz_Score,
#     Effekt_Relevanz_Num
#   )
# alpha(items_effekt, check.keys = TRUE)
## raw_alpha
## 0.51
## auch zu niedrig
## Besser:
# items_effekt <- survey.data %>%
#   select(
#     Effekt_Sicherheit_Num,
#     Effekt_Motivation_Num,
#     Effekt_Selbststaendig_Num,
#     Effekt_Stress_Num,
#     Effekt_Relevanz_Num
#   )
# alpha(items_effekt, check.keys = TRUE)
# # raw_alpha
# # 0.79 
## Außerdem: Was bedeutet Erfolg genau? Warum sind diese Items dafür ausschlaggebend

#Labeling der Personen nach Erfolgsscore
survey.data <- survey.data  %>%
  mutate(
    Erfolgs_Label = case_when(
      Erfolgs_Score >= 17 ~  "Sehr Erfolgreich"   ,
      Erfolgs_Score >= 14 ~  "Erfolgreich"        ,
      Erfolgs_Score >= 9  ~  "Weniger Erfolgreich",
      TRUE ~ "Unerfolgreich"
    )
  )

succes.analysis <- survey.data %>%
  # 1. Datensatz wieder Langziehen für die funktion
  pivot_longer(
    cols = starts_with("Nutzung_"),
    names_to = "Material",
    values_to = "Nutzung"
  ) %>%
  
  #Wieder für die Tabelle  die namen "Säubern"
  mutate(
    Material = str_remove(Material, "Nutzung_"),
    Material = str_remove(Material, "_Num")
  )

succes.material.analysis <- succes.analysis  %>%
  
  #Gruppieren nach  Erfolgslabel und nach Material
  group_by(Material, Erfolgs_Label) %>%
  
  #Erechnen der Durchschnittlichen Nutzung in jeder Lerngruppe für jedes Material 
  summarise(
    Durchschnitt_Nutzung = mean(Nutzung, na.rm = TRUE)
  )


# Definieren der logischen Reihenfolge der Faktoren
succes.material.analysis$Erfolgs_Label <- factor(
  succes.material.analysis$Erfolgs_Label, 
  levels = c("Unerfolgreich", "Weniger Erfolgreich", "Erfolgreich", "Sehr Erfolgreich")
)




#################
# Gruppiertes und sortiertes Balkendiagramm

ggplot(data = succes.material.analysis,
       aes(x = reorder(Material, Durchschnitt_Nutzung), # Sortiert Balken nach Größe
           y = Durchschnitt_Nutzung,
           fill = Material)) +
  
  geom_col(show.legend = FALSE) + # Legende ist hier redundant
  
  coord_flip() +
  
  # Der Zaubertrick: Aufteilen nach Erfolgslabel
  facet_wrap(~Erfolgs_Label, nrow = 1) + 
  
  labs(
    title = "Welche Gruppe nutzt was?",
    subtitle = "Durchschnittliche Nutzungshäufigkeit nach Erfolgstyp",
    x = "",
    y = "Avg. Nutzung (1-5)"
  ) +
  theme_minimal() +
  scale_color_viridis_d()

# Zusammenfassung der Studiengänge in MINT/Lehramt/Sonstige


table(survey.data$Fakultaet)

survey.data <- survey.data %>%
  mutate(
    Fachbereich_Gruppe = case_when(
      
      # 1. GRUPPE: LEHRAMT
      # Schnappt sich "Lehramt" und "Sachunterricht"
      str_detect(Fakultaet, regex("Lehramt|Sachunterricht", ignore_case = TRUE)) ~ "Lehramt",
      
      # 2. GRUPPE: MINT
      # Hier fassen wir alle technischen/naturwissenschaftlichen Fächer zusammen
      # Das "regex(..., ignore_case = TRUE)" sorgt dafür, dass Groß-/Kleinschreibung egal ist
      str_detect(Fakultaet, regex("Mathe|Statistik|Informatik|Physik|Chemie|Bci|Maschinenbau|Bau|Architektur|Raumplanung", ignore_case = TRUE)) ~ "MINT",
      
      # 3. GRUPPE: SONSTIGES
      # Alles was oben nicht passt (WiWi, Soziologie, Erziehungswissenschaften etc.)
      TRUE ~ "Sonstiges"
    )
  )

# KONTROLLE (Wichtig!)
# Damit siehst du sofort, ob die Zuordnung geklappt hat und wer wo gelandet ist
# Statt print(n = 50) nimmst du head(50)
survey.data %>%
  count(Fachbereich_Gruppe, Fakultaet) %>%
  head(50)


survey.data <- read.csv("results-survey_cleaned.csv")

#Frage 4 - Wie verändert sich Materialnutzung im Studienverlauf? Werden Studenten selbstständiger?
verlauf.daten <- survey.data %>%
  #Wir gruppieren die Semster zusammen damit wir bessere Aussagen treffen können da für einzelne
  #Semester die Daten nicht wirklich ausreichend sind um so genau zu unterscheiden
  mutate(
    Semestergruppe = case_when(
      Fachsemester <= 2 ~ "Start",
      Fachsemester <= 5 ~ "Mitte",
      TRUE ~ "Ende"
    )
  ) %>% 
  pivot_longer(
    cols = starts_with("Nutzung_"),
    names_to = "Materialnutzung",
    values_to = "Nutzungshäufigkeit"
  ) %>% 
  group_by(Semestergruppe, Materialnutzung) %>%
  summarise(
    avg.Nutzung = mean(Nutzungshäufigkeit, na.rm = TRUE)
  ) 

#Wir bringen Ordung in die Semestergruppensortierung damit das im Plot später richtig dargestellt wird 
verlauf.daten$Semestergruppe <- factor(verlauf.daten$Semestergruppe,
                                       levels = c("Start", "Mitte", "Ende"))

#Wir erstellen den Linienplot um die veränderung der Materialnutzung im Semes
materialnutzung.semester <- ggplot(verlauf.daten, aes(x = Semestergruppe,
                                                    y = avg.Nutzung,
                                                    group = Materialnutzung,
                                                    color = Materialnutzung)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_viridis_d() +
  theme_minimal()

materialnutzung.semester

#Frage 5 - Unterscheidet sich die allgemeine Zufriedenheit zwischen KI-Nutzern und KI-Verweigerern

#Wir Kategorisieren die Leute nach Häufigkeit der Nutzung von KI 
ai.analysis <- survey.data %>% 
  mutate(
  KI.Type = case_when(
    Nutzung_KI_Num <= 2 ~ "Wenig/Keine Nutzung",
    Nutzung_KI_Num >= 4 ~ "Starke Nutzung",
    TRUE ~ "Mittelere Nutzung"
  ))

ai.analysis$KI.Type <- factor(ai.analysis$KI.Type,
                              levels = c("Wenig/Keine Nutzung", "Mittelere Nutzung", "Starke Nutzung"))

#Wir überprüfen welche der KI Nutzungstypen besser 
ai.satisfaction <- ai.analysis %>%
  group_by(KI.Type) %>%
  summarise(
    avg.Zufriedenheit = mean(Zufriedenheit_Score, na.rm = TRUE),
    Anzahl.Personen = n()
  )

#Erstellen eines Plots
ggplot(ai.analysis, aes(x = KI.Type, y = Zufriedenheit_Score, fill = KI.Type)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  labs(title = "Macht KI Nutzung Zufriedener",
       subtitle = "Vergleich der Studienzufridenheitnach Nutzungshäufigkeit von KI",
       x = "Nutzungshäufigkeit",
       y = "Zufriedenheitsscore (1-5)") +
  theme_minimal() +
  theme(legend.position = "none")

#Frage 6 - Sind aufschieber unzufriedener?







