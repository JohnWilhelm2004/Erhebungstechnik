
#Library installieren 
library(tidyverse)

#Reinladen der Umfrage ins global Enviorment
survey.data <- read.csv("results-survey_cleaned.csv")

#Frage/Plot 1 - Ein abbild zeigen von dem was wir eigentlich grade Nutzen
#Die Idee hinter diesem Plot ist für die Analyse dann um den Roten Faden zu halten erstmal zu zeigen was 
#die Studenten eigentlich Nutzen, damit wir nachher Zeigen können das obwohl KI und Digitale Lernmittel
#häufig genutzt werden sie nicht mit einer erfahrung

#Wir extrahieren die passenden Informationen aus unserer Umfrage für unseren Plot
plot.1.data <- survey.data %>%
  #Wir wählen uns die wichtigen Daten aus der Befragung aus (Hier: Die Daten zur Nutzung)
  select(starts_with("Nutzung")) %>%
  #Wir fassen alles zusammen indem wir den Durschnitt für alle mittel ausrechnen und fehlende Werte entfernen
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  #Wir Formen unser Material um damit wir das ganze mit ggplot vernünftig erstellen 
  #können und bennen Material und den Score richtig um 
  pivot_longer(everything(), names_to = "Material", values_to = "Score") %>%
  #Wir entfernen von den Spalten die bleibsel der csv Datei und Sortieren
  #es so das zuerst die Materialspalte und dann die Scorespalte kommt
  mutate(
    Material = str_remove_all(Material, "Nutzung_|_Num"),
    Material = fct_reorder(Material, Score)
  )

#Wir erstellen unseren Plot für das erste Thema 
ggplot(plot.1.data, aes(x = Score, y = Material)) +
  #Wir erstellen einen kleinen Punkt an der Spitze der Nutzung
  geom_point(size = 3, aes(color = Material)) +
  #Wir zeichnen eine Linie die bis zu dem eben gezeichneten Punkt geht 
  geom_segment(aes(
    x = 0,
    xend = Score,
    y = Material,
    yend = Material
  )) +
  #Hinzufügen von Achsenbeschriftungen
  labs(x = "Durchschnittliche Nutzung (Von 1 bis 5)",
       y = "Material") +
  #Minimal Theme und Farbpallete für Optik vom Plots
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top", 
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16), 
    axis.text.y = element_text(color = "black", size = 11)
  ) +
  scale_color_viridis_d(option = "mako") 

#Frage/Plot 2 - Zeigen das viele Studenten sich Zeitersparnis beim Studium wünschen

#Um für diese Frage einen guten Plot zu erstellen müssen wir uns erstmal
#anschauen wie Zeitaufwand und Material Korrelieren

cor.time <- survey.data %>%
  summarise(across( 
    starts_with("Nutzung"), #Für alle Spalten die mit "Nutzung" anfangen in der csv
    ~ cor(.x, Effekt_Zeitaufwand_Num, use = "complete.obs") #Berechnen wir die Korrelation mit dem Zeitaufwand
  )) %>%
  #Wir beschriften unsere Zeilen wieder um und formen sie in lange Reihen um
  pivot_longer(everything(), names_to = "Material", values_to = "Zeit.Effekt")

#jetzt bauen wir diese Korrelationswerte auch für den zusammenhang mit dem Verständnis zusammen

cor.understanding <- survey.data %>%
  summarise(across( 
    starts_with("Nutzung"), #Wir nehmen uns wieder alle Nutzungsspalten
    ~ cor(.x, Qualitaet_Verstehen_Num, use = "complete.obs") #Berechnen nur diesesmal die Korrelation mit dem Verständnis
  )) %>%
  
  #Wir beschriften unsere Zeilen wieder um und formen sie in lange Reihen um
  pivot_longer(everything(), names_to = "Material", values_to = "Verstaendnis.Effekt")


#jetzt haben wir die Korrelationsdaten von Zeit und Verständnis wir wollen jetzt diese
#Kombinieren in einen Datensatz den wir dann in ggplot übergeben können

plot.2.data <- left_join(cor.time, cor.understanding, by = "Material") %>%
  mutate(
    Material = str_remove_all(Material, "Nutzung_|_Num"), #Wir entfernen wieder die Umfrage überbleibsel
    Material = fct_reorder(Material, Verstaendnis.Effekt) #Und ordnen die Spalten dann neu an 
  )

#Jetzt können wir unseren finalen ggplot Erstellen das hier wird ein Dumbell Plot 
ggplot(plot.2.data) +
  
  #Dieser Teil baut die Linie in der mitte die die Beiden Korrelationswerte miteinander verbindet 
  geom_segment(aes(
    y = Material, yend = Material,
    x = Zeit.Effekt, xend = Verstaendnis.Effekt
  ), color = "grey70") +
  
  #Hier erstellen wir den Punkt für die Zeit auf dem Graphen
  geom_point(aes(x = Zeit.Effekt, y = Material, color = "Zeitaufwand"), size = 3) +
  
  #Das Hier ist der Punkt für das Verständnis
  geom_point(aes(x = Verstaendnis.Effekt, y = Material, color = "Verständnis"), size = 3) +
  
  #Hier nehmen wir unsere Standard Farbpalette 
  scale_color_viridis_d(option = "mako", begin = 0.2, end = 0.8) +
  #Wir erstellen Überschrift und Achsenbeschriftungen für die Verständnis
  labs(title = "Effizienz Trugschluss",
       subtitle = "Vergleich der Korrelation zwischen Zeitaufwand vs. Korrelation mit Verständnis",
       x = "Korrelationsstärke (r)\n ← Spart Zeit/ Wenig Verständnis | Kostet Zeit / Hohes Verständnis →",
       y = NULL,
       color = "Gemessener Effekt"
       ) +
  #Wir editieren unser Theme minimal etwas damit es unseren Anforderungen entspricht
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top", 
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16), 
    axis.text.y = element_text(color = "black", size = 11)
  )
  
