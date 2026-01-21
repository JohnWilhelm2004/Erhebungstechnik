
#Library installieren 
library(tidyverse)

#Reinladen der Umfrage ins global Enviorment
survey.data <- read.csv("results-survey_cleaned.csv")

#Frage/Plot 1 - Ein abbild zeigen von dem was wir eigentlich grade Nutzen

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
  theme_minimal() +
  scale_color_viridis_d() 


