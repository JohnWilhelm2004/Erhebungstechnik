
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
  labs(
       x = "Korrelationsstärke (r)\n ← Spart Zeit/ Wenig Verständnis | Kostet Zeit / Hohes Verständnis →",
       y = NULL,
       color = "Gemessener Effekt"
  ) +
  #Wir editieren unser Theme minimal etwas damit es unseren Anforderungen entspricht
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top", #Sorgt dafür das die Legende oben ist 
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16), #Überschrift Makieren und Richtige Größe einstellen 
    axis.text.y = element_text(color = "black", size = 11) #Achsenbeschriftung einfärben und Größe Richtig bestimmen
  )

#Frage/Plot 3 - Welche Materialien Korrelieren Tatsächlich mit dem Gefühl des Veständnis

plot.3.data <- survey.data %>%
  
  #Wir brauchen das verständnis und alle Nutzungs-Spalten
  select(Qualitaet_Verstehen_Num, starts_with("Nutzung")) %>%
  
  #Wir drehen wieder für ggplot die Daten damit alles untereinander steht 
  pivot_longer(
    cols = starts_with("Nutzung"),
    names_to = "Tool",
    values_to = "Haeufigkeit"
  ) %>%
  
  #Wir räumen wieder die Namen auf 
  mutate(Tool = str_remove_all(Tool, "Nutzung_|_Num")) %>%
  
  #Wir filtern alle die das Tool regelmäßig Nutzen
  filter(Haeufigkeit >= 3) %>%
  
  #Wir rechnen absofort pro Tool
  group_by(Tool) %>%
  
  #Jetzt berechnen wir die Werte die wir für den Plot eigentlich brauchen
  summarise(
    Note = mean(Qualitaet_Verstehen_Num, na.rm = TRUE), #Berechnung des Durchschnitts
    Streuung = sd(Qualitaet_Verstehen_Num, na.rm = TRUE), #Fehlerbalken
    Anzahl.User = n() #Wir zeigen an wie viele Nutzer es gab so kann man die aussagekräftigkeit besser einschätzen
  ) %>%
  
  #Sortieren die Notes so das die beste vorne steht 
  mutate(Tool = fct_reorder(Tool, Note, .desc = TRUE))


#Jetzt erstellen wir unseren Mean/Error Plot 
ggplot(plot.3.data, aes(x = Tool, y = Note, color = Tool)) +
  
  #Zuerst erstellen wir den Fehlerbalken
  geom_errorbar(aes(ymin = Note - Streuung, ymax = Note + Streuung),
                width = 0.2, size = 1) +
  #Der Punkt welcher unseren durchschnittswert darstellt 
  geom_point(size = 5) +
  
  #Text Label damit man sieht wie viele Leute zu einer Häufigen Lerngruppe gehören
  geom_text(aes(label = paste0("n=", Anzahl.User)),
            vjust = -2.5, size = 3.5, color = "grey70") +
  
  #Auswählen der Farbpalette
  scale_colour_viridis_d(option = "mako", begin = 0.3, end = 0.9) +
  
  #Jezt fixieren wir die Achsen 
  scale_y_continuous(limits = c(1, 5.5), breaks = 1:5) +
  
  #Beschriftung(WIP)
  labs(
    y = "Verständnis (Subjektiv)",
    x = NULL 
  ) +
  theme_minimal(base_size = 12)


#Frage/Plot 4 - Macht es einen unterschied für die Note, ob ich ein Tool viel oder wenig nutze?

#Wir erstellen hierfür einen Density Plot, ursprünglich wollte ich nen einfachen 
#Boxplot verwenden aber das war mir ein wenig zu langweilig

#Wir starten wie immer mit unserer datenumformung
plot.4.data <- survey.data %>%
  
  #Wir wählen die Zeilen aus die wir brauchen das Verständnis und Nutzung 
  #von KI und Youtube im Vergleich mit Skript und Büchern
  select(Qualitaet_Verstehen_Num,
         Nutzung_Skript_Num,
         Nutzung_KI_Num,
         Nutzung_YouTube_Num,
         Nutzung_Buecher_Num) %>%
  
  #Wir drehen die Daten wieder für den ggplot
  pivot_longer(cols = starts_with("Nutzung"),
               names_to = "Tool",
               values_to = "Haeufigkeit") %>%
  
  #Wir machen unseren Datensatz wieder für den Plot "sauber"
  mutate(Tool = str_remove_all(Tool, "Nutzung_|_Num")) %>%
  
  #Wir entfernen alle NAs
  drop_na(Haeufigkeit) %>%
  
  #Jetzt unterscheiden wir die Studenten in 2 Gruppen
  mutate(Gruppe = if_else(Haeufigkeit >= 4,
                          "Viel Nutzer",
                          "Wenig Nutzer")) %>%
  
  #Wir filtern Tool nur nach den Tools die wir tatsächlich haben wollen in unserem Violinen Plot
  filter(Tool %in% c("Skript", "KI", "YouTube", "Buecher"))

#Jetzt erstellen wir unseren density Plot 
ggplot(plot.4.data, aes(x = Qualitaet_Verstehen_Num, fill = Gruppe, color = Gruppe)) +
  
  #Wir erstellen unseren density Plot mit alpha = 0.4 damit die Hügel transperent sind 
  geom_density(alpha = 0.4) +
  
  #Dieser Befehl sorgt dafür das wir sozusagen mini Plots für jedes der Materialien erstellen 
  facet_wrap(~Tool) +
  
  #Hier nehmen wir unsere Standard Farbpalette 
  scale_fill_viridis_d(option = "mako", begin = 0.4, end = 0.8) +
  scale_color_viridis_d(option = "mako", begin = 0.4, end = 0.8) +
  
  #Wir erstellen Überschrift und Achsenbeschriftungen für die Verständnis
  labs(
       x = "Verständnis (Von 1 bis 5)",
       y = "Dichte",
       fill = "Gruppe"
  ) +
  
  #Wir editieren unser Theme minimal etwas damit es unseren Anforderungen entspricht
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top", #Sorgt dafür das die Legende oben ist 
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16), #Überschrift Makieren und Richtige Größe einstellen 
    axis.text.y = element_text(color = "black", size = 11) #Achsenbeschriftung einfärben und Größe Richtig bestimmen
  )

#Frage4.5/Plot4.5 - Mit welchen Effekten Korrelieren die Arbeitsmaterialien
#Es fehlt zwischen PLot 4 und 5 für die Argumentation unserer These der Übergang deswegen
#gucken wir uns jetzt an wie die Materialien mit den Effekten zusammenhängen die für die Zufriedenheit sorgen können

plot.4.5.data <- survey.data %>%
  #Wir wählen wieder unsere gebrauchten Eigenschaften aus, 
  #wir nehmen nicht alle Effekte weil unsere Plots sonst zu unübersichtlich werden
  select(starts_with("Nutzung_"),
         Effekt_Sicherheit_Num,
         Effekt_Stress_Num,
         Effekt_Zeitaufwand_Rev,
         Qualitaet_Verstehen_Num) %>%
  
  #Wir entfernen alle NAs 
  drop_na() %>%
  
  #Wir berechnen die Korrelation zwischen all diesen Werten
  cor() %>%
  
  #Hier wieder unser trick aus Plot 4 um die Daten ins richtige Format zu rücken
  as.table() %>%
  as.data.frame() %>%
  
  #Jetzt filtern wir die Korrelationen die wir tatsächlich haben wollen
  filter(str_detect(Var1, "Nutzung"), !str_detect(Var2, "Nutzung")) %>%
  
  #Wir machen unsere Namen für die Plots wieder schön 
  mutate(
    Tool = str_remove_all(Var1, "Nuztung_|_Num"),
    
    #Wir entfernen auch noch andere Überbleibsel 
    Effekt = str_remove_all(Var2, "Effekt_|Qualitaet_|Nutzung_|_Num|_Rev")
  )

ggplot(plot.4.5.data, aes (x = Freq, y = reorder(Tool, Freq), fill = Tool)) +
  #Hiermit erstellen wir unsere Balken
  geom_col() +
  
  #Das hier sorgt dafür das wir wieder viele
  facet_wrap(~Effekt, scales = "free_x") +
  scale_fill_viridis_d(option = "mako", begin = 0.4, end = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
  labs(
    x = "Stärke des Zusammenhangs (GGrößer ist besser)",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11)
  )

#Plot/Frage 5 - Wie Korrelieren Qualitäts und Effekteigenschaften mit der Zufriedenheit
plot.5.data.cor <- survey.data %>%
  
  #Wir suchen alle Eigenschaften raus die mit der Zufriedenheit interessant korrelieren könnten
  select(starts_with("Qualitaet_"), starts_with("Effekt_"), Zufriedenheit_Score) %>%
  
  #Wir entfernen alle Fehlenden Werte für den Plot 
  drop_na() %>%
  
  #Wir berechnen die Korrelation zwischen allen werten
  cor() %>%
  
  #Formen die Matrix zuerst in eine Tabelle und dann in einen Data Frame um 
  as.table() %>%
  as.data.frame() %>%
  
  #Wir filtern alle Korrelationen heraus so dass nurnoch die wichtigen Dinge übrig sind 
  filter(Var1 != "Zufriedenheit_Score", Var2 == "Zufriedenheit_Score") %>%
  
  mutate(
    Faktor = str_remove_all(Var1, "Qualitaet_|Effekt_|_Num|_Rev"),
  ) %>%
  
  mutate(Faktor = fct_reorder(Faktor, Freq)) 

#Da wir in der ersten Transformation die Daten schon umgebaut haben mit correlationen
#holen wir uns jetzt noch die anderen Sachen die wir brauchen für den finalen Tornado Plot 

plot.5.data.cor.2 <- survey.data %>%
  #Wir wählen wieder alle Spalten aus die wir brauchen 
  select(starts_with("Nutzung_"),
         Effekt_Sicherheit_Num,
         Effekt_Stress_Num,
         Effekt_Zeitaufwand_Rev,
         Qualitaet_Verstehen_Num) %>%
  #Wir entfernen alle na werte damit wir später keine Probleme bekommen
  drop_na() %>%
  
  #Wir berechnen von diesen Aspekten die Korrelationen
  cor() %>%
  
  #Wir wenden wieder unseren Umformatierungstrick an
  as.table() %>%
  as.data.frame() %>%
  filter(
    str_detect(Var1, "Nutzung"),
    !str_detect(Var2, "Nutzung")
  ) %>%
  mutate(
    Tool = str_remove_all(Var1, "Nutzung_|_Num"),
    Effekt = str_remove_all(Var2, "Effekt_|Qualitaet_|_Num|_Rev")
  )

ggplot(plot.5.data.cor.2, aes(x = Freq, y = reorder(Effekt, Freq), fill = Effekt)) +
  geom_col() +
  facet_wrap(~Tool, ncol = 3) +
  scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
  labs(
    x = "Korrelation (r) (Höher ist hier immer besser)",
    y = NULL
  ) +
  theme_minimal(base_size = 12)

