# ==============================================================================
# Aufgabe 4: Analyse des LimeSurvey-Datensatzes
# ==============================================================================

# 1. Bibliotheken laden
library(tidyverse)

# 2. Daten einlesen
source("funktionen.R") # Lädt automatisch auch die Helfer
daten <- read.csv("results-survey_cleaned.csv")

# Test (i): Wie hoch ist die Zufriedenheit im Schnitt?
print(calculate_metric_stats(daten, "Zufriedenheit_Score"))

# Test (iv): Sind Informatiker zufriedener als Statistiker?
print(calculate_metric_by_group(daten, "Zufriedenheit_Score", "Fakultaet"), n=21)

# Test (v): Grafik erstellen
# Zusammenhang: Digital Score nach Fakultaet
grafik <- plot_categorical_multivar(daten, "Fakultaet", "Digital_Score", "Abschluss")
print(grafik)

# Zusammenhang: Zufriedenheit nach Fakultaet
grafik <- plot_categorical_multivar(daten, "Fakultaet", "Zufriedenheit_Score", "Abschluss")
print(grafik)


# Zusammenfassung seltener Studiengänge zu Soonstiges-Kategorie
simplified <- data %>%
  mutate(
    # Fasse alles mit weniger als 5 Nennungen zu "Sonstiges" zusammen
    Fakultaet_Gruppiert = fct_lump_min(Fakultaet, min = 4, other_level = "Sonstiges")
  )

# Kontrolle: Wie sieht die Verteilung jetzt aus?
counts = table(simplified$Fakultaet_Gruppiert)

pie(counts, main = "Verteilung der Fakultäten")

pie(table(data$Abschluss), main = "Verteilung Bachelor/Master")

#####
# ==============================================================================
# Grafik: Der Haupteffekt
# Zusammenhang zwischen Materialqualität und Zufriedenheit
# ==============================================================================

# Plot erstellen
plot_main <- ggplot(daten, aes(x = Qualitaet_Score, y = Zufriedenheit_Score)) +
  # 1. Die Punkte (mit leichtem "Jitter", damit sie nicht übereinander liegen)
  geom_jitter(aes(color = Fakultaet), width = 0.1, height = 0.1, alpha = 0.7, size = 3) +
  
  # 2. Die Regressionsgerade (Lineares Modell "lm")
  geom_smooth(method = "lm", color = "darkblue", fill = "lightblue") +
  
  # 3. Beschriftung und Design
  labs(
    title = "Einfluss der Lernmaterialien auf die Zufriedenheit",
    subtitle = paste("Korrelation: r =", round(cor(processed_data$Qualitaet_Score, data$Zufriedenheit_Score, use="complete.obs"), 2)),
    x = "Materialqualität",
    y = "Zufriedenheit mit Lernerfolg",
    color = "Fakultät"
  ) +
  theme_minimal() +
  scale_color_viridis_d() # Schöne Farben für Publikationen

# Anzeigen
print(plot_main)

# Speichern für den Bericht
ggsave("plots/scatter_material_zufriedenheit.png", plot_main, width = 8, height = 6)

