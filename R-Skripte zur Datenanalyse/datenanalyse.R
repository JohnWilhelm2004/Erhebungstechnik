# ==============================================================================
# Aufgabe 4: Analyse des LimeSurvey-Datensatzes
# ==============================================================================

# 1. Bibliotheken laden
library(tidyverse)

# 2. Daten einlesen
# source("funktionen.R") # Lädt automatisch auch die Helfer

survey.data <- read.csv("results-survey_cleaned.csv")


# ==============================================================================
# PLOT 4
# ==============================================================================

plot.4.data <- survey.data %>%
  
  #Wir wählen die Zeilen aus die wir brauchen das Verständnis und Nutzung 
  #von KI und Youtube im Vergleich mit Skript und Büchern
  select(starts_with("Nutzung_"),
         Qualitaet_Verstehen_Num) %>%
  
  #Wir drehen die Daten wieder für den ggplot
  pivot_longer(cols = starts_with("Nutzung"),
               names_to = "Tool",
               values_to = "Haeufigkeit") %>%
  
  #Wir machen unseren Datensatz wieder für den Plot "sauber"
  mutate(Tool = str_remove_all(Tool, "Nutzung_|_Num")) %>%
  
  #Wir filtern alle störenden NAs raus 
  filter(!is.na(Haeufigkeit), !is.na(Qualitaet_Verstehen_Num)) %>%
  
  #Jetzt unterscheiden wir die Studenten in 2 Gruppen
  mutate(Gruppe = case_when(
    Haeufigkeit >= 4 ~ "häufige Nutzung",          # 4 und 5
    TRUE ~ "(unter-)durchschnittliche Nutzung"     # kleiner gleich 3
  ))
  
  #Wir filtern Tool nur nach den Tools die wir tatsächlich haben wollen in unserem Density Plot
  # filter(Tool %in% c("Skript", "KI", "YouTube", "Buecher"))

#Jetzt erstellen wir unseren density Plot 
plot4 <- ggplot(plot.4.data, aes(x = Qualitaet_Verstehen_Num, fill = Gruppe, color = Gruppe)) +
  
  #Wir erstellen unseren density Plot mit alpha = 0.4 damit die Hügel transparent sind 
  geom_density(alpha = 0.4) +
  
  #Dieser Befehl sorgt dafür das wir sozusagen Mini Plots für jedes der Materialien erstellen 
  facet_wrap(~Tool) +
  
  #Hier nehmen wir unsere Standard Farbpalette 
  scale_fill_viridis_d(option = "mako", begin = 0.4, end = 0.8) +
  scale_color_viridis_d(option = "mako", begin = 0.4, end = 0.8) +
  
  #Wir erstellen Überschrift und Achsenbeschriftungen für die Verständnis
  labs(
    title = "Verständnis nach Nutzungsintensität",
    subtitle = "Vergleich: (Unter-)Durchscnittlich (1-3) vs. Viel (4-5) Nutzung",
    x = "Verständnis (Von 1 bis 5)",
    y = "Dichte",
    fill = "Gruppe",
    color = "Gruppe"
  ) +
  
  #Wir editieren unser Theme minimal etwas damit es unseren Anforderungen entspricht
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top", #Sorgt dafür das die Legende oben ist 
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16), #Überschrift Makieren und Richtige Größe einstellen 
    axis.text.y = element_text(color = "black", size = 11) #Achsenbeschriftung einfärben und Größe Richtig bestimmen
  )


#Wir speichern wieder unsere Grafik 
ggsave("Plot4.Verständnisdichte.pdf",
       plot = plot4,
       device = "pdf",
       width = 7,
       height = 4)


# ==============================================================================
# PLOT 4.5
# ==============================================================================

plot.4.5.data <- survey.data %>%
  #Wir wählen wieder unsere gebrauchten Eigenschaften aus, 
  #wir nehmen nicht alle Effekte weil unsere Plots sonst zu unübersichtlich werden
  select(starts_with("Nutzung_"),
         starts_with("Effekt_"),
         Qualitaet_Verstehen_Num) %>%
  
  
  select(-Effekt_Zeitaufwand_Rev) %>%
  
  #Wir entfernen alle NAs 
  # drop_na() %>%
  
  #Wir berechnen die Korrelation zwischen all diesen Werten
  cor(use = "pairwise.complete.obs") %>%
  
  #Hier wieder unser trick aus Plot 4 um die Daten ins richtige Format zu rücken
  as.table() %>%
  as.data.frame() %>%
  
  #Jetzt filtern wir die Korrelationen die wir tatsächlich haben wollen
  filter(str_detect(Var1, "Nutzung"), !str_detect(Var2, "Nutzung")) %>%
  
  #Wir machen unsere Namen für die Plots wieder schön 
  mutate(
    Tool = str_remove_all(Var1, "Nutzung_|_Num"),
    
    #Wir entfernen auch noch andere Überbleibsel 
    Effekt = str_remove_all(Var2, "Effekt_|Qualitaet_|Nutzung_|_Num|_Rev")
  )

plot4.5 <- ggplot(plot.4.5.data, aes (x = Freq, y = reorder(Tool, Freq), fill = Tool)) +
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

#Wir speichern wieder unser Bild
ggsave("Plot4.5.Korrelation.pdf",
       plot = plot4.5,
       device = "pdf",
       width = 10,
       height = 4.5)


# ==============================================================================
# PLOT 6
# ==============================================================================


# 1. Datenvorbereitung: WIR NEHMEN ALLES
plot.6.data <- survey.data %>%
  # Wähle automatisch JEDE Spalte, die Zahlen enthält
  select(where(is.numeric)) %>%
  select(-Fachsemester) # VERÄNDERUNG
  
  # Optional: Falls du eine ID-Spalte oder einen Index ("X") hast, weg damit:
  # select(-contains("ID"), -any_of("X")) %>% 
  
  # drop_na()

# WICHTIG: Wir speichern die Original-Reihenfolge der Spalten
# (Damit R sie gleich nicht alphabetisch sortiert)
original_order <- names(plot.6.data)

# 2. Korrelation berechnen
plot.6.cor <- plot.6.data %>%
  cor(use = "pairwise.complete.obs") %>%
  as.table() %>%
  as.data.frame()

# 3. Die Reihenfolge erzwingen
# Wir sagen dem Plot: "Benutze genau die Liste 'original_order' zum Sortieren"
plot.6.cor <- plot.6.cor %>%
  mutate(
    Var1 = factor(Var1, levels = original_order),
    # Für die y-Achse drehen wir die Reihenfolge oft um (rev), 
    # damit die Diagonale wie gewohnt von links oben nach rechts unten läuft
    # oder wir lassen es gleich, je nach Geschmack. Hier: Gleich wie CSV.
    Var2 = factor(Var2, levels = rev(original_order)) 
  )

# 4. Der große Heatmap-Plot
ggplot(plot.6.cor, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile(color = "white", lwd = 0.2) + # Feine weiße Linien
  
  # Farben: Volles Spektrum von -1 (Rot/Blau) bis +1 (Hell/Gelb)
  scale_fill_viridis_c(option = "mako", direction = 1, limits = c(-1, 1)) +
  
  # <--- HIER: Zahlen hinzufügen
  geom_text(aes(label = round(Freq, 2)), color = "white", size = 2.5) +
  
  # Achsenbeschriftung säubern (wir entfernen _Num etc. für Lesbarkeit)
  scale_x_discrete(labels = function(x) str_remove_all(x, "Nutzung_|Qualitaet_|Effekt_|_Num|_Score|_Rev")) +
  scale_y_discrete(labels = function(x) str_remove_all(x, "Nutzung_|Qualitaet_|Effekt_|_Num|_Score|_Rev")) +
  
  labs(
    title = "Gesamt-Matrix: Alle Korrelationen",
    subtitle = "Übersicht aller gemessenen Variablen im Kurs",
    x = NULL,
    y = NULL,
    fill = "Korrelation"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    # Text auf x-Achse senkrecht stellen, sonst überlappt alles bei vielen Variablen
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  coord_fixed() # Quadratische Kacheln





plot(table(survey.data$Qualitaet_Verstehen_Num[survey.data$Nutzung_YouTube_Num >= 2]))

survey.data[ ,c(12:18)][survey.data$Abschluss == "Master"]
# Schema: data[ ZEILEN , SPALTEN ]
(survey.data[survey.data$Abschluss == "Master", c(12:18)])




library(tidyverse)

# 1. Wir definieren die Spalten, die uns interessieren
usage_cols <- survey.data %>%
  select(starts_with("Nutzung_") & ends_with("_Num")) %>%
  names()

# 2. Wir berechnen für jedes Material die beiden Korrelationen
# map_dfr iteriert über die Liste und baut direkt einen Dataframe zusammen
efficiency_table <- usage_cols %>%
  map_dfr(function(col_name) {
    
    # Korrelation: Nutzung vs. Zeitaufwand
    r_time <- cor(survey.data[[col_name]], 
                  survey.data$Effekt_Zeitaufwand_Num, 
                  use = "complete.obs") # Wichtig: NAs ignorieren
    
    # Korrelation: Nutzung vs. Verständnis
    r_verst <- cor(survey.data[[col_name]], 
                   survey.data$Qualitaet_Verstehen_Num, 
                   use = "complete.obs")
    
    # Ergebniszeile zurückgeben
    tibble(
      Material = str_remove_all(col_name, "Nutzung_|_Num"), # Namen säubern
      Cor_Zeit = r_time,
      Cor_Verstaendnis = r_verst,
      # Hier berechnen wir direkt deinen "Effizienz-Abstand"
      Effizienz_Delta = r_verst - r_time 
    )
  }) %>%
  # Optional: Nach Effizienz sortieren
  arrange(desc(Effizienz_Delta))

# 3. Ergebnis anzeigen
print(efficiency_table)





# Test (i): Wie hoch ist die Zufriedenheit im Schnitt?
print(calculate_metric_stats(daten, "Zufriedenheit_Score"))

# Test (iv): Sind Informatiker zufriedener als Statistiker?
print(calculate_metric_by_group(daten, "Zufriedenheit_Score", "Fakultaet"), n=21)

# Test (v): Grafik erstellen
# Zusammenhang: Digital Score nach Fakultaet
grafik <- plot_categorical_multivar(daten, "Fakultaet", "Abschluss")
print(grafik)

# Zusammenhang: Zufriedenheit nach Fakultaet
grafik <- plot_categorical_multivar(daten, "Fakultaet", "Zufriedenheit_Score", "Abschluss")
print(grafik)


# Zusammenfassung seltener Studiengänge zu Soonstiges-Kategorie
simplified <- data %>%
  mutate(
    # Fasse alles mit weniger als 5 Nennungen zu "Sonstiges" zusammen
    Fakultaet_Gruppiert = fct_lump_min(Fakultaet, min = 5, other_level = "Sonstiges")
  )

# Kontrolle: Wie sieht die Verteilung jetzt aus?
counts = table(simplified$Fakultaet_Gruppiert)

pie(counts, main = "Verteilung der Fakultäten")

pie(table(data$Abschluss), main = "Verteilung Bachelor/Master")

barplot(table(data$Fachsemester))

simplified <- data %>%
  mutate(
    # Fasse alles mit weniger als 5 Nennungen zu "Sonstiges" zusammen
    Fakultaet_Gruppiert = fct_lump_min(Fakultaet, min = 5, other_level = "Sonstiges")
  )

barplot(table(data$Fachsemester))

#####
# Bibliotheken laden
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(corrplot)) install.packages("corrplot")
if(!require(fastDummies)) install.packages("fastDummies")
library(tidyverse)
library(corrplot)
library(fastDummies)

# 1. Daten laden
df <- read.csv("results-survey_cleaned.csv")

# 2. Daten vorbereiten
# Wir wandeln alles in Zahlen um. 
# WICHTIG: Wir entfernen Spalten, die nur einen einzigen Wert haben (Varianz = 0),
# da diese zu Fehlern in der Korrelation führen.
df_numeric <- df %>%
  dummy_cols(select_columns = c("Fakultaet", "Abschluss"), 
             remove_first_dummy = FALSE, 
             remove_selected_columns = TRUE) %>%
  select_if(is.numeric) %>%
  select(where(~ var(., na.rm = TRUE) > 0)) # Entfernt Spalten ohne Variation (Konstante)

# 3. Korrelationsmatrix berechnen
cor_matrix <- cor(df_numeric, use = "pairwise.complete.obs")

# 4. FEHLERBEHEBUNG: NAs durch 0 ersetzen
# Das ist der entscheidende Schritt, der Ihren Fehler verhindert.
# Wenn eine Korrelation nicht berechnet werden konnte, setzen wir sie auf 0 (kein Zusammenhang).
cor_matrix[is.na(cor_matrix)] <- 0

# 5. Filtern: Nur relevante Variablen behalten
threshold <- 0.4
# Diagonale temporär auf 0 setzen, damit sie nicht als "starke Korrelation" zählt
diag_backup <- diag(cor_matrix)
diag(cor_matrix) <- 0

# Finde Zeilen, die mindestens eine Korrelation > 0.4 haben
# na.rm = TRUE sorgt dafür, dass evtl. verbliebene Fehler ignoriert werden
relevant_indices <- apply(abs(cor_matrix), 1, max, na.rm = TRUE) > threshold

# Matrix reduzieren
cor_matrix_filtered <- cor_matrix[relevant_indices, relevant_indices]

# Diagonale wiederherstellen (auf 1 setzen)
diag(cor_matrix_filtered) <- 1

# 6. Plotten
# Falls nach dem Filtern nichts übrig bleibt, geben wir eine Warnung aus
if(nrow(cor_matrix_filtered) > 1) {
  corrplot(cor_matrix_filtered, 
           method = "color", 
           type = "upper", 
           order = "hclust", 
           addCoef.col = "black", 
           tl.col = "black", 
           tl.cex = 0.6, 
           number.cex = 0.5, 
           title = "Relevante Korrelationen (> 0.4)", 
           mar = c(0,0,2,0))
} else {
  print("Keine starken Korrelationen über dem Schwellenwert gefunden.")
}

