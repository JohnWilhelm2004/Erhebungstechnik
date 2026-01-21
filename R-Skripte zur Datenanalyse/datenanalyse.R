# ==============================================================================
# Aufgabe 4: Analyse des LimeSurvey-Datensatzes
# ==============================================================================

# 1. Bibliotheken laden
library(tidyverse)

# 2. Daten einlesen
# setwd("C:/Users/Annika/Documents/GitHub/Erhebungstechnik/R-Skripte zur Datenanalyse")
source("funktionen.R") # Lädt automatisch auch die Helfer

# setwd("C:/Users/Annika/Documents/ErhebTech")
data <- read.csv("results-survey_cleaned.csv")

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

