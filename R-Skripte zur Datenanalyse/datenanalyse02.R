# ==============================================================================
# Aufgabe 4: Analyse & Berichtserstellung
# ==============================================================================

# 1. Setup & Laden
library(tidyverse)
library(corrplot) # Für die schöne Korrelations-Grafik (ggf. install.packages("corrplot"))
library(xtable)   # Optional: Für LaTeX-Tabellen-Export

source("helper_funktionen.R")
source("funktionen.R")

# Daten einlesen
daten <- read_csv("results-survey_cleaned.csv")

# Ausgabe-Ordner für Grafiken erstellen
if(!dir.exists("plots")) dir.create("plots")

# ==============================================================================
# TEIL A: Deskriptive Statistik (Anwendung der Funktionen i & ii)
# ==============================================================================

print("--- Analyse: Zufriedenheit (Metrisch) ---")
stats_zufriedenheit <- calculate_metric_stats(daten, "Zufriedenheit_Score")
print(stats_zufriedenheit)
# Tipp: Für LaTeX Bericht exportieren:
# print(xtable(stats_zufriedenheit), type = "latex")

print("--- Analyse: Wer hat teilgenommen? (Kategorial) ---")
stats_fakultaet <- calculate_cat_stats(daten, "Fakultaet")
print(stats_fakultaet)

# ==============================================================================
# TEIL B: Bivariate Analyse (Anwendung der Funktionen iii & iv)
# ==============================================================================

print("--- Analyse: Zusammenhang Fakultät & Abschluss ---")
table_fak_abschluss <- calculate_bivariate_cat(daten, "Fakultaet", "Abschluss")
print(table_fak_abschluss)

print("--- Analyse: Zufriedenheit nach Fakultät ---")
# Sind Physiker zufriedener als Informatiker?
stats_zufriedenheit_fak <- calculate_metric_by_group(daten, "Zufriedenheit_Score", "Fakultaet")
print(stats_zufriedenheit_fak)

# ==============================================================================
# TEIL C: Die Korrelationsanalyse (Dein Spezialwunsch)
# ==============================================================================

# 1. Wir wählen nur die numerischen Spalten aus (Nutzung, Qualität, Effekte)
#    und die Zielvariable Zufriedenheit
cor_data <- daten %>%
  select(Zufriedenheit_Score, starts_with("Nutzung"), starts_with("Effekt"), starts_with("Qualitaet")) %>%
  select(where(is.numeric)) # Zur Sicherheit nur Zahlen

# 2. Korrelation berechnen (Spearman ist besser für Likert-Skalen)
cor_matrix <- cor(cor_data, use = "pairwise.complete.obs", method = "spearman")

# 3. Nur die Spalte "Zufriedenheit" anzeigen (sortiert)
cor_target <- sort(cor_matrix[,"Zufriedenheit_Score"], decreasing = TRUE)
print("--- Top Korrelationen mit Zufriedenheit ---")
print(head(cor_target, 10)) # Top 10 positive

# 4. Grafik: Korrelations-Plot speichern
png("plots/korrelation_heatmap.png", width = 800, height = 800)
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.cex = 0.7, # Textgröße anpassen
         title = "Korrelationsmatrix: Was beeinflusst den Erfolg?", mar=c(0,0,1,0))
dev.off()
print("Grafik gespeichert: plots/korrelation_heatmap.png")


# ==============================================================================
# TEIL D: Visualisierung (Anwendung Funktion v)
# ==============================================================================

# Grafik 1: Zufriedenheit nach Fakultät und Abschluss (Boxplot)
plot1 <- plot_categorical_multivar(daten, "Fakultaet", "Zufriedenheit_Score", "Abschluss")
ggsave("plots/boxplot_zufriedenheit.png", plot1, width = 8, height = 6)

# Grafik 2: KI-Nutzung nach Fakultät
plot2 <- plot_categorical_multivar(daten, "Fakultaet", "Nutzung_KI_Num", "Abschluss")
ggsave("plots/boxplot_ki_nutzung.png", plot2, width = 8, height = 6)

print("Analyse abgeschlossen. Alle Grafiken liegen im Ordner 'plots'.")
