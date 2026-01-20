# ==============================================================================
# Aufgabe 3: Funktionen zur Analyse des LimeSurvey-Datensatzes
# ==============================================================================

library(ggplot2)
library(dplyr)
library(rlang) # Für das sichere Übergeben von Variablennamen

# Laden der Helfer-Funktionen
source("helper_funktionen.R")

# ------------------------------------------------------------------------------
# (i) Deskriptive Statistik für metrische Variablen
#     Beispiel: Mittelwert & Median für "Zufriedenheit_Score"
# ------------------------------------------------------------------------------
calculate_metric_stats <- function(data, var_name) {
  # 1. Checks
  check_variable_exists(data, var_name)
  check_is_numeric(data, var_name)
  
  # 2. Berechnung
  stats <- data %>%
    summarise(
      Variable = var_name,
      N = sum(!is.na(.data[[var_name]])),
      Mean = mean(.data[[var_name]], na.rm = TRUE),
      Median = median(.data[[var_name]], na.rm = TRUE),
      SD = sd(.data[[var_name]], na.rm = TRUE),
      Min = min(.data[[var_name]], na.rm = TRUE),
      Max = max(.data[[var_name]], na.rm = TRUE)
    ) %>%
    # Schön formatieren mit der Helfer-Funktion
    mutate(across(where(is.numeric), ~format_number(., 2)))
  
  return(stats)
}

# ------------------------------------------------------------------------------
# (ii) Deskriptive Statistik für kategoriale Variablen
#      Beispiel: Wie viel Prozent studieren Informatik?
# ------------------------------------------------------------------------------
calculate_cat_stats <- function(data, var_name) {
  
  check_variable_exists(data, var_name)
  check_is_factor(data, var_name)
  
  stats <- data %>%
    group_by(.data[[var_name]]) %>%
    summarise(Anzahl = n()) %>%
    mutate(
      Prozent = Anzahl / sum(Anzahl) * 100,
      Prozent_Formatiert = paste0(format_number(Prozent, 1), "%")
    ) %>%
    arrange(desc(Anzahl)) # Häufigste zuerst
  
  return(stats)
}

# ------------------------------------------------------------------------------
# (iii) Bivariate Statistik: Zwei kategoriale Variablen
#       Beispiel: Fakultät vs. Abschluss (Kreuztabelle)
# ------------------------------------------------------------------------------
calculate_bivariate_cat <- function(data, var1, var2) {
  check_variable_exists(data, var1)
  check_variable_exists(data, var2)
  
  # Kreuztabelle mit Anteilen
  table <- data %>%
    count(.data[[var1]], .data[[var2]]) %>%
    group_by(.data[[var1]]) %>%
    mutate(Prozent_in_Gruppe_1 = n / sum(n) * 100) %>%
    ungroup() %>%
    mutate(Prozent_Gesamt = n / sum(n) * 100)
  
  return(table)
}

# ------------------------------------------------------------------------------
# (iv) Bivariate Statistik: Metrisch vs. Dichotom/Kategorial
#      Beispiel: Zufriedenheit (Metrisch) nach Fakultät (Kategorial)
# ------------------------------------------------------------------------------
calculate_metric_by_group <- function(data, metric_var, group_var) {
  check_variable_exists(data, metric_var)
  check_variable_exists(data, group_var)
  
  stats <- data %>%
    group_by(.data[[group_var]]) %>%
    summarise(
      N = n(),
      Mean = mean(.data[[metric_var]], na.rm = TRUE),
      SD = sd(.data[[metric_var]], na.rm = TRUE),
      Median = median(.data[[metric_var]], na.rm = TRUE)
    ) %>%
    mutate(across(where(is.numeric), ~format_number(., 2)))
  
  return(stats)
}

# ------------------------------------------------------------------------------
# (v) Visualisierung: 3-4 Variablen
#     Beispiel: Zufriedenheit (Y) nach Fakultät (X), eingefärbt nach Abschluss
# ------------------------------------------------------------------------------
plot_categorical_multivar <- function(data, cat_var_x, metric_var_y, cat_var_fill) {
  # Checks
  check_variable_exists(data, cat_var_x)
  check_variable_exists(data, metric_var_y)
  check_variable_exists(data, cat_var_fill)
  
  # Plot
  p <- ggplot(data, aes(x = .data[[cat_var_x]], y = .data[[metric_var_y]], fill = .data[[cat_var_fill]])) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) + # Boxplot für Verteilung
    geom_jitter(position = position_jitterdodge(), alpha = 0.4) + # Punkte für echte Daten
    labs(
      title = paste("Zusammenhang:", metric_var_y, "nach", cat_var_x),
      subtitle = paste("Gruppiert nach:", cat_var_fill),
      x = cat_var_x,
      y = metric_var_y,
      fill = cat_var_fill
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Labels drehen falls lang
  
  return(p)
}

