# ==============================================================================
# Aufgabe 2: Helfer-Funktionen (Interne Helfer für Fehlerprüfung und Formatierung)
# ==============================================================================

# Helfer 1: Prüft, ob eine Variable im Datensatz existiert
# Wirft einen Fehler, wenn der Spaltenname falsch geschrieben ist.
check_variable_exists <- function(data, var_name) {
  if (!var_name %in% names(data)) {
    stop(paste("Fehler: Die Variable '", var_name, "' existiert nicht im Datensatz."))
  }
}

# Helfer 2: Prüft, ob eine Variable numerisch ist
# Wichtig für Mittelwerte etc.
check_is_numeric <- function(data, var_name) {
  if (!is.numeric(data[[var_name]])) {
    stop(paste("Fehler: Die Variable '", var_name, "' muss numerisch sein."))
  }
}

# Helfer 3: Prüft, ob eine Variable ein Factor (kategorial) ist
check_is_factor <- function(data, var_name) {
  if (!is.factor(data[[var_name]]) && !is.character(data[[var_name]])) {
    warning(paste("Hinweis: Die Variable '", var_name, "' ist kein Factor. Sie wird temporär umgewandelt."))
  }
}

# Helfer 4: Gibt einen schön formatierten String für Tabellen zurück
# Macht aus 0.12345 -> "0.12"
format_number <- function(x, digits = 2) {
  format(round(x, digits), nsmall = digits)
}
