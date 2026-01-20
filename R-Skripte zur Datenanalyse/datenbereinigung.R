# ==============================================================================
# Aufgabe 1: Vorverarbeitung des LimeSurvey-Datensatzes
# ==============================================================================

# 1. Bibliotheken laden
# install.packages("tidyverse")
library(tidyverse)

# 2. Daten einlesen
# Wir lesen die CSV ein.
# setwd("C:/Users/Annika/Documents/ErhebTech")
raw_data <- read_csv("results-survey478754.csv", show_col_types = FALSE)

# ==============================================================================
# SCHRITT A: Datenbereinigung & Filterung 
# ==============================================================================

# Wir behalten nur Teilnehmer, die bis zur letzten Seite (Seite 6) gekommen sind.
# Abbrecher verfälschen die Statistik.
clean_data <- raw_data %>%
  filter(`Letzte Seite` == 6) %>%
  # Entfernen von technischen Spalten, die für die Analyse irrelevant sind
  select(-`Antwort ID`, -`Datum Abgeschickt`, -`Start-Sprache`, -`Zufallsstartwert`,
         -`Welchen Abschluss erhalten Sie in Ihrem aktuellen Studiengang? [Sonstiges]`,
         -`Letzte Seite`)

# ==============================================================================
# SCHRITT B: Umbenennung der Variablen (Lesbarkeit)
# ==============================================================================
# Die Spaltennamen von LimeSurvey sind sehr lang. Wir kürzen sie für die Analyse ab.

clean_data <- clean_data %>%
  rename(
    # Demografie
    Zufriedenheit_Lernerfolg = `Wie zufrieden sind Sie mit ihrem aktuellen Lernerfolg?`,
    Fakultaet = `Welcher Fakultät gehört Ihr Studiengang an?`,
    Fakultaet_Sonst =`Welcher Fakultät gehört Ihr Studiengang an? [Sonstiges]`,
    Abschluss = `Welchen Abschluss erhalten Sie in Ihrem aktuellen Studiengang?`,
    Fachsemester = `Im wievielten Fachsemester studieren Sie?`,
    
    # --- 2. Einstellung zum Übungsbetrieb (Matrix) ---
    Einstellung_Uebung_Hilfreich  = `Wählen Sie bitte die passende Aussage aus. [Ich empfinde die wöchentlichen Übungsaufgaben als hilfreich.]`,
    Einstellung_Pflicht_Ball      = `Wählen Sie bitte die passende Aussage aus. [Wenn Übungsaufgaben verpflichtend sind, hilft es mir am Ball zu bleiben.]`,
    Einstellung_Mat_Ausreichend   = `Wählen Sie bitte die passende Aussage aus. [Normalerweise finde ich die einem Kurs gestellten Materialien ausreichend und hilfreich.]`,
    Einstellung_Aufschieben       = `Wählen Sie bitte die passende Aussage aus. [Wenn ein Kurs keine Verpflichtungen oder Fristen vorgibt, schiebe ich vieles auf.]`,
    Einstellung_Effizienz         = `Wählen Sie bitte die passende Aussage aus. [Effizientes Lernen in der Prüfungsvorbereitung ist mir wichtig]`,
    
    # --- 3. Nutzungshäufigkeit Lernressourcen (Matrix) ---
    Nutzung_Kurzskript      = `Wie häufig nutzen Sie (falls gegeben) die folgenden Arten von Lernmaterialien? [Kurzskript]`,
    Nutzung_Skript          = `Wie häufig nutzen Sie (falls gegeben) die folgenden Arten von Lernmaterialien? [Vollständiges Skript]`,
    Nutzung_Mitschriften    = `Wie häufig nutzen Sie (falls gegeben) die folgenden Arten von Lernmaterialien? [Mitschriften aus der Vorlesung]`,
    Nutzung_Video           = `Wie häufig nutzen Sie (falls gegeben) die folgenden Arten von Lernmaterialien? [Videoaufzeichnungen der Vorlesung]`,
    Nutzung_Musterloesung   = `Wie häufig nutzen Sie (falls gegeben) die folgenden Arten von Lernmaterialien? [Übungsaufgaben mit Musterlösung]`,
    Nutzung_Buecher         = `Wie häufig nutzen Sie (falls gegeben) die folgenden Arten von Lernmaterialien? [Bücher]`,
    Nutzung_Altklausuren    = `Wie häufig nutzen Sie (falls gegeben) die folgenden Arten von Lernmaterialien? [Altklausuren]`,
    Nutzung_YouTube         = `Wie häufig nutzen Sie (falls gegeben) die folgenden Arten von Lernmaterialien? [Online-Videos (z.B. YouTube)]`,
    Nutzung_KI              = `Wie häufig nutzen Sie (falls gegeben) die folgenden Arten von Lernmaterialien? [KI-Chatbots (z.B. Chat-GPT, Gemini)]`,
    
    # --- 4. Qualität & Verfügbarkeit (Matrix) ---
    Qualitaet_Verstehen     = `Die Materialien, die ich benutze... [... fördern Verstehen statt Auswendiglernen.]`,
    Qualitaet_Struktur      = `Die Materialien, die ich benutze... [... sind klar strukturiert und organisiert.]`,
    Qualitaet_Einfachheit   = `Die Materialien, die ich benutze... [... sind nicht unnötig kompliziert dargestellt.]`,
    Qualitaet_Rechtzeitig   = `Die Materialien, die ich benutze... [... stehen rechtzeitig zur Verfügung.]`,
    Effekt_Arbeitsbelastung = `Die Materialien, die ich benutze... [... erhöhen meine Arbeitsbelastung.]`,
    Qualitaet_Erleichterung = `Die Materialien, die ich benutze... [... erleichtern meinen Lernprozess.]`,
    
    # --- 5. Motivation, Stress & Wirkung (Matrix) ---
    Effekt_Sicherheit       = `Die Materialien, die ich benutze... [... geben mir Sicherheit, den Stoff zu schaffen.]`,
    Effekt_Motivation       = `Die Materialien, die ich benutze... [... steigern meine Motivation.]`,
    Effekt_Selbststaendig   = `Die Materialien, die ich benutze... [... unterstützen mein selbstständiges Lernen.]`,
    Effekt_Stress           = `Die Materialien, die ich benutze... [... reduzieren meinen Prüfungsstress.]`,
    Effekt_Zeitaufwand      = `Die Materialien, die ich benutze... [... erhöhen meinen Zeitaufwand.]`,
    Effekt_Relevanz         = `Die Materialien, die ich benutze... [... geben mir Sicherheit über Prüfungsrelevanz.]`,
    
    # --- 6. Sonstiges ---
    Feedback_Text           = `Feedback zur Umfrage`
  )

# ==============================================================================
# SCHRITT C: Datentypen anpassen & Kodierung
# ==============================================================================

# 1. Hilfsfunktion zur Umwandlung von Text in Zahlen (1 bis 5)
recode_likert <- function(x) {
  
  case_when(
    # Zustimmungsskala
    x == "Stimme überhaupt nicht zu" ~ 1,
    x == "Stimme eher nicht zu" ~ 2,
    x == "Neutral" ~ 3,
    x == "Stimme eher zu" ~ 4,
    x == "Stimme voll und ganz zu" ~ 5,
    
    # Häufigkeitsskala
    x == "Gar nicht" ~ 1,
    x == "Selten" ~ 2,
    x == "Manchmal" ~ 3,
    x == "Oft" ~ 4,
    x == "Immer" ~ 5,
    
    # Zufriedenheitsskala
    x == "Sehr unzufrieden" ~ 1,
    x == "Eher unzufrieden" ~ 2,
    x == "Neutral" ~ 3, 
    x == "Eher zufrieden" ~ 4,
    x == "Sehr zufrieden" ~ 5,
    
    TRUE ~ NA_real_ # Falls leer oder Tippfehler -> NA
  )
}

# 2. Anwendung auf den gesamten Datensatz
processed_data <- clean_data %>%
  # --- NEU: Vorbereinigung der Freitext-Spalte ---
  mutate(
    Fakultaet_Sonst_Clean = Fakultaet_Sonst %>%
      # 1. Alles, was nach einem "/" kommt, abschneiden (Statistik/Sport -> Statistik)
      str_remove("/.*") %>%
      # 2. " und " sowie alles danach entfernen (Erziehungswissenschaft und Anglistik -> Erziehungswissenschaft)
      str_remove(" und .*") %>%
      # 3. Leerzeichen am Rand entfernen
      str_trim() %>%
      # 4. Erster Buchstabe Groß, Rest klein (maschinenbau -> Maschinenbau)
      str_to_title()
  ) %>%
  
  # --- NEU: Manuelle Korrekturen für Schreibfehler & Vereinheitlichung ---
  mutate(
    Fakultaet_Sonst_Clean = case_when(
      # Fehlerkorrektur: "Maschienenbau" oder "Maschbau" -> "Maschinenbau"
      # str_detect sucht nach Mustern (regex): "masch" gefolgt von irgendwas bis "bau"
      str_detect(Fakultaet_Sonst_Clean, "Masch.*bau") ~ "Maschinenbau",
      
      # Vereinheitlichung: "Erziehungswissenschaft" -> "Erziehungswissenschaften"
      str_detect(Fakultaet_Sonst_Clean, "Erziehungswissenschaft") ~ "Erziehungswissenschaften",
      
      # Vereinheitlichung: "Wiwi" oder "Economics" -> "Wirtschaftswissenschaften"
      str_detect(Fakultaet_Sonst_Clean, "Wiwi|Economics|Wirtschafts-") ~ "Wirtschaftswissenschaften",
      
      # Vereinheitlichung: "Reha..." -> "Rehabilitationswissenschaften"
      str_detect(Fakultaet_Sonst_Clean, "Reha") ~ "Rehabilitationswissenschaften",
      
      # Wenn keine Regel greift, behalten wir den (bereinigten) Text
      TRUE ~ Fakultaet_Sonst_Clean
    )
  ) %>%
  
  # --- Teil 1: Zusammenfügen (jetzt mit der sauberen Spalte!) ---
  mutate(
    Fakultaet_Raw = case_when(
      # Wenn Sonstiges gewählt wurde, nimm die BEREINIGTE Spalte
      Fakultaet == "Sonstiges" & !is.na(Fakultaet_Sonst_Clean) ~ Fakultaet_Sonst_Clean,
      TRUE ~ Fakultaet
    ),
    
    # Abschluss (bleibt wie gehabt)
    Abschluss = as.factor(Abschluss),
    Fachsemester = as.numeric(Fachsemester)
  ) %>%
  
  # --- Teil 2: Umwandlung in Factor ---
  mutate(
    Fakultaet = as.factor(Fakultaet_Raw), # Leerzeichen sind oben schon weg
    # --- Teil 3: Rekodierung aller Likert-Skalen zu Zahlen (_Num) ---
    
    # Zielvariable
    Zufriedenheit_Score = recode_likert(Zufriedenheit_Lernerfolg),
    
    # Block: Einstellung zum Übungsbetrieb
    Einstellung_Uebung_Hilfreich_Num = recode_likert(Einstellung_Uebung_Hilfreich),
    Einstellung_Pflicht_Ball_Num     = recode_likert(Einstellung_Pflicht_Ball),
    Einstellung_Mat_Ausreichend_Num  = recode_likert(Einstellung_Mat_Ausreichend),
    Einstellung_Aufschieben_Num      = recode_likert(Einstellung_Aufschieben),
    Einstellung_Effizienz_Num        = recode_likert(Einstellung_Effizienz),
    
    # Block: Nutzungshäufigkeit
    Nutzung_Kurzskript_Num    = recode_likert(Nutzung_Kurzskript),
    Nutzung_Skript_Num        = recode_likert(Nutzung_Skript),
    Nutzung_Mitschriften_Num  = recode_likert(Nutzung_Mitschriften),
    Nutzung_Video_Num         = recode_likert(Nutzung_Video),
    Nutzung_Musterloesung_Num = recode_likert(Nutzung_Musterloesung),
    Nutzung_Buecher_Num       = recode_likert(Nutzung_Buecher),
    Nutzung_Altklausuren_Num  = recode_likert(Nutzung_Altklausuren),
    Nutzung_YouTube_Num       = recode_likert(Nutzung_YouTube),
    Nutzung_KI_Num            = recode_likert(Nutzung_KI),
    
    # Block: Qualität & Verfügbarkeit
    Qualitaet_Verstehen_Num     = recode_likert(Qualitaet_Verstehen),
    Qualitaet_Struktur_Num      = recode_likert(Qualitaet_Struktur),
    Qualitaet_Einfachheit_Num   = recode_likert(Qualitaet_Einfachheit),
    Qualitaet_Rechtzeitig_Num   = recode_likert(Qualitaet_Rechtzeitig),
    Qualitaet_Arbeitsbelastung_Num = recode_likert(Effekt_Arbeitsbelastung),
    Qualitaet_Erleichterung_Num = recode_likert(Qualitaet_Erleichterung),
    
    # Block: Motivation, Stress & Wirkung
    Effekt_Sicherheit_Num     = recode_likert(Effekt_Sicherheit),
    Effekt_Motivation_Num     = recode_likert(Effekt_Motivation),
    Effekt_Selbststaendig_Num = recode_likert(Effekt_Selbststaendig),
    Effekt_Stress_Num         = recode_likert(Effekt_Stress),
    Effekt_Zeitaufwand_Num    = recode_likert(Effekt_Zeitaufwand),
    Effekt_Relevanz_Num       = recode_likert(Effekt_Relevanz)
  )

# Kurzer Check, ob alles geklappt hat
glimpse(processed_data)

# ==============================================================================
# SCHRITT D: Reverse Coding (Umpolen negativer Items)
# ==============================================================================
# Wir polen Items um, bei denen Zustimmung (5) etwas "Schlechtes" bedeutet.
# Ziel: 5 soll immer "positiv/wünschenswert" sein.
# 
processed_data <- processed_data %>%
  mutate(
    # 1. Arbeitsbelastung: Hohe Belastung (5) -> Schlecht (1)
    Qualitaet_Arbeitsbelastung_Rev = 6 - Qualitaet_Arbeitsbelastung_Num,

    # 2. Zeitaufwand: Hoher Zeitaufwand (5) -> Schlecht (1)
    # (Nur falls du Zeitaufwand als negativ interpretierst!)
    Effekt_Zeitaufwand_Rev = 6 - Effekt_Zeitaufwand_Num
  )

# HINWEIS:
# Die Variable "Effekt_Stress_Num" ("reduzieren Stress") ist bereits positiv!
# (5 = Reduziert Stress viel = Gut). Die muss NICHT umgepolt werden.

# ==============================================================================
# SCHRITT E: Feature Engineering (Neue Variablen erstellen)
# ==============================================================================

# library(psych)
# 
# # (Äquivalent zur Titanic-Aufgabe "Extrahiert aus Cabin...")
# 
# # Berechne Cronbachs Alpha (QUALITAET)
# items_qualitaet <- processed_data %>%
#   select(
#     Qualitaet_Verstehen_Num, 
#     Qualitaet_Struktur_Num, 
#     Qualitaet_Rechtzeitig_Num,
#     Qualitaet_Erleichterung_Num
#   )
# alpha(items_qualitaet, check.keys = TRUE)
# # raw_alpha
# # 0.64
# 
# # Berechne Cronbachs Alpha (WIRKUNG)
# items_effekt <- processed_data %>%
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
# 
# 
# processed_data <- processed_data %>%
#   rowwise() %>%
#   mutate(
#     # 1. QUALITAETS-SCORE (Die "Basis")
#     # Fokus: Wie sind die Materialien beschaffen? (Struktur, Verständlichkeit)
#     Qualitaet_Score = mean(c(
#       Qualitaet_Verstehen_Num, 
#       Qualitaet_Struktur_Num, 
#       Qualitaet_Einfachheit_Num, 
#       Qualitaet_Erleichterung_Num,
#       Qualitaet_Rechtzeitig_Num
#     ), na.rm = TRUE),
#     
#     # 2. WIRKUNGS-SCORE (Der "Effekt")
#     # Fokus: Was lösen sie beim Studierenden aus? (Sicherheit, Motivation)
#     # Alpha hier ist sehr gut (0.79)!
#     Wirkungs_Score = mean(c(
#       Effekt_Sicherheit_Num, 
#       Effekt_Motivation_Num, 
#       Effekt_Selbststaendig_Num, 
#       Effekt_Stress_Num, 
#       Effekt_Relevanz_Num
#     ), na.rm = TRUE)
#   ) %>%
#   ungroup()

# Schritt 1 (Qualität): Wie bewerten die Studierenden die Materialien objektiv? (Struktur, Einfachheit).
# Schritt 2 (Wirkung): Was machen diese Materialien mit den Studierenden? (Motivieren sie? Nehmen sie die Angst?).
# Schritt 3 (Zusammenhang): Führt hohe Qualität zu besserer Wirkung? Und führt bessere Wirkung zu mehr Zufriedenheit?
# Hypothese: "Es wird vermutet, dass die Qualität der Materialien (Struktur) nicht direkt die Zufriedenheit bestimmt, 
# sondern indirekt über die Wirkung (Stressreduktion). Ein gut strukturiertes Skript macht erst dann zufrieden, 
# wenn es dadurch den Stress senkt."

# ==============================================================================
# SCHRITT F: Abschluss & Speichern
# ==============================================================================

# Nur die fertigen, sauberen Spalten behalten
final_dataset <- processed_data %>%
  select(
    Zufriedenheit_Score,
    Fakultaet,
    Abschluss,
    Fachsemester,
    Qualitaet_Arbeitsbelastung_Rev,
    Effekt_Zeitaufwand_Rev,
    ends_with("_Num")
  )

# Kontrolle
print("Datenvorverarbeitung abgeschlossen. Vorschau:")
glimpse(final_dataset)

# Speichern als RDS (R-Format, behält Datentypen) oder CSV
write_rds(final_dataset, "results-survey_cleaned.rds")
write_csv(final_dataset, "results-survey_cleaned.csv")
