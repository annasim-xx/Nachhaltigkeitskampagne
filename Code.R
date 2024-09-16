library(tibble)
library(kableExtra)
library(dplyr)

# Erstellen der HradilTabelle
HradilTabelle <- tribble(
  ~`Name der Lage`, ~`Primäre Dimension`, ~`Sekundäre Dimensionen`,
  "Macht-Elite", "Formale Macht 1", "Geld 1, Geld 2, Formale Bildung 1, Formale Bildung 2, Prestige 1, Prestige 2",
  "Reiche", "Geld 1", "Formale Bildung 1, Formale Bildung 2, Formale Bildung 3, Prestige 1, Prestige 2, Formale Macht 1, Formale Macht 2, Formale Macht 3",
  "Bildungselite", "Formale Bildung 1", "Geld 2, Geld 3, Prestige 1, Prestige 2, Formale Macht 2, Formale Macht 3",
  "Manager", "Formale Macht 2", "Geld 1, Geld 2, Formale Bildung 1, Formale Bildung 2, Prestige 2, Arbeitsbedingungen 2, Arbeitsbedingungen 3, Arbeitsbedingungen 4, Freizeitbedingungen 3, Freizeitbedingungen 4",
  "Experten", "Formale Bildung 2", "Geld 1, Geld 2, Geld 3, Prestige 2, Prestige 3, Formale Macht 2, Formale Macht 3, Formale Macht 4, Arbeitsbedingungen 2, Arbeitsbedingungen 3, Arbeitsbedingungen 4, Freizeitbedingungen 4, Freizeitbedingungen 5",
  "Studenten", "Formale Bildung 3", "Geld 3, Geld 4, Geld 5, Arbeitsbedingungen 1, Arbeitsbedingungen 2, Arbeitsbedingungen 3, Freizeitbedingungen 1, Freizeitbedingungen 2, Freizeitbedingungen 3",
  "Normalverdiener mit geringen Risiken", "Geld 3, Geld 4, Risiken 1, Risiken 2", "Formale Bildung 3, Formale Bildung 4, Prestige 3, Prestige 4, Formale Macht 3, Formale Macht 4, Arbeitsbedingungen 1, Arbeitsbedingungen 2, Arbeitsbedingungen 3, Freizeitbedingungen 1, Freizeitbedingungen 2, Freizeitbedingungen 3, Wohnbedingungen 2, Wohnbedingungen 3",
  "Normalverdiener mit mittleren Risiken", "Geld 3, Geld 4, Risiken 3, Risiken 4", "Formale Bildung 3, Formale Bildung 4, Prestige 3, Prestige 4, Formale Macht 3, Formale Macht 4, Arbeitsbedingungen 2, Arbeitsbedingungen 3, Arbeitsbedingungen 4, Freizeitbedingungen 2, Freizeitbedingungen 3, Freizeitbedingungen 4, Wohnbedingungen 2, Wohnbedingungen 3, Wohnbedingungen 4, Soziale Absicherung 2, Soziale Absicherung 3, Soziale Absicherung 4",
  "Normalverdiener mit hohen Risiken", "Geld 3, Geld 4, Risiken 5, Risiken 6", "Formale Bildung 3, Formale Bildung 4, Prestige 3, Prestige 4, Prestige 5, Arbeitsbedingungen 3, Arbeitsbedingungen 4, Arbeitsbedingungen 5, Freizeitbedingungen 3, Freizeitbedingungen 4, Freizeitbedingungen 5, Wohnbedingungen 3, Wohnbedingungen 4, Soziale Absicherung 3, Soziale Absicherung 4, Soziale Absicherung 5",
  "Rentner", "Geld 2, Geld 3, Geld 4, Soziale Rollen 5, Soziale Rollen 6", "Prestige 4, Soziale Absicherung 3, Soziale Absicherung 4, Soziale Absicherung 5, Freizeitbedingungen 3, Freizeitbedingungen 4, Wohnbedingungen 2, Wohnbedingungen 3, Wohnbedingungen 4, Wohnbedingungen 5, Demokratische Institutionen 4, Demokratische Institutionen 5, Soziale Beziehungen 3, Soziale Beziehungen 4, Soziale Beziehungen 5",
  "Arbeitslose (langfristig)", "Geld 4, Geld 5, Risiken 5, Risiken 6", "Formale Bildung 4, Formale Bildung 5, Prestige 4, Prestige 5, Soziale Absicherung 3, Soziale Absicherung 4, Soziale Absicherung 5, Wohnbedingungen 4, Wohnbedingungen 5, Demokratische Institutionen 4, Demokratische Institutionen 5, Soziale Beziehungen 3, Soziale Beziehungen 4, Soziale Beziehungen 5, Soziale Rollen 4, Soziale Rollen 5, Soziale Rollen 6",
  "Arme (keine Erwerbspersonen)", "Geld 6", "Prestige 5, Soziale Absicherung 4, Soziale Absicherung 5, Freizeitbedingungen 4, Freizeitbedingungen 5, Wohnbedingungen 4, Wohnbedingungen 5, Demokratische Institutionen 4, Demokratische Institutionen 5, Soziale Beziehungen 3, Soziale Beziehungen 4, Soziale Beziehungen 5",
  "Randgruppen", "Diskriminierung 5, Diskriminierung 6", "Geld 3, Geld 4, Geld 5, Formale Bildung 4, Formale Bildung 5, Soziale Absicherung 4, Soziale Absicherung 5, Soziale Absicherung 6, Wohnbedingungen 3, Wohnbedingungen 4, Wohnbedingungen 5, Wohnbedingungen 6, Demokratische Institutionen 4, Demokratische Institutionen 5, Demokratische Institutionen 6, Soziale Rollen 4, Soziale Rollen 5, Soziale Rollen 6"
)

# Erstellen der Variablentabelle
Variablentabelle <- tribble(
  ~Faktoren, ~`1M`, ~`2M`, ~`3M`, ~`4M`, ~`5M`, ~`6M`, ~`1T`, ~`2T`, ~`3T`, ~`4T`, ~`5T`, ~`6T`,
  "Formale Macht", "hoch", "hoch", "mittel", "mittel", "mittel", "mittel", "niedrig", "niedrig", "mittel", "mittel", "mittel", "mittel",
  "Geld", "hoch", "hoch", "mittel", "mittel", "niedrig", "niedrig", "niedrig", "niedrig", "mittel", "mittel", "mittel", "mittel",
  "Formale Bildung", "mittel", "mittel", "mittel", "mittel", "mittel", "mittel", "niedrig", "niedrig", "mittel", "mittel", "hoch", "hoch",
  "Risiken", "hoch", "hoch", "mittel", "mittel", "niedrig", "niedrig", "mittel", "mittel", "mittel", "mittel", "mittel", "mittel",
  "Soziale Rollen", "mittel", "mittel", "mittel", "mittel", "niedrig", "niedrig", "mittel", "mittel", "mittel", "mittel", "mittel", "mittel",
  "Soziale Beziehungen", "hoch", "hoch", "mittel", "mittel", "niedrig", "niedrig", "mittel", "mittel", "mittel", "mittel", "mittel", "mittel",
  "Diskriminierung", "mittel", "mittel", "mittel", "mittel", "mittel", "mittel", "mittel", "mittel", "mittel", "mittel", "hoch", "hoch",
  "Prestige", "hoch", "hoch", "mittel", "mittel", "mittel", "mittel", "niedrig", "niedrig", "mittel", "mittel", "mittel", "mittel",
  "Freizeitbedingungen", "hoch", "hoch", "mittel", "mittel", "niedrig", "niedrig", "mittel", "mittel", "mittel", "mittel", "mittel", "mittel",
  "Arbeitsbedingungen", "hoch", "hoch", "mittel", "mittel", "niedrig", "niedrig", "mittel", "mittel", "mittel", "mittel", "mittel", "mittel",
  "Wohnbedingungen", "mittel", "mittel", "mittel", "mittel", "mittel", "mittel", "mittel", "mittel", "mittel", "mittel", "mittel", "mittel",
  "Soziale Absicherungen", "hoch", "hoch", "mittel", "mittel", "niedrig", "niedrig", "mittel", "mittel", "mittel", "mittel", "mittel", "mittel",
  "Demokratische Institutionen", "hoch", "hoch", "mittel", "mittel", "niedrig", "niedrig", "niedrig", "niedrig", "mittel", "mittel", "hoch", "hoch"
)

# Konvertierung von Zeichenketten zu numerischen Werten
VariablentabelleMitZahlen <- Variablentabelle %>%
  mutate(across(-Faktoren, ~case_when(
    . == "hoch" ~ 7.5,
    . == "mittel" ~ 5.0,
    . == "niedrig" ~ 2.5,
    TRUE ~ NA_real_
  )))

# Funktion zur Zuordnung der Pfeile und Kreise basierend auf Terzil
assign_terzil_symbol <- function(value, terzil_low, terzil_high) {
  if (is.na(value)) return(NA)
  if (value <= terzil_low) {
    "↓"
  } else if (value >= terzil_high) {
    "↑"
  } else {
    "○"
  }
}

# Funktion zum Berechnen der Terzile
calculate_terzile <- function(data) {
  terzil_low <- quantile(data, 1/3, na.rm = TRUE)
  terzil_high <- quantile(data, 2/3, na.rm = TRUE)
  list(low = terzil_low, high = terzil_high)
}

# Funktion zum Extrahieren und Summieren der Werte mit angepasster Gewichtung
calculate_potential_and_transparency <- function(primary, secondary, variablentabelle) {
  # Hilfsfunktion, um Werte für Mobilisierungspotential und Transparenzbedarf zu berechnen
  extract_and_sum_values <- function(dimension, variablentabelle, suffix) {
    dims <- strsplit(dimension, ",\\s*")[[1]]  # Dimensionen in einzelne Elemente aufteilen
    values <- sapply(dims, function(dim) {
      main_dim <- sub("\\s*\\d", "", dim)  # Hauptdimension extrahieren (ohne Nummer)
      index <- as.numeric(gsub("\\D", "", dim))  # Nummer der Dimension extrahieren
      column_name <- paste0(index, suffix)  # Spaltennamen erstellen, z.B. 1M oder 1T
      as.numeric(variablentabelle[variablentabelle$Faktoren == main_dim, column_name])
    })
    sum(values, na.rm = TRUE) / length(dims)  # Durchschnitt der Werte
  }
  
  # Angepasste Gewichtung der primären und sekundären Dimensionen
  mobilisierungspotential <- extract_and_sum_values(primary, variablentabelle, "M") * (2/3) +
    extract_and_sum_values(secondary, variablentabelle, "M") * (1/3)
  transparenzbedarf <- extract_and_sum_values(primary, variablentabelle, "T") * (2/3) +
    extract_and_sum_values(secondary, variablentabelle, "T") * (1/3)
  
  c(Mobilisierungspotential = mobilisierungspotential, Transparenzbedarf = transparenzbedarf)
}

# Berechnung der Potentiale und Bedarfe
result <- HradilTabelle %>%
  rowwise() %>%
  mutate(
    Mobilisierungspotential = calculate_potential_and_transparency(`Primäre Dimension`, `Sekundäre Dimensionen`, VariablentabelleMitZahlen)["Mobilisierungspotential"],
    Transparenzbedarf = calculate_potential_and_transparency(`Primäre Dimension`, `Sekundäre Dimensionen`, VariablentabelleMitZahlen)["Transparenzbedarf"]
  ) %>%
  select(`Name der Lage`, `Primäre Dimension`, Mobilisierungspotential, Transparenzbedarf) %>%
  ungroup()

# Berechnung der Terzile für die Symbolzuordnung
terzile_potential <- calculate_terzile(result$Mobilisierungspotential)
terzile_transparency <- calculate_terzile(result$Transparenzbedarf)

# Anwendung der Zuordnung der Pfeile und Kreise
result_with_symbols <- result %>%
  mutate(
    Mobilisierungspotential = sapply(Mobilisierungspotential, assign_terzil_symbol, terzil_low = terzile_potential$low, terzil_high = terzile_potential$high),
    Transparenzbedarf = sapply(Transparenzbedarf, assign_terzil_symbol, terzil_low = terzile_transparency$low, terzil_high = terzile_transparency$high)
  )

# Ergebnis anzeigen
result_with_symbols %>%
  kable() %>%
  kable_styling(full_width = FALSE)

