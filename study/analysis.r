# Load necessary packages --------------------------------------------
#library(tidyverse)

# Define the URL and the file path -----------------------------------
url <- "https://gender.robingebert.com/csv"
file_path <- "gender_data.csv"

# Download the CSV file ----------------------------------------------
download.file(url, destfile = file_path, method = "curl")

# Read the CSV file --------------------------------------------------
data <- read.csv2(file_path)

question_columns <- grep("^r[1-3]+_q", colnames(data), value = TRUE)

# Berechne den Anteil der richtig beantworteten Fragen pro Datensatz
data <- data %>%
  rowwise() %>%
  mutate(correct_percentage = mean(c_across(all_of(question_columns)) == TRUE)) %>%
  ungroup()

# Filtere die Daten nach den Anforderungen
cleaned_data <- data %>%
  #filter(correct_percentage >= 0.25 & gender != "other") %>%
  select(-correct_percentage)  # Entferne die Hilfsspalte

# Spalten "gender" und "r*_gamifiedElement" in Faktoren umwandeln
cleaned_data <- cleaned_data %>%
  mutate(
    gender = as.factor(gender)
  )

# Identifiziere alle Spalten, die mit "r*_gamifiedElement" beginnen
gamified_columns <- grep("^r[0-9]+_gamifiedElement", colnames(cleaned_data), value = TRUE)

# Wandeln die identifizierten Spalten in Faktoren um
cleaned_data <- cleaned_data %>%
  mutate(across(all_of(gamified_columns), as.factor))

# Ergebnis anzeigen -------------------------------------------------
view(cleaned_data)

# Levels der Faktoren anzeigen ---------------------------------------
# Levels der 'gender' Spalte
print("Levels der 'gender' Spalte:")
print(levels(cleaned_data$gender))

# Levels der 'r*_q' Spalten
for (col in gamified_columns) {
  cat("Levels der", col, "Spalte:\n")
  print(levels(cleaned_data[[col]]))
}