library(tidyverse)

# Define the URL and the file path -----------------------------------
url <- "https://gender.robingebert.com/csv"
file_path <- "gender_data.csv"

# Download the CSV file ----------------------------------------------
download.file(url, destfile = file_path, method = "auto")

# Read the CSV file --------------------------------------------------
data <- read.csv2(file_path)

# Entferne alle Datensätze, bei denen das Geschlecht 'other' ist
data <- data %>% 
  filter(gender != "other")

# Konvertiere alle Frage-Spalten in logicals
question_columns <- grep("^r[1-3]_q[1-20]?", colnames(data), value = TRUE)
data <- data %>%
  mutate(across(all_of(question_columns), ~ .x == "true"))

# Identifiziere die Frage-Spalten für jede Runde
question_columns_r1 <- grep("^r1_q[1-20]?", colnames(data), value = TRUE)
question_columns_r2 <- grep("^r2_q[1-20]?", colnames(data), value = TRUE)
question_columns_r3 <- grep("^r3_q[1-20]?", colnames(data), value = TRUE)


# Berechne den Anteil der richtig beantworteten Fragen pro Runde
data <- data %>%
  rowwise() %>%
  mutate(
    correct_percentage_r1 = mean(c_across(all_of(question_columns_r1)) == TRUE, na.rm = TRUE),
    correct_percentage_r2 = mean(c_across(all_of(question_columns_r2)) == TRUE, na.rm = TRUE),
    correct_percentage_r3 = mean(c_across(all_of(question_columns_r3)) == TRUE, na.rm = TRUE)
  ) %>%
  ungroup()

# Identifiziere Nutzer, bei denen in einer der Runden die korrekte Antwortrate von weniger als 25% erreicht wird
users_to_remove <- data %>%
  filter(correct_percentage_r1 < 0.25 | correct_percentage_r2 < 0.25 | correct_percentage_r3 < 0.25) %>%
  pull(id) %>% 
  unique()

# Filtere alle Datensätze dieser Nutzer heraus
cleaned_data <- data %>%
  filter(!(id %in% users_to_remove)) %>%
  select(-starts_with("correct_percentage"))  # Entferne die Hilfsspalten

# Spalten "gender" und "r*_gamifiedElement" in Faktoren umwandeln
gamified_columns <- grep("^r[0-9]+_gamifiedElement", colnames(cleaned_data), value = TRUE)
cleaned_data <- cleaned_data %>%
  mutate(
    gender = as.factor(gender),
    across(all_of(gamified_columns), as.factor)
  )
