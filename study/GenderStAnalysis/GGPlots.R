# Lade benötigte Bibliotheken
library(dplyr)
library(ggplot2)
library(cowplot)

# Funktion zur Berechnung von Mittelwerten und Standardfehlern
calculate_stats <- function(data, group_variables, value_variable) {
  data %>%
    group_by(across(all_of(group_variables))) %>%
    summarise(
      Mean = mean({{ value_variable }}, na.rm = TRUE),
      SE = sd({{ value_variable }}, na.rm = TRUE) / sqrt(n()),
      .groups = 'drop'
    )
}

# Funktion zum Erstellen von Balkendiagrammen
plot_data <- function(data, title, xlab, ylab, by_gender = FALSE) {
  if (by_gender) {
    ggplot(data, aes(x = gamifiedElement, y = Mean, fill = gender)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
      geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
                    width = 0.25, position = position_dodge(width = 0.8)) +
      labs(x = xlab, y = ylab) +
      theme_minimal() +
      theme(panel.grid.major = element_blank(),  # Entfernt Hauptgitterlinien
            panel.grid.minor = element_blank())  # Entfernt Nebengitterlinien
  } else {
    ggplot(data, aes(x = gamifiedElement, y = Mean, fill = "color")) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
                    width = 0.25, position = position_dodge(width = 0.7)) +
      scale_fill_manual(values = c("color" = "#f8bc6d")) +  # Ein leichtes Pastellblau
      labs(x = xlab, y = ylab) +
      theme_minimal() +
      theme(panel.grid.major = element_blank(),  # Entfernt Hauptgitterlinien
            panel.grid.minor = element_blank()) +  # Entfernt Nebengitterlinien
      theme(legend.position = "none")
  }
}

# Globale Variable für den Speicherpfad
output_folder <<- "C:\\Users\\robin\\Documents\\GitHub\\bachelor-thesis\\img\\plots\\2"

# Funktion zum Speichern von Plots mit Ordnerauswahl
save_ggplot <- function(plot, plot_name) {
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }
  file_path <- file.path(output_folder, paste0(plot_name, ".png"))
  ggsave(file_path, plot, height=20, width =30, units ="cm", dpi=800, bg="white")
}

# Hauptdatensatz
#dat_gender <- read.csv("your_data.csv")  # Pfad zur Datendatei anpassen

# Datenverarbeitung
agg_data_performance_gender <- calculate_stats(dat_gender, c("gender", "gamifiedElement"), percentage_correct)
agg_data_anxiety_gender <- calculate_stats(dat_gender, c("gender", "gamifiedElement"), stai)
agg_data_performance <- calculate_stats(dat_gender, "gamifiedElement", percentage_correct)
agg_data_anxiety <- calculate_stats(dat_gender, "gamifiedElement", stai)

# Daten visualisieren
plot_performance_gender <- plot_data(agg_data_performance_gender, "Average percentage of correct answers by Gender", "Gamified Element", "Average percentage of correct answers", TRUE)
plot_anxiety_gender <- plot_data(agg_data_anxiety_gender, "Average anxiety level by Gender", "Gamified Element", "Average anxiety level", TRUE)
plot_performance <- plot_data(agg_data_performance, "Average percentage of correct answers", "Gamified Element", "Average percentage of correct answers")
plot_anxiety <- plot_data(agg_data_anxiety, "Average anxiety level", "Gamified Element", "Average anxiety level")

# Diagramme anzeigen
print(plot_performance_gender)
print(plot_anxiety_gender)
print(plot_performance)
print(plot_anxiety)

# Diagramme speichern
save_ggplot(plot_performance_gender, "plot_performance_gender")
save_ggplot(plot_anxiety_gender, "plot_anxiety_gender")
save_ggplot(plot_performance, "plot_performance")
save_ggplot(plot_anxiety, "plot_anxiety")