# Lade benötigte Bibliotheken
library(dplyr)
library(ggplot2)
library(cowplot)

# Funktion zur Berechnung von Mittelwerten und Standardfehlern
calculate_stats <- function(data, group_variables, value_variable) {
  data %>%
    group_by(across(all_of(group_variables))) %>%
    summarise(
      Mean = mean({{ value_variable }}, na.rm = TRUE),  # Berechnet den absoluten Wert des Mittelwerts
      SE = sd({{ value_variable }}, na.rm = TRUE) / sqrt(n()),
      .groups = 'drop'
    )
}

# Funktion zur Anpassung der Legendenbeschriftung
capitalize_labels <- function(labels) {
  sapply(labels, function(label) {
    paste(toupper(substring(label, 1, 1)), substring(label, 2), sep = "")
  })
}

# Funktion zum Erstellen von Balkendiagrammen mit Unterstützung für negative Werte und Achsenspiegelung
plot_data <- function(data, xlab, ylab, by_gender = FALSE) {
  
  if (by_gender) {
    p <- ggplot(data, aes(x = gamifiedElement, y = Mean, fill = gender)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
      geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
                    width = 0.25, position = position_dodge(width = 0.8)) +
      labs(x = xlab, y = ylab) +
      theme_minimal(base_family = "Arial") +
      guides(fill = guide_legend(title = "Gender", label.position = "right",
                                 labels = capitalize_labels)) +
      theme(
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 25),  # Textgröße erhöhen
        axis.text.x = element_text(angle = -45, hjust = 0, vjust = 0, size = 22),  # X-Achsenbeschriftung drehen
        axis.text.y = element_text(size = 25),  # Y-Achsenbeschriftung Textgröße
        axis.title.x = element_text(size = 25, margin = margin(t = 20)),  # X-Achsentitel Textgröße und Abstand
        axis.title.y = element_text(size = 25, margin = margin(r = 20)),  # Y-Achsentitel Textgröße und Abstand
        legend.title = element_text(size = 25),  # Legendentitel Textgröße
        legend.text = element_text(size = 25),  # Legendeneinträge Textgröße
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20)  # Ränder hinzufügen
      ) +
      scale_y_continuous(labels = scales::comma, limits = c(floor(min(data$Mean - data$SE)), ceiling(max(data$Mean + data$SE))))
  } else {
    p <- ggplot(data, aes(x = gamifiedElement, y = Mean, fill = "color")) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
                    width = 0.25, position = position_dodge(width = 0.7)) +
      scale_fill_manual(values = c("color" = "#f8bc6d")) +
      labs(x = xlab, y = ylab) +
      theme_minimal(base_family = "Arial") +
      theme(
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 25),  # Textgröße erhöhen
        axis.text.x = element_text(angle = -45, hjust = 0, vjust = 0, size = 22),  # X-Achsenbeschriftung drehen
        axis.text.y = element_text(size = 25),  # Y-Achsenbeschriftung Textgröße
        axis.title.x = element_text(size = 25, margin = margin(t = 20)),  # X-Achsentitel Textgröße und Abstand
        axis.title.y = element_text(size = 25, margin = margin(r = 20)),  # Y-Achsentitel Textgröße und Abstand
        legend.position = "none",  # Legende entfernen
        plot.margin = margin(t = 20, r = 140, b = 20, l = 20)  # Ränder hinzufügen
      ) +
      scale_y_continuous(labels = scales::comma, limits = c(floor(min(data$Mean - data$SE)), ceiling(max(data$Mean + data$SE))))
  }    
  # Achsen spiegeln, wenn negative Werte vorhanden sind
  if (any(data$Mean < 0)) {
    p <- p + scale_y_reverse()
  }
  return(p)
}




# Globale Variable für den Speicherpfad
output_folder <<- "C:\\Users\\robin\\Documents\\GitHub\\bachelor-thesis\\img\\plots\\4"

# Funktion zum Speichern von Plots mit Ordnerauswahl
save_ggplot <- function(plot, plot_name) {
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }
  file_path <- file.path(output_folder, paste0(plot_name, ".png"))
  ggsave(file_path, plot, height=20, width =30, units ="cm", dpi=800, bg="transparent")
}

# Hauptdatensatz
#dat_gender <- read.csv("your_data.csv")  # Pfad zur Datendatei anpassen

# Datenverarbeitung
agg_data_performance_gender <- calculate_stats(dat_gender, c("gender", "gamifiedElement"), percentage_correct)
agg_data_anxiety_gender <- calculate_stats(dat_gender, c("gender", "gamifiedElement"), stai)
agg_data_performance <- calculate_stats(dat_gender, "gamifiedElement", percentage_correct)
agg_data_anxiety <- calculate_stats(dat_gender, "gamifiedElement", stai)
agg_data_performance_gender_no_ge <- calculate_stats(dat_gender, "gender", percentage_correct)

# Daten visualisieren
plot_performance_gender <- plot_data(agg_data_performance_gender, "Gamified Element", "Average correct answers", TRUE)
plot_anxiety_gender <- plot_data(agg_data_anxiety_gender, "Gamified Element", "Average anxiety level", TRUE)
plot_performance <- plot_data(agg_data_performance, "Gamified Element", "Average correct answers")
plot_anxiety <- plot_data(agg_data_anxiety, "Gamified Element", "Average anxiety level")

# Diagramme anzeigen
#print(plot_performance_gender)
#print(plot_anxiety_gender)
#print(plot_performance)
#print(plot_anxiety)

# Diagramme speichern
save_ggplot(plot_performance_gender, "plot_performance_gender")
save_ggplot(plot_anxiety_gender, "plot_anxiety_gender")
save_ggplot(plot_performance, "plot_performance")
save_ggplot(plot_anxiety, "plot_anxiety")