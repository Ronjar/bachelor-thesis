## ---------------------------
## Script name: dataPreProcessing.R
##
## Author: Nadine N. Koch
##
## Date Created: 04.07.2024
## ---------------------------
##
## Input: "gender_data.csv"
##      
##
## Output: Console summary of demographics and study programs
##
## ---------------------------
## necessary packages
library(tidyverse)

## ------------ optional: set working directory ---------------
# please fill in the directory of the scripts here
# please uncomment the following lines
# path <- "Documents/Lehre/Betreuung Arbeiten/Robin/"
# set working directory
# setwd(path)


#------------ read in data ---------------
data_wide <- read.csv2("gender_data.csv", na.strings = "")

## Calculate basic statistics for age
min_age <- min(data_wide$age, na.rm = TRUE)
max_age <- max(data_wide$age, na.rm = TRUE)
mean_age <- mean(data_wide$age, na.rm = TRUE)
sd_age <- sd(data_wide$age, na.rm = TRUE)

## Gender distribution
gender_distribution <- data_wide %>%
  group_by(gender) %>%
  summarise(Count = n(), Percentage = (Count / sum(Count)) * 100)

## Top three study programs
top_study_programs <- data_wide %>%
  group_by(studyProgram) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  slice_head(n = 3)

## Print the summary in the console
output_text <- paste(
  "Study Participants Summary:\n",
  "Total Participants: ", nrow(data_wide), "\n",
  "Age Range: ", min_age, " to ", max_age, "\n",
  "Average Age: ", format(mean_age, digits = 2), " (SD = ", format(sd_age, digits = 2), ")\n",
  "Gender Distribution:\n",
  paste(gender_distribution$gender, gender_distribution$Count, " (", format(gender_distribution$Percentage, digits = 2), "%)", collapse = ", "),
  "\nTop Three Study Programs:\n",
  paste(top_study_programs$studyProgram, top_study_programs$Count, collapse = ", "),
  sep = ""
)

cat(output_text)
