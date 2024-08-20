## ---------------------------
## Script name: dataPreProcessing.R
##
## Author: Nadine N. Koch
##
## Date Created: 04.07.2024
## ---------------------------
##
## Input: "dat_gender" from Analysis.r, considering each person has three entries
##      
##
## Output: Console summary of demographics and study programs
##
## ---------------------------
## necessary packages
library(dplyr)

## ------------ optional: set working directory ---------------
# please fill in the directory of the scripts here
# please uncomment the following lines
# path <- "Documents/Lehre/Betreuung Arbeiten/Robin/"
# set working directory
# setwd(path)

## Use distinct records for each person to calculate age statistics
distinct_people <- dat_gender %>%
  distinct(id, .keep_all = TRUE)  # Take the first occurrence to represent each person

## Calculate basic statistics for age
min_age <- min(distinct_people$age, na.rm = TRUE)
max_age <- max(distinct_people$age, na.rm = TRUE)
mean_age <- mean(distinct_people$age, na.rm = TRUE)
sd_age <- sd(distinct_people$age, na.rm = TRUE)

## Gender distribution
total_participants <- nrow(distinct_people)

## Gender distribution
gender_distribution <- distinct_people %>%
  group_by(gender) %>%
  summarise(Count = n(), Percentage = (Count / total_participants) * 100)

## Top three study programs
top_study_programs <- distinct_people %>%
  group_by(studyProgram) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  slice_head(n = 5)

## Print the summary in the console
output_text <- paste(
  "Study Participants Summary:\n",
  "Total Participants: ", nrow(distinct_people), "\n",
  "Age Range: ", min_age, " to ", max_age, "\n",
  "Average Age: ", format(mean_age, digits = 4), " (SD = ", format(sd_age, digits = 4), ")\n",
  "Gender Distribution:\n",
  paste(gender_distribution$gender, gender_distribution$Count, " (", format(gender_distribution$Percentage, digits = 4), "%)", collapse = ", "),
  "\nTop Three Study Programs:\n  - ",
  paste(top_study_programs$studyProgram, top_study_programs$Count, collapse = ",\n  - "),
  sep = ""
)

cat(output_text)
