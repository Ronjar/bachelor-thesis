## ---------------------------
## Script name: dataPreProcessing.R
##
## Author: Nadine N. Koch
##
## Date Created: 04.07.2024
## ---------------------------
##
## Input: "gender_data.csv§
##      
##
## Output: "dataGenderPreProcessed.txt"
##
## ---------------------------
##
## necessary packages
library(tidyverse)

## ------------ optional: set working directory ---------------
# please fill in the directory of the scripts here
# please uncomment the following lines
#path <- "Documents/Lehre/Betreuung Arbeiten/Robin/"
# set working directory
#setwd(path)


# Define the URL and the file path -----------------------------------
url <- "https://gender.robingebert.com/csv"

# Download the CSV file ----------------------------------------------
download.file(url, destfile = "gender_data.csv", method = "auto")

#------------ read in data ---------------
data_wide <- read.csv2("gender_data.csv", na.strings = "")


#------------ reshape to long ---------------
data <- data_wide %>%
  pivot_longer(
    cols = starts_with("r"),
    names_to = c("round", ".value"),
    names_pattern = "r(\\d+)_(.*)"
  ) %>%
  mutate(round = as.integer(round))

# convert back to data frame
data <- as.data.frame(data)

#------------ recode, clean and convert to logical or factor ---------------
# recode NA to none
data$gamifiedElement[is.na(data$gamifiedElement)] <- "none"

# remove missing data sets
data <- na.omit(data)

# Convert questions to logicals
question_columns <- grep("q[1-20]?", colnames(data), value = TRUE)
data <- data %>%
  mutate_at(question_columns, list(~as.logical(.)))

# convert gender and gamifiedElement to factor
data$gender <- factor(data$gender)
data$gamifiedElement <- factor(data$gamifiedElement)

#------------ calculate scale values ---------------
#------------ performance ---------------
# get columns with questions
question_columns <- grep("q[1-20]?", colnames(data), value = TRUE)

# calculate number of correct items and percentage
data$nr_correct <- rowSums(data[,names(data) %in% question_columns]*1)
data$percentage_correct <- data$nr_correct/20

#------------ self efficacy ---------------
# mean od ngse items
ngse_columns <- grep("ngse_[1-20]?", colnames(data), value = TRUE)
data$ngse <- rowMeans(data[,names(data) %in% ngse_columns])

#------------ motivation ---------------
# mean of SIMS scales
sims_columns <- grep("sims_[1-20]?", colnames(data), value = TRUE)
data$sims_intr_mot <- rowMeans(data[,names(data) %in% c("sims_1", "sims_5", 
                                               "sims_9", "sims_13")])
data$sims_ident_reg <- rowMeans(data[,names(data) %in% c("sims_2", "sims_6", 
                                                        "sims_10", "sims_14")])
data$sims_ext_reg <- rowMeans(data[,names(data) %in% c("sims_3", "sims_7", 
                                                        "sims_11", "sims_15")])
data$sims_amot <- rowMeans(data[,names(data) %in% c("sims_4", "sims_8", 
                                                        "sims_12", "sims_16")])
#------------ anxiety ---------------
# weightsfor each answer:
# first number "not at all", second number "somewhat", 
# thrid number "moderately", forth number "very much"
item1 <- c(2.66, 0.60, -1.41, -3.55)
item2 <- c(-2.9, -0.76, 1.25, 3.31)
item3 <- c(-1.27, 0.87, 2.88, 4.94)
item4 <- c(4.09, 0.01, -2.00, -4.14)
item5 <- c(2.25, 0.19, -1.82,-3.96)
item6 <- c(-2.95, -0.81, 1.2, 3.26)

# weights according to the answers
data$stai_1_weight <- item1[data[,c("stai_1")]]
data$stai_2_weight <- item2[data[,c("stai_2")]]
data$stai_3_weight <- item3[data[,c("stai_3")]]
data$stai_4_weight <- item4[data[,c("stai_4")]]
data$stai_5_weight <- item5[data[,c("stai_5")]]
data$stai_6_weight <- item6[data[,c("stai_6")]]

# get stai-y value
stai_columns <- grep("stai_[1-20]?", colnames(data), value = TRUE)
stai_weight_columns <- grep("stai_[1-20]?_weight", colnames(data), value = TRUE)
data$stai <- rowMeans(data[,names(data) %in% stai_columns])

#------------ write external table with relevant data ---------------
write.table(data[,!names(data) %in% c(ngse_columns, sims_columns, 
                                     stai_columns, stai_weight_columns)], 
            "dataGenderPreProcessed.txt", quote = FALSE, 
            sep = "\t", row.names = FALSE)
