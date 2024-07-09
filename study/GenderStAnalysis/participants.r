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
