## ---------------------------
## Script name: .R
##
## Author: Nadine N. Koch
##
## Date Created: 04.07.2024
## ---------------------------
##
## Input: "dataGenderPreProcessed.txt"
##
##
## Output:
##
## ---------------------------
##
## necessary packages
library(lmerTest)
#library(robustlmm)
library(sjPlot)
#library(ez)
library("car")
## ------------ optional: set working directory ---------------
# please fill in the directory of the scripts here
# please uncomment the following lines
#path <- "Documents/Lehre/Betreuung Arbeiten/Robin/"
# set working directory
#setwd(path)

se <- function(x){
  return(sd(x)/sqrt(length(x)))
}


## ------------ read in data set ---------------
dat_gender_All <-
  read.table("dataGenderPreProcessed.txt",
             header = TRUE,
             sep = "\t")


## ------------ remove data ---------------
# number of participants (each has 3 data sets)
length(dat_gender_All$id) / 3
# remove others
dat_gender_without_other <-
  dat_gender_All[dat_gender_All$gender != "other", ]
# number of participants (each has 3 data sets)
length(dat_gender_without_other$id) / 3

# remove lines with less than 25% of correct answers
## remove complete participant or only the condition?
remove_id <-
  dat_gender_without_other$id[dat_gender_without_other$percentage_correct < 0.25]
dat_gender <-
  dat_gender_without_other[!dat_gender_without_other$id %in% remove_id, ]
# number of participants (each has 3 data sets)
length(dat_gender$id) / 3

# convert gender and gamifiedElement to factor
dat_gender$gender <- factor(dat_gender$gender)
dat_gender$gamifiedElement <- factor(dat_gender$gamifiedElement,
                                     levels = c("none", "p", "b", "l", "a",
                                                "n", "pbla", "pblan"))

# Umbenennen der Faktoren für 'gamifiedElement'
levels(dat_gender$gamifiedElement) <- c("None", "Points (P)", "Badges (B)", "Leaderboards (L)", "Avatars (A)", "Narrative Content (N)", "Combination (PBLA)", "All Elements (PBLAN)")

dat_gender_leaderboards <- dat_gender[dat_gender$gamifiedElement == "Leaderboards (L)", ]
dat_gender_avatars <- dat_gender[dat_gender$gamifiedElement == "Avatars (A)", ]

count_fours <- sapply(dat_gender[, c("stai_1", "stai_2", "stai_3", "stai_4", "stai_5", "stai_6")], function(x) sum(x == 1))
print(count_fours)


dat_gender$id <- factor(dat_gender$id)
dat_gender$round <- factor(dat_gender$round)

## ------------ tables, means and sds ---------------

## ------------ LMMs ---------------
# ------------ performance ---------------
lmer_per <-
  lmerTest::lmer(
    percentage_correct ~ gender * gamifiedElement + (1 | id),
    dat_gender,
    REML = TRUE,
    control = lmerControl(optimizer = "Nelder_Mead")
  )

aggregate(percentage_correct ~ gender, dat_gender, mean)

aggregate(percentage_correct ~ gamifiedElement, dat_gender, mean)

aggregate(percentage_correct ~ gender + gamifiedElement, dat_gender, mean)

sjPlot::tab_model(
  lmer_per,
  show.intercept = FALSE,
  show.est = TRUE,
  show.ci = 0.95,
  show.std = "std",
  show.se = TRUE,
  show.stat = TRUE,
  show.p = TRUE,
  p.adjust = "BH",
  show.df = TRUE,
  show.r2 = TRUE,
  show.obs = TRUE,
  digits = 2,
  df.method = "satterthwaite"
)


# ------------------------------ Anxiety ------------------------------------------------
  lmer_stai <- lmerTest::lmer(
    stai ~ gender * gamifiedElement + (1 | id),
    dat_gender,
    REML = TRUE,
    control = lmerControl(optimizer = "Nelder_Mead")
  )


sjPlot::tab_model(
  lmer_stai,
  show.intercept = FALSE,
  show.est = TRUE,
  show.ci = 0.95,
  show.std = "std",
  show.se = TRUE,
  show.stat = TRUE,
  show.p = TRUE,
  p.adjust = "BH",
  show.df = TRUE,
  show.r2 = TRUE,
  show.obs = TRUE,
  digits = 2,
  df.method = "satterthwaite"
)