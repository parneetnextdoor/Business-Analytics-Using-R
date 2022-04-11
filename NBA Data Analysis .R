#I pledge on my honor that I have neither received nor given unauthorized assistance on this deliverable.

#### PREAMBLE : ## Clearing mem buffers ####
rm(list = ls(all.names = TRUE))# clear all files
gc() # garbage collector i.e.,  dump buffers and show available mem space
if(!is.null(dev.list())) dev.off() #Clears plots and No error if no plots
cat("\014")  # Clear Console

#CUSTOM FUNCTIONS**********************************************************************************************************

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))#Specifies decimal for an output

options(scipen = 999) #Removes all Scientific notation from the output

# install.packages("tidyverse")
library(tidyr)
# *************************************************************************************************************************

dt <- read.csv("2019-20_pbp_Refinedv3.csv", header = TRUE, sep = ",") #Reads the CSV file and stores it in the database dt

dtAwayMIA <- dt[dt$AwayTeam == "MIA",]
dtHomeMIA <- dt[dt$HomeTeam == "MIA",]

dtMIA <- rbind(dtAwayMIA, dtHomeMIA)

EssentialdtMIA <- dtMIA[,c(2,4,9,10,11,12,18,30,32,39,40,41,42)]

# First Check for MIA
str(EssentialdtMIA)

# Data Clean Up & Wrangling

# Cleans up the Shot type column to a numeric format
EssentialdtMIA[EssentialdtMIA == "2-pt dunk"] <- as.numeric(1)
EssentialdtMIA[EssentialdtMIA == "2-pt hook shot"] <- as.numeric(2)
EssentialdtMIA[EssentialdtMIA == "2-pt jump shot"] <- as.numeric(3)
EssentialdtMIA[EssentialdtMIA == "2-pt layup"] <- as.numeric(4)
EssentialdtMIA[EssentialdtMIA == "3-pt jump shot"] <- as.numeric(5)

#Force columns to a Factor Format for MIA
EssentialdtMIA$ShotType <- as.numeric(sapply(EssentialdtMIA$ShotType, as.character))
EssentialdtMIA$MIAFreeThrows <- as.factor(sapply(EssentialdtMIA$MIAFreeThrows, as.integer))
EssentialdtMIA$MIATurnovers <- as.factor(sapply(EssentialdtMIA$MIATurnovers, as.integer))
EssentialdtMIA$MIAAssists <- as.factor(sapply(EssentialdtMIA$MIAAssists, as.integer))
EssentialdtMIA$MIARebounds <- as.factor(sapply(EssentialdtMIA$MIARebounds, as.integer))

# Second Check for MIA
str(EssentialdtMIA)


# Preliminary Variables for MIA

# Total Number of Games played
TemporaryDatasetMIA <- EssentialdtMIA
TemporaryDatasetMIA <- TemporaryDatasetMIA[!duplicated(TemporaryDatasetMIA$Date), ]
totalgamesplayedMIA <- as.numeric(nrow(TemporaryDatasetMIA))

TemporaryDatasetMIA2 <- TemporaryDatasetMIA[TemporaryDatasetMIA$WinningTeam == "MIA",]
totalwinsMIA <- as.numeric(nrow(TemporaryDatasetMIA2))
totallostMIA <- totalgamesplayedMIA - totalwinsMIA

# Independent Variable Analysis

# 1) ShotType

# Creating a frequency table
bins <- seq(0, 5, by=1) #Creates a sequence of Bins separated by 1 unit
ShotType <- cut(EssentialdtMIA$ShotType,bins) #strips individual ShotType values and puts them into appropriate bins
shottypeFrequencytableMIA <- transform(table(ShotType), Percentages=specify_decimal(prop.table(Freq)*100,2)) #creates a data set with frequency distributions and percentages of employees in each age category.
print(shottypeFrequencytableMIA)

# ShotTypes per game
cat(specify_decimal(shottypeFrequencytableMIA[1,2]/totalgamesplayedMIA,2),"2-pt dunks per game.\n")
cat(specify_decimal(shottypeFrequencytableMIA[2,2]/totalgamesplayedMIA,2),"2-pt hook shots per game.\n")
cat(specify_decimal(shottypeFrequencytableMIA[3,2]/totalgamesplayedMIA,2),"2-pt jump shots per game.\n")
cat(specify_decimal(shottypeFrequencytableMIA[4,2]/totalgamesplayedMIA,2),"2-pt layups per game.\n")
cat(specify_decimal(shottypeFrequencytableMIA[5,2]/totalgamesplayedMIA,2),"3-pt jump shots per game.\n")

xtabs(~ WinningTeam + ShotType, data = EssentialdtMIA)


#Logit Analysis Prep
str(EssentialdtMIA)
EssentialdtMIA$ShotType <- as.factor(sapply(EssentialdtMIA$ShotType, as.numeric))

EssentialdtMIA$WinningTeam[which(EssentialdtMIA$WinningTeam != "MIA")] <- as.integer(0)
EssentialdtMIA$WinningTeam[which(EssentialdtMIA$WinningTeam == "MIA")] <- as.integer(1)
EssentialdtMIA$WinningTeam <- as.factor(sapply(EssentialdtMIA$WinningTeam, as.integer))
str(EssentialdtMIA)

#Logit Analysis
EssentialdtMIA$ShotType <- as.factor(sapply(EssentialdtMIA$ShotType, as.numeric))
ShotTypeLogisticModelMIA <- glm(WinningTeam ~ ShotType, data = EssentialdtMIA, family = "binomial")
summary(ShotTypeLogisticModelMIA)
# library(stargazer)
# stargazer(ShotTypeLogisticModelMIA,type="text")
EssentialdtMIA$ShotType <- as.numeric(sapply(EssentialdtMIA$ShotType, as.character))

# 2) FreeThrows
TemporaryDatasetMIA <- EssentialdtMIA
TemporaryDatasetMIA <- TemporaryDatasetMIA[TemporaryDatasetMIA$MIAFreeThrows == 1,]
totalfreethrowsinseasonMIA <- as.numeric(nrow(TemporaryDatasetMIA))
TemporaryDatasetMIA2 <- TemporaryDatasetMIA[TemporaryDatasetMIA$FreeThrowOutcome == "make",]
totalfreethrowsscoredbyMIA <- as.numeric(nrow(TemporaryDatasetMIA2))

cat(specify_decimal(totalfreethrowsscoredbyMIA/totalgamesplayedMIA,2), "Freethrows Scored per Game by MIA.\n")

xtabs(~ WinningTeam + MIAFreeThrows, data = EssentialdtMIA)

FreethrowLogisticModelMIA <- glm(WinningTeam ~ MIAFreeThrows, data = EssentialdtMIA, family = "binomial")
summary(FreethrowLogisticModelMIA)

# 3) Assists
TemporaryDatasetMIA <- EssentialdtMIA
TemporaryDatasetMIA <- TemporaryDatasetMIA[TemporaryDatasetMIA$MIAAssists == 1,]
totalassistsMIA <- as.numeric(nrow(TemporaryDatasetMIA))

cat(specify_decimal(totalassistsMIA/totalgamesplayedMIA,2), "Assists per game by MIA.\n")

xtabs(~ WinningTeam + MIAFreeThrows, data = EssentialdtMIA)

#Logistic Regression of Assists

AssistLogisticModelMIA <- glm(WinningTeam ~ MIAAssists, data = EssentialdtMIA, family = "binomial")
summary(AssistLogisticModelMIA)


# 4) Rebounds
TemporaryDatasetMIA <- EssentialdtMIA
TemporaryDatasetMIA <- TemporaryDatasetMIA %>% drop_na(MIARebounds)
TemporaryDatasetMIA2 <- TemporaryDatasetMIA[TemporaryDatasetMIA$MIARebounds == "1",]
totalOffensiveRebounds <- as.numeric(nrow(TemporaryDatasetMIA2))

cat(specify_decimal(totalOffensiveRebounds/totalgamesplayedMIA,2), "Offensive Rebounds per game by MIA.\n")

#Logistic Regression of Rebounds
ReboundsLogisticModelMIA <- glm(WinningTeam ~ MIARebounds, data = EssentialdtMIA, family = "binomial")
summary(ReboundsLogisticModelMIA)


# 5) TimeOuts
TemporaryDatasetMIA <- EssentialdtMIA
TemporaryDatasetMIA <- TemporaryDatasetMIA[TemporaryDatasetMIA$TimeoutTeam == "MIA",]
totaltimeoutsMIA <- as.numeric(nrow(TemporaryDatasetMIA))

cat(specify_decimal(totaltimeoutsMIA/totalgamesplayedMIA,2), "Timeouts per game by MIA.\n")

xtabs(~ WinningTeam  + TimeoutTeam, data = EssentialdtMIA)

#Logistic Regression of Timeouts
TimeoutLogisticModelMIA <- glm(WinningTeam ~ TimeoutTeam, data = EssentialdtMIA, family = "binomial")
summary(TimeoutLogisticModelMIA)


## ****************************************************************************************************************************

# GSW

dtAwayGSW <- dt[dt$AwayTeam == "GSW",]
dtHomeGSW <- dt[dt$HomeTeam == "GSW",]

dtGSW <- rbind(dtAwayGSW, dtHomeGSW)

EssentialdtGSW <- dtGSW[,c(2,4,9,10,11,12,18,30,32,39,40,41,42)]

# Second Check for GSW
str(EssentialdtGSW)

# Data Clean Up & Wrangling

# Cleans up the Shot type column to a numeric format
EssentialdtGSW[EssentialdtGSW == "2-pt dunk"] <- as.numeric(1)
EssentialdtGSW[EssentialdtGSW == "2-pt hook shot"] <- as.numeric(2)
EssentialdtGSW[EssentialdtGSW == "2-pt jump shot"] <- as.numeric(3)
EssentialdtGSW[EssentialdtGSW == "2-pt layup"] <- as.numeric(4)
EssentialdtGSW[EssentialdtGSW == "3-pt jump shot"] <- as.numeric(5)



#Force columns to a Factor Format for GSW
EssentialdtGSW$ShotType <- as.numeric(sapply(EssentialdtGSW$ShotType, as.character))
EssentialdtGSW$GSWFreeThrows <- as.factor(sapply(EssentialdtGSW$GSWFreeThrows, as.integer))
EssentialdtGSW$GSWTurnovers <- as.factor(sapply(EssentialdtGSW$GSWTurnovers, as.integer))
EssentialdtGSW$GSWAssists <- as.factor(sapply(EssentialdtGSW$GSWAssists, as.integer))
EssentialdtGSW$GSWRebounds <- as.factor(sapply(EssentialdtGSW$GSWRebounds, as.integer))

# Second Check for GSW
str(EssentialdtGSW)


# Preliminary Variables for GSW

# Total Number of Games played
TemporaryDatasetGSW <- EssentialdtGSW
TemporaryDatasetGSW <- TemporaryDatasetGSW[!duplicated(TemporaryDatasetGSW$Date), ]
TemporaryDatasetGSW2 <- TemporaryDatasetGSW[TemporaryDatasetGSW$WinningTeam == "GSW",]

totalgamesplayedGSW <- as.numeric(nrow(TemporaryDatasetGSW))
totalwinsGSW <- as.numeric(nrow(TemporaryDatasetGSW2))
totallostGSW <- totalgamesplayedGSW - totalwinsGSW

# Independent Variable Analysis

# 1) ShotType

# Creating a frequency table
bins <- seq(0, 5, by=1) #Creates a sequence of Bins separated by 1 unit
ShotType <- cut(EssentialdtGSW$ShotType,bins) #strips individual ShotType values and puts them into appropriate bins
shottypeFrequencytableGSW <- transform(table(ShotType), Percentages=specify_decimal(prop.table(Freq)*100,2)) #creates a data set with frequency distributions and percentages of employees in each age category.
print(shottypeFrequencytableGSW)

# ShotTypes per game
cat(specify_decimal(shottypeFrequencytableGSW[1,2]/totalgamesplayedGSW,2),"2-pt dunks per game.\n")
cat(specify_decimal(shottypeFrequencytableGSW[2,2]/totalgamesplayedGSW,2),"2-pt hook shots per game.\n")
cat(specify_decimal(shottypeFrequencytableGSW[3,2]/totalgamesplayedGSW,2),"2-pt jump shots per game.\n")
cat(specify_decimal(shottypeFrequencytableGSW[4,2]/totalgamesplayedGSW,2),"2-pt layups per game.\n")
cat(specify_decimal(shottypeFrequencytableGSW[5,2]/totalgamesplayedGSW,2),"3-pt jump shots per game.\n")

xtabs(~ WinningTeam + ShotType, data = EssentialdtGSW)


#Logit Analysis Prep

str(EssentialdtGSW)
EssentialdtGSW$ShotType <- as.factor(sapply(EssentialdtGSW$ShotType, as.numeric))


# Logistic Regression of ShotTypes

ShotTypeLogisticModel <- glm(WinningTeam ~ ShotType, data = EssentialdtMIA, family = "binomial")
summary(ShotTypeLogisticModel)
EssentialdtGSW$ShotType <- as.numeric(sapply(EssentialdtGSW$ShotType, as.factor))
# library(stargazer)
# stargazer(ShotTypeLogisticModel,type="text")


# 2) FreeThrows
TemporaryDatasetGSW <- EssentialdtGSW
TemporaryDatasetGSW <- TemporaryDatasetGSW[TemporaryDatasetGSW$GSWFreeThrows == 1,]
totalfreethrowsinseasonGSW <- as.numeric(nrow(TemporaryDatasetGSW))
TemporaryDatasetGSW2 <- TemporaryDatasetGSW[TemporaryDatasetGSW$FreeThrowOutcome == "make",]
totalfreethrowsscoredbyGSW <- as.numeric(nrow(TemporaryDatasetGSW2))

cat(specify_decimal(totalfreethrowsscoredbyGSW/totalgamesplayedGSW,2), "Freethrows Scored per Game by GSW.\n")

# 3) Assists
TemporaryDatasetGSW <- EssentialdtGSW
TemporaryDatasetGSW <- TemporaryDatasetGSW[TemporaryDatasetGSW$GSWAssists == 1,]
totalassistsGSW <- as.numeric(nrow(TemporaryDatasetGSW))

cat(specify_decimal(totalassistsGSW/totalgamesplayedGSW,2), "Assists per game by GSW.\n")

# 4) Rebounds
TemporaryDatasetGSW <- EssentialdtGSW
TemporaryDatasetGSW <- TemporaryDatasetGSW %>% drop_na(GSWRebounds)
TemporaryDatasetGSW2 <- TemporaryDatasetGSW[TemporaryDatasetGSW$GSWRebounds == "1",]
totalOffensiveRebounds <- as.numeric(nrow(TemporaryDatasetGSW2))

cat(specify_decimal(totalOffensiveRebounds/totalgamesplayedGSW,2), "Offensive Rebounds per game by GSW.\n")


# 5) TimeOuts
TemporaryDatasetGSW <- EssentialdtGSW
TemporaryDatasetGSW <- TemporaryDatasetGSW[TemporaryDatasetGSW$TimeoutTeam == "GSW",]
totaltimeoutsGSW <- as.numeric(nrow(TemporaryDatasetGSW))

#Generic Value, can be split between games timeouts in games won and lost
cat(specify_decimal(totaltimeoutsGSW/totalgamesplayedGSW,2), "Timeouts per game by GSW.\n")







