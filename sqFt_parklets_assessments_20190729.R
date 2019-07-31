###############
## Title: STAE parklet assessements

# Author: Lydia Jessup
# Date: July 15, 2019
# Last modified: July 29th, 2019
# Last modified by: Crystal Penalosa
# Description: Summarizes parklet land values and finds average dollar per square foot amount 
#

###############

#install libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

#set working directory
getwd()
# read locally
setwd("C:/Users/...")



####################################
#  Import Data
####################################

#read in assessors file
parklets <- read.csv('data/parklet-BlockIDs.csv', header=TRUE, sep=",")

assessments <- read.csv('data/2017Assessor_Historical_Secured_Property_Tax_Rolls.csv', header=TRUE, sep=",")


####################################
# Get to know data
####################################

#var names
names(parklets)
names(assessments)

#dimensions
dim(parklets)
dim(assessments)

#any missing?
#check blocks
sum(is.na(assessments$Block))
#check assessment improvement value 
sum(is.na(assessments$Assessed.Improvement.Value))
#check assessment land value
sum(is.na(assessments$Assessed.Land.Value))
#check assessment property area
sum(is.na(assessments$Property.Area))


#none missing, looks good

#max/min
summary(assessments$Assessed.Improvement.Value)
summary(assessments$Assessed.Land.Value)
summary(assessments$Property.Area)


#types
class(assessments$Block)
class(assessments$Assessed.Improvement.Value)
class(assessments$Assessed.Land.Value)
class(assessments$Property.Area)
class(parklets$Block)

#change to character
assessments$Block <- as.character(assessments$Block)
parklets$Block <- as.character(parklets$Block)
parklets$Block <- str_pad(parklets$Block, 4, side = "left", pad = "0")

#how many blocks
length(unique(assessments$Block))
#there are 5,356 blocks

####################################
# Reformat
####################################

#group by parklet

assess_land_sums <- assessments %>%
  group_by(Block) %>%
  summarize(total_land_value_2017 = sum(Assessed.Land.Value))

assess_area_sums <- assessments %>%
  group_by(Block) %>%
  summarize(total_property_area_2017 = sum(Property.Area))

assess_sums <- merge(assess_land_sums, assess_area_sums, by = "Block", all.x = TRUE)

#add in year variable
assess_sums$year <- 2017

#double checked that a few summed correctly

####################################
# Merge with parklets
####################################

#add parklet dummy
parklets$parklet <- 1

#now merge
assess_merge  <- merge(assess_sums, parklets, by = "Block", all.x = TRUE)
head(assess_merge)

#drop duplicates
assess_merge <- assess_merge[!duplicated(assess_merge),]

#fill in nas
assess_merge$parklet[is.na(assess_merge$parklet)] <- 0  
head(assess_merge)


####################################
#  Save file
####################################

write.csv(assess_merge, "output/sqft_assessments2017.csv")


###Filter for Parklets, save, then reload this file
landValue <- read.csv('output/sqft_assessments2017.csv')

#take summary of land value/assessments, take mean
summary(landValue$total_property_area_2017)
summary(landValue$total_land_value_2017)

