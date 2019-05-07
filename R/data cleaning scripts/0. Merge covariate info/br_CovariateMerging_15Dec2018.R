################################################################################
# PAHO Mortality Data                                                          #
#                                                                              #
#        DATE: December 2018                                                   #
#    CODED BY: Kayoko Shioda (kayoko.shioda@yale.edu)                          #
#     ADVISOR: Dan Weinberger (daniel.weinberger@yale.edu)                     #
################################################################################

#------------------------------------------------------------------------------#
# DESCRIPTION
#------------------------------------------------------------------------------#

# This code merges covariates (HDI, population, vaccine converage) with 
# PAHO mortality data.

# Original data on mortality and covariate information were downloaded from Dropbox:
# https://www.dropbox.com/sh/yr7iazb80gigvzp/AADibpc5QHGI_NY_LeB7l7G4a?dl=0
# I then created .csv files from these Excel files.


#------------------------------------------------------------------------------#
# BRAZIL
#------------------------------------------------------------------------------#

#-----*-----*-----*-----*-----*-----#
# Load datasets 
#-----*-----*-----*-----*-----*-----#

rm(list = ls(all = TRUE))
country <- "br"
setwd(paste("~/Desktop/PAHO_Mortality_New/Data/",country,sep=""))
deaths <- read.csv("Mortality_Brazil_2005_2015.csv", as.is = T)
coverage <- read.csv(paste(country,"_coverage_26Jul2018.csv",sep=""))
hdi <- read.csv(paste(country,"_hdi_26Jul2018.csv",sep=""))

# To merge datasets, column names for key variables should be same 
colnames(coverage)[which(names(coverage) == "Coverage.PCV..3rd.dose.")] <- "coverage"
colnames(hdi)[which(names(hdi) == "HDI_2010")] <- "HDI"

#-----*-----*-----*-----*-----*-----#
# Create Age and Age Group
#-----*-----*-----*-----*-----*-----#

# Reformate date variables
head(deaths$DOB)
head(deaths$Date.of.Death)
deaths$DOB.formatted <- as.Date(deaths$DOB, format="%Y/%m/%d")
deaths$DOD.formatted <- as.Date(deaths$Date.of.Death, format="%Y/%m/%d")
# Check
deaths[1:10,c("DOB","DOB.formatted","Date.of.Death","DOD.formatted")]
summary(deaths$DOB.formatted) # No missings
summary(deaths$DOD.formatted) # No missings

# Calculate age at death in the mortality data
deaths$age_d <- as.numeric(deaths$DOD.formatted-deaths$DOB.formatted)
deaths$age_m <- as.numeric(deaths$DOD.formatted-deaths$DOB.formatted) / 30.44
deaths$age_y <- as.numeric(deaths$DOD.formatted-deaths$DOB.formatted) / 365.25
summary(deaths$age_d) # No missings but negative ages
summary(deaths$age_m) # No missings but negative ages
summary(deaths$age_y) # No missings but negative ages
hist(deaths$age_d) # No missings
hist(deaths$age_m,col="grey",xlab="Months", main="Age in Months, Brazil") # No missings

# Check negative age and exactly zero-day age
length(which(deaths$age_d<0)) # 23
length(which(deaths$age_d<0))/nrow(deaths)*100 # 0.004%
length(which(deaths$age_d==0))  # 100374
length(which(deaths$age_d==0))/nrow(deaths)*100 # 18.6%

# Exclude these 23 observations from the dataset
nrow(deaths) # 540772
deaths <- deaths[which(deaths$age_d>=0),]
nrow(deaths) # 540749 (=540772-23)

# Create age group in the mortality data (Version 1)
deaths$Age.group <- NA
deaths$Age.group <- ifelse(deaths$age_m <2,                      0,   deaths$Age.group)
deaths$Age.group <- ifelse(deaths$age_m >=2  & deaths$age_m <12, 1,   deaths$Age.group)
deaths$Age.group <- ifelse(deaths$age_m >=12 & deaths$age_m <24, 2,   deaths$Age.group)
deaths$Age.group <- ifelse(deaths$age_m >=24 & deaths$age_m <60, 3,   deaths$Age.group)

# Create age group in the mortality data (Version 2)
deaths$Age.group2 <- NA
deaths$Age.group2 <- ifelse(deaths$age_m >=12 & deaths$age_m <24, 2,   deaths$Age.group2)
deaths$Age.group2 <- ifelse(deaths$age_m >=24 & deaths$age_m <60, 3,   deaths$Age.group2)
deaths$Age.group2 <- ifelse(deaths$age_m <1,                      4,   deaths$Age.group2)
deaths$Age.group2 <- ifelse(deaths$age_m >=1  & deaths$age_m <2,  5,   deaths$Age.group2)
deaths$Age.group2 <- ifelse(deaths$age_m >=2  & deaths$age_m <6,  6,   deaths$Age.group2)
deaths$Age.group2 <- ifelse(deaths$age_m >=6  & deaths$age_m <12, 7,   deaths$Age.group2)

# Check
table(deaths$Age.group, useNA = "ifany") # No missings
table(deaths$Age.group2, useNA = "ifany") # No missings


#-----*-----*-----*-----*-----*-----#
# Other Data Cleaning
#-----*-----*-----*-----*-----*-----#

# Create year of death and month of death in the mortality data
library(lubridate)
deaths$Year <- year(deaths$DOD.formatted)
deaths$Month <- month(deaths$DOD.formatted)
deaths[1:10,c("Year","Month","DOD.formatted")]
summary(deaths$Year) # From 2005 to 2015
summary(deaths$Month) # From 1 to 12


#-----*-----*-----*-----*-----*-----#
# Merge Datasets
#-----*-----*-----*-----*-----*-----#

# Year
table(deaths$Year, useNA = "ifany")  # From 2005 to 2015
table(hdi$Year, useNA = "ifany") # 2010
table(coverage$Year, useNA = "ifany") # From 2010 to 2017

# Age group
table(deaths$Age.group, useNA = "ifany")
table(deaths$Age.group2, useNA = "ifany")
table(hdi$Age.group, useNA = "ifany") # NA 
table(coverage$Age.group, useNA = "ifany") # NA

# Geographic areas
length(unique(deaths$State.Code)) # 27
length(unique(hdi$State.Code)) # 27
length(unique(coverage$State.Code)) # 27
table(deaths$State.Code, useNA = "ifany")
table(hdi$State.Code, useNA = "ifany")
table(coverage$State.Code, useNA = "ifany")

# Check that the HDI data are available for all age groups, for all years 
# and for all geographic areas
table(hdi$Year, hdi$State.Code, useNA = "ifany") # Only 2010

# Check that the coverage data are available for all age groups, for all years 
# and for all geographic areas
table(coverage$Year, coverage$State.Code, useNA = "ifany") # Only 2010-2017

# Moratlity + pop + hdi
hdi <- subset(hdi, select=-c(Year))
merge2 <- merge(deaths, hdi, by = c('State.Code'), all.x=T)
nrow(deaths) == nrow(merge2) # Should be TRUE

# Moratlity + pop + hdi + coverage
merge3 <- merge(merge2, coverage, by = c('State.Code','Year'), all.x=T)
nrow(deaths) == nrow(merge3) # Should be TRUE


##### Save a merged dataset #####

write.csv(merge3, file=paste(country,"_CovarMerged_15Dec2018.csv",sep=""),row.names=F)

#-----*-----*-----*-----*-----*-----#
# Extract relevant columns
# (Mortality data)
#-----*-----*-----*-----*-----*-----#

names(merge3)

# Change column names
colnames(merge3)[which(names(merge3) == "State.Code")] <- "area2"
colnames(merge3)[which(names(merge3) == "Municipality.Code")] <- "area3"
colnames(merge3)[which(names(merge3) == "Cause.of.death")] <- "dx1"
colnames(merge3)[which(names(merge3) == "linhaa")] <- "dx2"
colnames(merge3)[which(names(merge3) == "linhab")] <- "dx3"
colnames(merge3)[which(names(merge3) == "linhac")] <- "dx4"
colnames(merge3)[which(names(merge3) == "linhaii")] <- "dx5"
# Extract columns relevant for analysis
merge4 <- subset(merge3, select=c(Year, Month, Age.group, Age.group2, age_m, 
                                  HDI,coverage,
                                  area2,area3,dx1,dx2,dx3,dx4,dx5))
merge4$country <- "br"
merge4$dx6<-NA

# Check
table(merge4$Year, useNA = "ifany") # 2005-2015
table(merge4$Month, useNA = "ifany") # 1-12
table(merge4$Age.group, useNA = "ifany") # 0-3
table(merge4$Age.group2, useNA = "ifany") # 2-7
table(merge4$area2, useNA = "ifany") 
table(merge4$area3, useNA = "ifany")
length(unique(merge4$dx1)) # 3669
length(which(is.na(merge4$dx1))) # 0
summary(merge4$HDI)
summary(merge4$coverage)

# Create a final version of .csv file
write.csv(merge4, file="br_ForAnalysis_15Dec2018.csv",row.names=F)

#-----*-----*-----*-----*-----*-----#
# Extract relevant columns
# (Covariate data)
#-----*-----*-----*-----*-----*-----#

head(hdi)
head(coverage)

# Fix column names
colnames(hdi)[which(names(hdi) == "State.Code")] <- "area2"
colnames(coverage)[which(names(coverage) == "State.Code")] <- "area2"

# Add new columns
hdi$country <- country
coverage$country <- country

# Choose relevant columns
finalhdi <- subset(hdi, select=c(country, area2, HDI))
finalcoverage <- subset(coverage, select=c(country, Year, area2, coverage))

# Save final versions
write.csv(finalhdi,     file=paste(country,"_hdi_26Jul2018_ColNamesCorrected.csv",sep=""),     row.names = F)
write.csv(finalcoverage,file=paste(country,"_coverage_26Jul2018_ColNamesCorrected.csv",sep=""),row.names = F)



