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
# DOMINICAN REPUBLIC
#------------------------------------------------------------------------------#

#-----*-----*-----*-----*-----*-----#
# Load datasets 
#-----*-----*-----*-----*-----*-----#

rm(list = ls(all = TRUE))
setwd("~/Desktop/PAHO_Mortality_New/Data/dr")
country <- "dr"
deaths <- read.csv("Base MORTALIDAD DominicanaAug2018.2.csv", as.is = T) # Changed the name of column H to "Anos"
coverage <- read.csv(paste(country,"_coverage_21Oct2018.csv",sep=""))
hdi <- read.csv(paste(country,"_hdi_26Jul2018.csv",sep=""))

# To merge datasets, column names for key variables should be same 
colnames(deaths)[which(names(deaths) == "Province.Code")] <- "Province.code"
colnames(coverage)[which(names(coverage) == "Cobertura.PCV..2nd.dosis..considerando.12.23m.como.denominador...Para.paises.GAVI..12m")] <- "coverage"


#-----*-----*-----*-----*-----*-----#
# Create Age and Age Group
#-----*-----*-----*-----*-----*-----#

# Reformate date variables
head(deaths$Date.of.Birth)
head(deaths$Date.of.Death)
deaths$DOB.formatted <- as.Date(deaths$Date.of.Birth, format="%Y/%m/%d")
deaths$DOD.formatted <- as.Date(deaths$Date.of.Death, format="%Y/%m/%d")
# Check
deaths[1:10,c("Date.of.Birth","DOB.formatted","Date.of.Death","DOD.formatted")]
summary(deaths$DOB.formatted) # 30 missings
summary(deaths$DOD.formatted) # No missings

# Calculate age at death in the mortality data
deaths$age_d <- as.numeric(deaths$DOD.formatted-deaths$DOB.formatted)
deaths$age_m <- as.numeric(deaths$DOD.formatted-deaths$DOB.formatted) / 30.44
deaths$age_y <- as.numeric(deaths$DOD.formatted-deaths$DOB.formatted) / 365.25
summary(deaths$age_d) # 30 missings. 10 people were over 5 yo.
summary(deaths$age_m) # 30 missings. 10 people were over 5 yo.
summary(deaths$age_y) # 30 missings. 10 people were over 5 yo.
hist(deaths$age_m,col="grey",xlab="Months", main="Age in Months, DR") 

# Check negative age and >5 yo
length(which(deaths$age_d<0)) # 0
length(which(deaths$age_m>60)) # 10
length(which(deaths$age_m>60))/nrow(deaths)*100 # 0.02%
deaths$age_m[which(deaths$age_m>60)]

# Exclude these 10 observations who are >5 yo from the dataset
nrow(deaths) # 40151
deaths <- deaths[which(deaths$age_m<=60),]
nrow(deaths) # 40111 (=40151 -30 missings -10 over 5yo)

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
table(deaths$Year, useNA = "ifany") # 2005-2015
table(hdi$Year, useNA = "ifany") # None which is okay
table(coverage$Year, useNA = "ifany") # 2013-2015

# Age group
table(deaths$Age.group, useNA = "ifany")
table(deaths$Age.group2, useNA = "ifany")
table(hdi$Age.group, useNA = "ifany") # NA 
table(coverage$Age.group, useNA = "ifany") # NA

# Geographic areas
length(unique(deaths$Province.code)) # 34 (including NA and 99)
length(unique(hdi$Province.code)) # 33 (1 NA which is national-level which will be removed below)
length(unique(coverage$Province.code)) # 32
table(deaths$Province.code, useNA = "ifany")
length(which(is.na(deaths$Province.code))) / nrow(deaths)*100 # 12% missing
length(which(deaths$Province.code==99)) / nrow(deaths)*100 # 0.18% 
table(hdi$Province.code, useNA = "ifany") # 1 NA which is national-level
hdi <- hdi[!is.na(hdi$Province.code),]
table(coverage$Province.code, useNA = "ifany")

# Check that the HDI data are available for all age groups, for all years 
# and for all geographic areas
table(hdi$Year, hdi$Province.code, useNA = "ifany") # None which is okay

# Check that the coverage data are available for all age groups, for all years 
# and for all geographic areas
table(coverage$Year, coverage$Province.code, useNA = "ifany") # Only 2013-2015

# Moratlity + pop + hdi
merge2 <- merge(deaths, hdi, by = c('Province.code'), all.x=T)
nrow(deaths) == nrow(merge2) # Should be TRUE

# Moratlity + pop + hdi + coverage
merge3 <- merge(merge2, coverage, by = c('Province.code','Year'), all.x=T)
nrow(deaths) == nrow(merge3) # Should be TRUE


##### Save a merged dataset #####

write.csv(merge3, file="dr_CovarMerged_15Dec2018.csv",row.names=F)


#-----*-----*-----*-----*-----*-----#
# Extract relevant columns
# (Mortality data)
#-----*-----*-----*-----*-----*-----#

names(merge3)

# Change column names
colnames(merge3)[which(names(merge3) == "Province.code")] <- "area2"
colnames(merge3)[which(names(merge3) == "Cause.of.death")] <- "dx1"

# Extract columns relevant for analysis
merge4 <- subset(merge3, select=c(Year, Month, Age.group, Age.group2, age_m, 
                                  HDI,coverage,
                                  area2,dx1))
merge4$country <- "dr"
merge4$area3 <- NA
merge4$dx2 <- NA
merge4$dx3 <- NA
merge4$dx4 <- NA
merge4$dx5 <- NA
merge4$dx6 <- NA


# Check
table(merge4$Year, useNA = "ifany") # 2005-2015
table(merge4$Month, useNA = "ifany") # 1-12
table(merge4$Age.group, useNA = "ifany") # 0-3
table(merge4$Age.group2, useNA = "ifany") # 2-7
table(merge4$area2, useNA = "ifany")
table(merge4$area3, useNA = "ifany") # NA
length(unique(merge4$dx1)) # 1238
length(which(is.na(merge4$dx1))) # 0
summary(merge4$HDI) # 4937 missings
summary(merge4$coverage) # 31487 missings

# Create a final version of .csv file
write.csv(merge4, file="dr_ForAnalysis_15Dec2018.csv",row.names=F)

#-----*-----*-----*-----*-----*-----#
# Extract relevant columns
# (Covariate data)
#-----*-----*-----*-----*-----*-----#

head(hdi)
head(coverage)

# Fix column names
colnames(hdi)[which(names(hdi) == "Province.code")] <- "area2"
colnames(coverage)[which(names(coverage) == "Province.code")] <- "area2"

# Add new columns
hdi$country <- country
coverage$country <- country

# Choose relevant columns
finalhdi <- subset(hdi, select=c(country, area2, HDI))
finalcoverage <- subset(coverage, select=c(country, Year, area2, coverage))

# Save final versions
write.csv(finalhdi,     file=paste(country,"_hdi_26Jul2018_ColNamesCorrected.csv",sep=""),     row.names = F)
write.csv(finalcoverage,file=paste(country,"_coverage_26Jul2018_ColNamesCorrected.csv",sep=""),row.names = F)
