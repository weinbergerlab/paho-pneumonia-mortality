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
# ECUADOR
#------------------------------------------------------------------------------#

#-----*-----*-----*-----*-----*-----#
# Load datasets 
#-----*-----*-----*-----*-----*-----#

rm(list = ls(all = TRUE))
setwd("~/Desktop/PAHO_Mortality_NEW/Data/ec")
country <- "ec"
deaths <- read.csv("Mortality_data_Ecuador_2005_16_May2018_ColumnsDeleted.csv", as.is = T)
coverage <- read.csv(paste(country,"_coverage_26Jul2018.csv",sep=""))
hdi <- read.csv(paste(country,"_hdi_26Jul2018.csv",sep=""))

# To merge datasets, column names for key variables should be same 
colnames(deaths)[which(names(deaths) == "Province.Code")] <- "Province.code"
colnames(hdi)[which(names(hdi) == "year")] <- "Year"
colnames(coverage)[which(names(coverage) == "Cobertura.PCV..3rd.dosis..considerando.12.23m.como.denominador...Para.paises.GAVI..12m")] <- "coverage"
colnames(coverage)[which(names(coverage) == "year")] <- "Year"

#-----*-----*-----*-----*-----*-----#
# Age Group 
#-----*-----*-----*-----*-----*-----#

# Reformate date variables
head(deaths$DOB)
head(deaths$Date.of.death)
length(which(is.na(deaths$DOB))) # 0
length(which(is.na(deaths$Date.of.death))) # 0
length(which(deaths$DOB=="#NULL!")) # 18
deaths$DOB.formatted <- as.Date(deaths$DOB, format="%d-%b-%y")
deaths$DOD.formatted <- as.Date(deaths$Date.of.death, format="%d-%b-%y")
deaths[100:110,c("DOB","DOB.formatted","Date.of.death","DOD.formatted")]
summary(deaths$DOD.formatted) # Some of them are happening in the future (e.g., 2068)
summary(deaths$DOB.formatted) # Some of them are happening in the future (e.g., 2068) 18 missings, because their DOBs were #NULL!
deaths[which(deaths$DOD.formatted > Sys.Date()),c("DOB","DOB.formatted","Date.of.death","DOD.formatted")]

# Correct century (e.g., 2068 --> 1968)
deaths$DOD.formatted <- as.Date(ifelse(deaths$DOD.formatted > Sys.Date(), 
                                       format(deaths$DOD.formatted, "19%y-%m-%d"), 
                                       format(deaths$DOD.formatted)))
deaths$DOB.formatted <- as.Date(ifelse(deaths$DOB.formatted > Sys.Date(), 
                                       format(deaths$DOB.formatted, "19%y-%m-%d"), 
                                       format(deaths$DOB.formatted)))
summary(deaths$DOD.formatted) # All records are before the current date. Good. No missings
summary(deaths$DOB.formatted) # All records are before the current date. Good. 18 missings
length(which(is.na(deaths$DOB.formatted))) / nrow(deaths)*100 # 0.03%

# Calculate age at death in the mortality data
deaths$age_d <- as.numeric(deaths$DOD.formatted-deaths$DOB.formatted)
deaths$age_m <- as.numeric(deaths$DOD.formatted-deaths$DOB.formatted) / 30.42
deaths$age_y <- as.numeric(deaths$DOD.formatted-deaths$DOB.formatted) / 365.25
summary(deaths$age_d) # 18 missings
summary(deaths$age_m) # 18 missings
summary(deaths$age_y) # 18 missings
deaths[which(is.na(deaths$age_d)),] # DOB was "#NULL!" for these 18 records
18/nrow(deaths)*100 # 0.03%
hist(deaths$age_m, col="grey", xlab="Months", main="Age in months, Ecuador")

# age_m exceeding 60 
length(which(deaths$age_m>60))
deaths[which(deaths$age_m>60),] # They are just rounding issues. Do not exclude them.

# Create age group in the mortality data (Version 1)
deaths$Age.group <- NA
deaths$Age.group <- ifelse(deaths$age_m <2,                      0,   deaths$Age.group)
deaths$Age.group <- ifelse(deaths$age_m >=2  & deaths$age_m <12, 1,   deaths$Age.group)
deaths$Age.group <- ifelse(deaths$age_m >=12 & deaths$age_m <24, 2,   deaths$Age.group)
deaths$Age.group <- ifelse(deaths$age_m >=24 & deaths$age_m <61, 3,   deaths$Age.group)

# Create age group in the mortality data (Version 2)
deaths$Age.group2 <- NA
deaths$Age.group2 <- ifelse(deaths$age_m >=12 & deaths$age_m <24, 2,   deaths$Age.group2)
deaths$Age.group2 <- ifelse(deaths$age_m >=24 & deaths$age_m <61, 3,   deaths$Age.group2)
deaths$Age.group2 <- ifelse(deaths$age_m <1,                      4,   deaths$Age.group2)
deaths$Age.group2 <- ifelse(deaths$age_m >=1  & deaths$age_m <2,  5,   deaths$Age.group2)
deaths$Age.group2 <- ifelse(deaths$age_m >=2  & deaths$age_m <6,  6,   deaths$Age.group2)
deaths$Age.group2 <- ifelse(deaths$age_m >=6  & deaths$age_m <12, 7,   deaths$Age.group2)

# Check
table(deaths$Age.group, useNA = "ifany") # 18 missings
table(deaths$Age.group2, useNA = "ifany") # 18 missings


#-----*-----*-----*-----*-----*-----#
# Data Cleaning 
#-----*-----*-----*-----*-----*-----#

# Create "Year" of deaths in the mortality data
library(lubridate)
deaths$Year <- year(deaths$DOD.formatted)
deaths[20001:20010,c("Year","DOD.formatted")]

# Year in the mortality data
table(deaths$Year, useNA = "ifany") # Some observations bbefore 2005. Remove them.
length(which(deaths$Year<2005))
length(which(deaths$Year<2005))/nrow(deaths)*100 # 0.44%
deaths <- deaths[which(deaths$Year >= 2005),] # Remove records before 2005
table(deaths$Year, useNA = "ifany") 

# Create "Month" of deaths in the mortality data
library(lubridate)
deaths$Month <- month(deaths$DOD.formatted)
deaths[20001:20010,c("Month","DOD.formatted")]

# HDI
class(hdi$HDI) # factor
table(hdi$HDI)
hdi$HDI <- as.numeric(as.character(hdi$HDI))
table(hdi$HDI, useNA="ifany") # 1 NA
class(hdi$HDI) # numeric
summary(hdi$HDI) # 1 NA

# HDI for Galápagos
hdi$HDI <- ifelse(hdi$Province.name=="20. Galápagos", hdi$HDI[hdi$Province.name=="17. Pichincha"], hdi$HDI)

#-----*-----*-----*-----*-----*-----#
# Merge Datasets
#-----*-----*-----*-----*-----*-----#

# Year
table(deaths$Year, useNA = "ifany") # 2005-2016
table(hdi$Year, useNA = "ifany") # 2010
table(coverage$Year, useNA = "ifany") # 2010-2015

# Age group
table(deaths$Age.group, useNA = "ifany")
table(deaths$Age.group2, useNA = "ifany")
table(hdi$Age.group, useNA = "ifany") # NA
table(coverage$Age.group, useNA = "ifany") # NA

# Geographic areas
length(unique(deaths$Province.code)) # 26, including 88 and 90, which will be removed later
length(unique(hdi$Province.code)) # 24
length(unique(coverage$Province.code)) # 24
table(deaths$Province.code, useNA = "ifany")
table(hdi$Province.code, useNA = "ifany")
table(coverage$Province.code, useNA = "ifany")

# Check that the HDI data are available for all age groups, for all years 
# and for all geographic areas
table(hdi$Year, hdi$Province.code, useNA = "ifany") # Only 2010

# Check that the coverage data are available for all age groups, for all years 
# and for all geographic areas
table(coverage$Year, coverage$Province.code, useNA = "ifany") # Only 2012-2015

# Moratlity + pop + hdi
hdi <- subset(hdi, select=-c(Year))
merge2 <- merge(deaths, hdi, by = c('Province.code'), all.x=T)
nrow(deaths) == nrow(merge2) # Should be TRUE

# Moratlity + pop + hdi + coverage
merge3 <- merge(merge2, coverage, by = c('Province.code','Year'), all.x=T)
nrow(deaths) == nrow(merge3) # Should be TRUE


##### Save a merged dataset #####

write.csv(merge3, file=paste(country,"_CovarMerged_15Dec2018.csv",sep=""),row.names=F)


#-----*-----*-----*-----*-----*-----#
# Extract relevant columns
#-----*-----*-----*-----*-----*-----#

names(merge3)

# Change column names
colnames(merge3)[which(names(merge3) == "Province.code")] <- "area2"
colnames(merge3)[which(names(merge3) == "Cause.of.death")] <- "dx1"

# Extract columns relevant for analysis
merge4 <- subset(merge3, select=c(Year, Month, Age.group, Age.group2, age_m, 
                                  HDI,coverage,
                                  area2,dx1))
merge4$country <- country
merge4$dx2 <- NA
merge4$dx3 <- NA
merge4$dx4 <- NA
merge4$dx5 <- NA
merge4$dx6 <- NA
merge4$area3 <- NA

# Check
table(merge4$Year, useNA = "ifany") # 2005-2016
table(merge4$Month, useNA = "ifany") # 1-12
table(merge4$Age.group, useNA = "ifany") # 0-3. 18 missings
table(merge4$Age.group2, useNA = "ifany") # 2-7. 18 missings
table(merge4$area2, useNA = "ifany")
table(merge4$area3, useNA = "ifany") # NA
length(unique(merge4$dx1)) # 687
length(which(is.na(merge4$dx1))) # 0
summary(merge4$HDI) # 52 NAs
table(merge4$area2[which(is.na(merge4$HDI))]) # province 88 and 90, which will be removed from analysis later
summary(merge4$coverage) # 28018 NAs
nrow(merge4[which(merge4$Year<2010),]) + nrow(merge4[which(merge4$Year==2016),]) # 28009. I think others are province 88 and 90   

# Create a final version of .csv file
write.csv(merge4, file="ec_ForAnalysis_15Dec2018.csv",row.names=F)


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
