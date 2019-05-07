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
# GUYANA
#------------------------------------------------------------------------------#

#-----*-----*-----*-----*-----*-----#
# Load datasets 
#-----*-----*-----*-----*-----*-----#

rm(list = ls(all = TRUE))
country <- "gy"
setwd(paste("~/Desktop/PAHO_Mortality_New/Data/",country,sep=""))
deaths <- read.csv("Data mortality_Guyana _rev081318.csv", as.is = T) # <-- Updated version shared by Cara on Mon, Aug 13, 2018 at 1:55 PM
coverage <- read.csv(paste(country,"_coverage_26Jul2018.csv",sep=""))
hdi <- read.csv(paste(country,"_hdi_26Jul2018.csv",sep="")) # Only one line 

# To merge datasets, column names for key variables should be same 
colnames(deaths)[which(names(deaths) == "Date.of.Birth")] <- "DOB"
colnames(deaths)[which(names(deaths) == "Year.of.Death")] <- "Year"
colnames(deaths)[which(names(deaths) == "Corrected.Date.of.death")] <- "Date.of.death"
colnames(deaths)[which(names(deaths) == "Age.Group")] <- "Age.group"
colnames(hdi)[which(names(hdi) == "year")] <- "Year"
colnames(hdi)[which(names(hdi) == "IDH")] <- "HDI"
colnames(coverage)[which(names(coverage) == "Cobertura.PCV..3rd.dosis..considerando.12.23m.como.denominador...Para.paises.GAVI..12m")] <- "coverage"
colnames(coverage)[which(names(coverage) == "Ano")] <- "Year"

#-----*-----*-----*-----*-----*-----#
# Create Age and Age Group
#-----*-----*-----*-----*-----*-----#

# Age group in the mortality data is:
# 0 if 0-59 days (i.e. 0-<2 months)
# 1 if 3-<12m 
# 2 if 12-<24m
# 3 if 24-<60m
table(deaths$Age.group,useNA = "ifany")

# Reformate date of death to calculate month of death
head(deaths$Date.of.death)
length(which(is.na(deaths$Date.of.death))) # 0
length(which(deaths$Date.of.death=="")) # 190 blanks
deaths$DOD.formatted <- as.Date(deaths$Date.of.death, format="%d-%b-%y")
summary(deaths$DOD.formatted) # 190 NA's

# Check
deaths[101:110,c("Date.of.death","DOD.formatted")]


#-----*-----*-----*-----*-----*-----#
# Other Data Cleaning
#-----*-----*-----*-----*-----*-----#

# Craete month of death from DOD in the mortality data
library(lubridate)
deaths$Month <- month(deaths$DOD.formatted)
class(deaths$Month)
table(deaths$Month)

# Convert coverage from % to numeric
coverage
coverage$coverage <- as.numeric(ifelse(coverage$Year==2011, 0.5, coverage$coverage))
coverage$coverage <- as.numeric(ifelse(coverage$Year==2012 | coverage$Year==2013, 0.9, coverage$coverage))
coverage


#-----*-----*-----*-----*-----*-----#
# Merge Datasets
#-----*-----*-----*-----*-----*-----#

# Year
table(deaths$Year, useNA = "ifany") # 2000-2013
table(hdi$Year, useNA = "ifany") # 2011
table(coverage$Year, useNA = "ifany") # 2011-2013

# Age group
table(deaths$Age.group, useNA = "ifany")
table(hdi$Age.group, useNA = "ifany") # NA 
table(coverage$Age.group, useNA = "ifany") # NA

# Geographic areas
# For Guyana, all analysis will be done at the national level.

# Moratlity+ hdi
deaths$HDI <- hdi$HDI[1]
merge2 <- deaths

# Moratlity + pop + hdi + coverage
merge3 <- merge(merge2, coverage, by = c('Year'), all.x=T)
nrow(deaths) == nrow(merge3) # Should be TRUE

##### Save a merged dataset #####

write.csv(merge3, file=paste(country,"_CovarMerged_15Dec2018.csv",sep=""),row.names=F)


#-----*-----*-----*-----*-----*-----#
# Extract relevant columns
# (Mortality data)
#-----*-----*-----*-----*-----*-----#

names(merge3)

# Change column names
colnames(merge3)[which(names(merge3) == "Cause.of.death")] <- "dx1"

# Extract columns relevant for analysis
merge4 <- subset(merge3, select=c(Year, Month, Age.group, #age_m, 
                                  HDI,coverage,
                                  #area2,
                                  dx1))
merge4$country <- country
merge4$area2<-"AA"
merge4$area3<-NA
merge4$age_m<-NA
merge4$dx2 <- NA
merge4$dx3 <- NA
merge4$dx4 <- NA
merge4$dx5 <- NA
merge4$dx6 <- NA
merge4$Age.group2<-NA

# Check
table(merge4$Year, useNA = "ifany") # 2000-2013. There are no missings (although Month has 190 missings), because Year is the original variable provided by the country. Month is what we created based on DOD, which had 190 missings.
table(merge4$Month, useNA = "ifany") # 1-12. 190 NAs
table(merge4$Age.group, useNA = "ifany")
table(merge4$area2, useNA = "ifany") # All AA, because we only had national-level data.
table(merge4$area3, useNA = "ifany") # NA
length(unique(merge4$dx1)) # 537
length(which(is.na(merge4$dx1)))  # 0
summary(merge4$HDI)
summary(merge4$coverage) # 4155 missings

# Create a final version of .csv file
write.csv(merge4, file=paste(country,"_ForAnalysis_15Dec2018.csv",sep=""),row.names=F)

#-----*-----*-----*-----*-----*-----#
# Extract relevant columns
# (Covariate data)
#-----*-----*-----*-----*-----*-----#

head(hdi)
head(coverage)

# Fix column names
hdi$area2 <- "AA"
coverage$area2 <- "AA"

# Add new columns
hdi$country <- country
coverage$country <- country

# Choose relevant columns
finalhdi <- subset(hdi, select=c(country, area2, HDI))
finalcoverage <- subset(coverage, select=c(country, Year, area2, coverage))

# Save final versions
write.csv(finalhdi,     file=paste(country,"_hdi_26Jul2018_ColNamesCorrected.csv",sep=""),     row.names = F)
write.csv(finalcoverage,file=paste(country,"_coverage_26Jul2018_ColNamesCorrected.csv",sep=""),row.names = F)

