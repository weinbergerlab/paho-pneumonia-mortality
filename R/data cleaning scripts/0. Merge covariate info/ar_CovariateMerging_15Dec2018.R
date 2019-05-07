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
# ARGENTINA
#------------------------------------------------------------------------------#

#-----*-----*-----*-----*-----*-----#
# Load .csv files 
#-----*-----*-----*-----*-----*-----#

rm(list = ls(all = TRUE))
country <- "ar"
setwd(paste("~/Desktop/PAHO_Mortality_New/Data/",country,sep=""))
deaths <- read.csv("Mortality_2005_2015_ARG.csv", as.is = T)
coverage <- read.csv(paste(country,"_coverage_26Jul2018.csv",sep=""))
hdi <- read.csv(paste(country,"_hdi_26Jul2018.csv",sep=""))

# To merge datasets, column names for key variables should be same 
colnames(deaths)[which(names(deaths) == "Province.Code")] <- "Province.code"
colnames(deaths)[which(names(deaths) == "Age.groups")] <- "Age.group_original"
colnames(coverage)[5] <- "coverage"

#-----*-----*-----*-----*-----*-----#
# Create Age and Age Group
#-----*-----*-----*-----*-----*-----#

# According to the Word document "Data Status and PCV Intro_INFO_12May2018.docx",
# age group is <1, 1, and 2-4 yrs
table(deaths$Age.group_original,useNA = "ifany")

# On August 5, 2018, we decided to use a combination of Unit of Age (uniedad) and 
# Age (Edad) to generate ages for the analysis.
# uniedad is 1=Year, 2=Month, 3=Day, 4=Hours, 9=NA.
# See an email from Cris on Sun, Aug 5, 2018 at 7:21 PM
table(deaths$uniedad) # 1=Year, 2=Month, 3=Day, 4=Hours
summary(deaths$Edad) 

# Based on the age unit (uniedad), calculate age in days, months, and years.
# uniedad is 1=Year, 2=Month, 3=Day, 4=Hours.
deaths$age_d <- NA
deaths$age_d <- ifelse(deaths$uniedad==1, deaths$Edad*365.25, deaths$age_d)
deaths$age_d <- ifelse(deaths$uniedad==2, deaths$Edad*30.42,  deaths$age_d)
deaths$age_d <- ifelse(deaths$uniedad==3, deaths$Edad,        deaths$age_d)
deaths$age_d <- ifelse(deaths$uniedad==4, deaths$Edad/24,     deaths$age_d)
deaths$age_m <- deaths$age_d/30.42
deaths$age_y <- deaths$age_d/365.25

# Check
summary(deaths$age_d)
summary(deaths$age_m)
summary(deaths$age_y)
hist(deaths$age_m, col="grey", xlab="Months", main="Age in months, Argentina")

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
table(deaths$Age.group, useNA = "ifany")
table(deaths$Age.group2, useNA = "ifany")
table(deaths$Age.group_original, useNA = "ifany") # Age.group and Age.group_original are exactly same. They match. Good.


#-----*-----*-----*-----*-----*-----#
# Merge Datasets
#-----*-----*-----*-----*-----*-----#

# Year
table(deaths$Year, useNA = "ifany") # From 2005 to 2015
table(hdi$Year, useNA = "ifany") # 2011
table(coverage$Year, useNA = "ifany") # From 2012 to 2015

# Age group
table(deaths$Age.group, useNA = "ifany")
table(deaths$Age.group2, useNA = "ifany")
table(hdi$Age.group, useNA = "ifany")
table(coverage$Age.group, useNA = "ifany")

# Geographic areas
length(unique(deaths$Province.code)) # 26 <-- Only the mortality dataset has 98 and 99 which we decided to remove later
length(unique(hdi$Province.code)) # 24
length(unique(coverage$Province.code)) # 24
table(deaths$Province.code, useNA = "ifany")
table(hdi$Province.code, useNA = "ifany")
table(coverage$Province.code, useNA = "ifany")

# Check that the HDI data are available for all age groups, for all years 
# and for all geographic areas (Other than provine 98 and 99)
table(hdi$Year, hdi$Province.code, useNA = "ifany") # Only 2011

# Check that the coverage data are available for all age groups, for all years 
# and for all geographic areas (Other than provine 98 and 99)
table(coverage$Year, coverage$Province.code, useNA = "ifany") # Only 2012-2015

# Moratlity + hdi
hdi <- subset(hdi, select=-c(Year))
merge2 <- merge(deaths, hdi, by = c('Province.code'), all.x=T)
nrow(deaths) == nrow(merge2) # Should be TRUE

# Moratlity + pop + hdi + coverage
merge3 <- merge(merge2, coverage, by = c('Province.code','Year'), all.x=T)
nrow(deaths) == nrow(merge3) # Should be TRUE


##### Save a merged dataset #####

write.csv(merge3, file="ar_CovarMerged_15Dec2018.csv",row.names=F)


#-----*-----*-----*-----*-----*-----#
# Extract relevant columns
# (Mortality data)
#-----*-----*-----*-----*-----*-----#

names(merge3)

# Change column names
colnames(merge3)[which(names(merge3) == "Dep_Res")] <- "area3"
colnames(merge3)[which(names(merge3) == "Province.code")] <- "area2"
colnames(merge3)[which(names(merge3) == "Main.Cause.of.Death")] <- "dx1"

# Extract columns relevant for analysis
merge4 <- subset(merge3, select=c(Year, Month, Age.group, Age.group2, age_m, 
                                  HDI,coverage,
                                  area2,area3,dx1))
merge4$country <- "ar"

merge4$dx2 <- NA
merge4$dx3 <- NA
merge4$dx4 <- NA
merge4$dx5 <- NA
merge4$dx6 <- NA


# Check
table(merge4$Year, useNA = "ifany") # Should be 2005 to 2015
table(merge4$Month, useNA = "ifany") # Should be 1 to 12
table(merge4$Age.group, useNA = "ifany") # Should be 0, 1, 2, 3
table(merge4$Age.group2, useNA = "ifany") # Should be 2, 3, 4, 5, 6, 7
table(merge4$area2, useNA = "ifany") 
table(merge4$area3, useNA = "ifany")
length(unique(merge4$dx1)) # 1875
length(which(is.na(merge4$dx1))) # 0
summary(merge4$HDI)
summary(merge4$coverage)

# Create a final version of .csv file
write.csv(merge4, file="ar_ForAnalysis_15Dec2018.csv",row.names=F)


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


