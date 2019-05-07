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
# PERU
#------------------------------------------------------------------------------#

#-----*-----*-----*-----*-----*-----#
# Load datasets 
#-----*-----*-----*-----*-----*-----#

rm(list = ls(all = TRUE))
country <- "pr"
setwd(paste("~/Desktop/PAHO_Mortality_New/Data/",country,sep=""))
deaths <- read.csv("Mortality_2005-2014_PERU_18Sep2018.csv", as.is = T)
coverage <- read.csv(paste(country,"_coverage_18Sep2018.csv",sep=""))
hdi <- read.csv(paste(country,"_hdi_18Sep2018.csv",sep=""))

nrow(deaths) # Should be 68207

# To merge datasets, column names for key variables should be same 
colnames(deaths)[which(names(deaths) == "Year.of.death")] <- "Year"
colnames(deaths)[which(names(deaths) == "Month.of.death")] <- "Month"
colnames(coverage)[which(names(coverage) == "Cobertura.PCV..3rd.dosis..considerando.12.23m.como.denominador...Para.paises.GAVI..12m")] <- "coverage"
colnames(coverage)[which(names(coverage) == "year")] <- "Year"


#-----*-----*-----*-----*-----*-----#
# Create Age and Age Group
#-----*-----*-----*-----*-----*-----#

#' See "Questions_PAHO mortality_Sept11_2018.docx" from Cris:
#' 
#' Response from national team: “the date of birth was not available in the 
#' original databse and was estimated considering date of death and age at death. 
#' As age was not exact (only in full years, months or days) this may have 
#' generated incorrect/approximated dates of birth”. Therefore DOD-DOB not to be 
#' used.
#' As such, I recommend, doing what was done for Argentina (ie., consider a 
#' combination of the variables Unit of Age and Age to generate ages for the 
#' analysis), considering:
#' Reliable variables in the dataset to be considered has been further highlighted:
#'     Unit of Age - column L (1=Year, 2=Month, 3=Day, 4=Hours, 9=NA)
#'     Age – column K (as per unit described in unit of age variable)

table(deaths$Unit.of.Age.variable) # 1 (years), 2 (months), 3 (days), 4 (hours)
summary(deaths$Age)  # 1-31. No missings

# Based on the age unit (Unit.of.Age.variable), calculate age in days, months, 
# and years.
# Unit.of.Age.variable is 1=Year, 2=Month, 3=Day, 4=Hours.
deaths$age_d <- NA
deaths$age_d <- ifelse(deaths$Unit.of.Age.variable==1, deaths$Age*365.25, deaths$age_d)
deaths$age_d <- ifelse(deaths$Unit.of.Age.variable==2, deaths$Age*30.42,  deaths$age_d)
deaths$age_d <- ifelse(deaths$Unit.of.Age.variable==3, deaths$Age,        deaths$age_d)
deaths$age_d <- ifelse(deaths$Unit.of.Age.variable==4, deaths$Age/24,     deaths$age_d)
deaths$age_m <- deaths$age_d/30.42
deaths$age_y <- deaths$age_d/365.25

# Check age_d and age_y
summary(deaths$age_d)
summary(deaths$age_m)
summary(deaths$age_y)
hist(deaths$age_m, col="grey", xlab="Months", main="Age in months, Peru")

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
# Merge Datasets
#-----*-----*-----*-----*-----*-----#

# Year
table(deaths$Year, useNA = "ifany") # 2005-2014
table(hdi$Year, useNA = "ifany") # NA
table(coverage$Year, useNA = "ifany") # 2008-2014

# Age group
table(deaths$Age.group, useNA = "ifany") # 0-3
table(deaths$Age.group2, useNA = "ifany") # 2-7
table(hdi$Age.group, useNA = "ifany") # NA
table(coverage$Age.group, useNA = "ifany") # NA

# Geographic areas
length(unique(deaths$Department.code)) # 25
length(unique(hdi$Department.code)) # 25
length(unique(coverage$Department.code)) # 25
table(deaths$Department.code, useNA = "ifany")
table(hdi$Department.code, useNA = "ifany")
table(coverage$Department.code, useNA = "ifany")

# Check that the HDI data are available for all age groups, for all years 
# and for all geographic areas
table(hdi$Year, hdi$Department.code, useNA = "ifany") # NA

# Check that the coverage data are available for all age groups, for all years 
# and for all geographic areas
table(coverage$Year, coverage$Department.code, useNA = "ifany") # Only 2008-2014

# Moratlity + hdi
merge2 <- merge(deaths, hdi, by = c('Department.code'), all.x=T)
nrow(deaths) == nrow(merge2) # Should be TRUE

# Moratlity + hdi + coverage
merge3 <- merge(merge2, coverage, by = c('Department.code','Year'), all.x=T)
nrow(deaths) == nrow(merge3) # Should be TRUE


##### Save a merged dataset #####

write.csv(merge3, file="pr_CovarMerged_15Dec2018.csv",row.names=F)


#-----*-----*-----*-----*-----*-----#
# Extract relevant columns
# (Mortality data)
#-----*-----*-----*-----*-----*-----#

names(merge3)

# Change column names
colnames(merge3)[which(names(merge3) == "Department.code")] <- "area2"
colnames(merge3)[which(names(merge3) == "Cause.of.Death")] <- "dx1"
colnames(merge3)[which(names(merge3) == "m_med_sub_1")] <- "dx2"
colnames(merge3)[which(names(merge3) == "m_inm_sub")] <- "dx3"

# Extract columns relevant for analysis
merge4 <- subset(merge3, select=c(Year, Month, Age.group, Age.group2, age_m, 
                                  HDI,coverage,
                                  area2,dx1,dx2,dx3))
merge4$country <- "pr"
merge4$area3 <- NA
merge4$dx4 <- NA
merge4$dx5 <- NA
merge4$dx6 <- NA

# Check
table(merge4$Year, useNA = "ifany")
table(merge4$Month, useNA = "ifany") # 28 missings
table(merge4$Age.group, useNA = "ifany") # 0-3. No missings
table(merge4$Age.group2, useNA = "ifany") # 2-7. No missings
table(merge4$area2, useNA = "ifany") # No missings
table(merge4$area3, useNA = "ifany") # NA
length(unique(merge4$dx1)) # 2066
length(which(is.na(merge4$dx1))) # 0
summary(merge4$HDI)
summary(merge4$coverage) # 23796 missings

# Create a final version of .csv file
write.csv(merge4, file="pr_ForAnalysis_15Dec2018.csv",row.names=F)

#-----*-----*-----*-----*-----*-----#
# Extract relevant columns
# (Covariate data)
#-----*-----*-----*-----*-----*-----#

head(hdi)
head(coverage)

# Fix column names
colnames(hdi)[which(names(hdi) == "Department.code")] <- "area2"
colnames(coverage)[which(names(coverage) == "Department.code")] <- "area2"

# Add new columns
hdi$country <- country
coverage$country <- country

# Choose relevant columns
finalhdi <- subset(hdi, select=c(country, area2, HDI))
finalcoverage <- subset(coverage, select=c(country, Year, area2, coverage))

# Save final versions
write.csv(finalhdi,     file=paste(country,"_hdi_26Jul2018_ColNamesCorrected.csv",sep=""),     row.names = F)
write.csv(finalcoverage,file=paste(country,"_coverage_26Jul2018_ColNamesCorrected.csv",sep=""),row.names = F)

