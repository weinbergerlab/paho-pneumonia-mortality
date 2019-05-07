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
# HONDURAS
#------------------------------------------------------------------------------#

#-----*-----*-----*-----*-----*-----#
# Load datasets 
#-----*-----*-----*-----*-----*-----#

setwd("~/Desktop/PAHO_Mortality_NEW/Data/hr")
country <- "hr"
deaths <- read.csv("Mortality_Honduras_May15_2018.csv", as.is = T)
coverage <- read.csv(paste(country,"_coverage_26Jul2018.csv",sep=""))
hdi <- read.csv(paste(country,"_hdi_26Jul2018.csv",sep=""))

# Remove the national-level HDI
hdi <- hdi[-19,]

# To merge datasets, column names for key variables should be same 
colnames(deaths)[which(names(deaths) == "Year.of.death")] <- "Year"
colnames(coverage)[which(names(coverage) == "Cobertura.PCV..3rd.dosis..considerando.12.23m.como.denominador...Para.paises.GAVI..12m.Honduras.")] <- "coverage"
colnames(coverage)[which(names(coverage) == "Codigo.de.depto")] <- "Department.Code"
colnames(coverage)[which(names(coverage) == "Ano")] <- "Year"
colnames(hdi)[which(names(hdi) == "Department.code")] <- "Department.Code"

#-----*-----*-----*-----*-----*-----#
# Age Group 
#-----*-----*-----*-----*-----*-----#

# Honduras does not have DOB, but it has age in hours, days, months, and years 
# all in one column --> Need some cleaning

# First, extract just numbers from this column
head(deaths$Age)
library("stringr")
deaths$Age_num <- as.numeric(gsub("([0-9]+).*$", "\\1", deaths$Age))
summary(deaths$Age_num)

# Second, extract units (e.g., horas, dias, meses, anos)
table(str_extract(deaths$Age, "[A-Z]+" )) # A, DIAS, HORAS, MESES
deaths$Age_unit <- NA
deaths$Age_unit <- ifelse(str_extract(deaths$Age, "[A-Z]+" )=="A",     "y", deaths$Age_unit)
deaths$Age_unit <- ifelse(str_extract(deaths$Age, "[A-Z]+" )=="MESES", "m", deaths$Age_unit)
deaths$Age_unit <- ifelse(str_extract(deaths$Age, "[A-Z]+" )=="DIAS",  "d", deaths$Age_unit)
deaths$Age_unit <- ifelse(str_extract(deaths$Age, "[A-Z]+" )=="HORAS", "h", deaths$Age_unit)
table(deaths$Age_unit,useNA = "ifany")

# Third, based on the age unit, calculate age in days and years
deaths$age_d <- NA
deaths$age_d <- ifelse(deaths$Age_unit=="y", deaths$Age_num*365.25, deaths$age_d)
deaths$age_d <- ifelse(deaths$Age_unit=="m", deaths$Age_num*30.42,  deaths$age_d)
deaths$age_d <- ifelse(deaths$Age_unit=="d", deaths$Age_num,        deaths$age_d)
deaths$age_d <- ifelse(deaths$Age_unit=="h", deaths$Age_num/24,     deaths$age_d)
deaths$age_y <- deaths$age_d/365.25
deaths$age_m <- deaths$age_d/30.42

# Check age_d and age_y
summary(deaths$age_d)
summary(deaths$age_m)
summary(deaths$age_y)
hist(deaths$age_m, col="grey", xlab="Months", main="Age in months, Honduras")
# It seems Honduras doesn't have data on 4-5 yo kids.

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
# Data Cleaning 
#-----*-----*-----*-----*-----*-----#

# HDI
class(hdi$HDI) # factor
summary(hdi$HDI)
hdi$HDI <- as.numeric(as.character(hdi$HDI))
class(hdi$HDI) # numeric
summary(hdi$HDI)

# Department code in the HDI dataset
table(hdi$Department.Code)
class(hdi$Department.Code)
hdi$Department.Code <- as.numeric(as.character(hdi$Department.Code))


#-----*-----*-----*-----*-----*-----#
# Merge Datasets
#-----*-----*-----*-----*-----*-----#

# Year
table(deaths$Year, useNA = "ifany")  # 2007-2016
table(hdi$Year, useNA = "ifany") # 2009
table(coverage$Year, useNA = "ifany") # 2011-2015

# Age group
table(deaths$Age.group, useNA = "ifany") # 0-3
table(hdi$Age.group, useNA = "ifany") # NA
table(coverage$Age.group, useNA = "ifany") # NA

# Geographic areas
length(unique(deaths$Department.Code)) # 18
length(unique(hdi$Department.Code)) # 18
length(unique(coverage$Department.Code)) # 18
table(deaths$Department.Code, useNA = "ifany")
table(hdi$Department.Code, useNA = "ifany")
table(coverage$Department.Code, useNA = "ifany")

# Check that the HDI data are available for all age groups, for all years 
# and for all geographic areas
table(hdi$Year, hdi$Department.Code, useNA = "ifany") # Only 2009

# Check that the coverage data are available for all age groups, for all years 
# and for all geographic areas
table(coverage$Year, coverage$Department.Code, useNA = "ifany") # Only 2011-2015

# Moratlity + hdi
hdi <- subset(hdi, select=-c(Year))
merge2 <- merge(deaths, hdi, by = c('Department.Code'), all.x=T)
nrow(deaths) == nrow(merge2) # Should be TRUE

# Moratlity + pop + hdi + coverage
merge3 <- merge(merge2, coverage, by = c('Department.Code','Year'), all.x=T)
nrow(deaths) == nrow(merge3) # Should be TRUE


##### Save a merged dataset #####

write.csv(merge3, file=paste(country,"_CovarMerged_15Dec2018.csv",sep=""),row.names=F)


#-----*-----*-----*-----*-----*-----#
# Extract relevant columns
#-----*-----*-----*-----*-----*-----#

names(merge3)

# Change column names
colnames(merge3)[which(names(merge3) == "Department.Code")] <- "area2"
colnames(merge3)[which(names(merge3) == "Month.of.death")] <- "Month"
colnames(merge3)[which(names(merge3) == "Cause.of.Death")] <- "dx1"
colnames(merge3)[which(names(merge3) == "X2CAUSA_INTERMEDIA1")] <- "dx2"
colnames(merge3)[which(names(merge3) == "X3CAUSA_INTERMEDIA2")] <- "dx3"
colnames(merge3)[which(names(merge3) == "X1CAUSA_DIRECTA")] <- "dx4"
colnames(merge3)[which(names(merge3) == "X5PRIMER_ESTADO_PATOLOGICO")] <- "dx5"
colnames(merge3)[which(names(merge3) == "X6SEGUNDO_ESTADO_PATOLOGICO")] <- "dx6"


# Extract columns relevant for analysis
merge4 <- subset(merge3, select=c(Year, Month, Age.group, age_m, 
                                  HDI,coverage,
                                  area2,dx1,dx2,dx3,dx4,dx5,dx6))
merge4$country <- country
merge4$area3<-NA
merge4$Age.group2<-NA

# Check
table(merge4$Year, useNA = "ifany") # 2007-2016
table(merge4$Month, useNA = "ifany") # 1-12
table(merge4$Age.group, useNA = "ifany") # 0-3
table(merge4$area2, useNA = "ifany")
table(merge4$area3, useNA = "ifany") # NA
length(unique(merge4$dx1)) # 870
length(which(is.na(merge4$dx1))) # 0
summary(merge4$HDI)
summary(merge4$coverage) # 11971 missings
nrow(merge4[which(merge4$Year<2011),]) + nrow(merge4[which(merge4$Year==2016),]) # 11971. Good

# Create a final version of .csv file
write.csv(merge4, file="hr_ForAnalysis_15Dec2018.csv",row.names=F)


#-----*-----*-----*-----*-----*-----#
# Extract relevant columns
# (Covariate data)
#-----*-----*-----*-----*-----*-----#

head(hdi)
head(coverage)

# Fix column names
colnames(hdi)[which(names(hdi) == "Department.Code")] <- "area2"
colnames(coverage)[which(names(coverage) == "Department.Code")] <- "area2"

# Add new columns
hdi$country <- country
coverage$country <- country

# Choose relevant columns
finalhdi <- subset(hdi, select=c(country, area2, HDI))
finalcoverage <- subset(coverage, select=c(country, Year, area2, coverage))

# Save final versions
write.csv(finalhdi,     file=paste(country,"_hdi_26Jul2018_ColNamesCorrected.csv",sep=""),     row.names = F)
write.csv(finalcoverage,file=paste(country,"_coverage_26Jul2018_ColNamesCorrected.csv",sep=""),row.names = F)
