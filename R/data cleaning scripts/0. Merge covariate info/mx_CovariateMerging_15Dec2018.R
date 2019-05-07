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
# MEXICO
#------------------------------------------------------------------------------#

#-----*-----*-----*-----*-----*-----#
# Load datasets 
#-----*-----*-----*-----*-----*-----#

rm(list = ls(all = TRUE))
country <- "mx"
setwd(paste("~/Desktop/PAHO_Mortality_New/Data/",country,sep=""))
# deaths <- read.csv("Defunciones_Mex2002_2016.csv", as.is = T) # <-- Old version
deaths <- read.csv("Deaths_Mex2000_2016_updated081318.csv", as.is = T) # <-- Updated version shared by Cara on Mon, Aug 13, 2018 at 1:55 PM
coverage <- read.csv(paste(country,"_coverage_26Jul2018.csv",sep=""))
hdi <- read.csv(paste(country,"_hdi_26Jul2018.csv",sep=""))

# Correct HDI data (See Cara's email on Thu, Aug 9, 2018 at 2:08 PM)
hdi <- hdi[-31,] # Remove one of two rows for Yucatán

# To merge datasets, column names for key variables should be same 
colnames(deaths)[which(names(deaths) == "AGE.GROUP")] <- "Age.group"
colnames(deaths)[which(names(deaths) == "STATE.CODE")] <- "State.Code"
colnames(deaths)[which(names(deaths) == "MES_OCURR")] <- "Month"
colnames(deaths)[which(names(deaths) == "ANIO_OCUR")] <- "Year"
colnames(coverage)[which(names(coverage) == "Coverage..total.3rd.dose.total.eligible.children.")] <- "coverage"


#-----*-----*-----*-----*-----*-----#
# Create Age and Age Group
#-----*-----*-----*-----*-----*-----#

# Age group is:
# 0 if 0-59 days (i.e. 0-<2 months)
# 1 if 3-<12m 
# 2 if 12-<24m
# 3 if 24-<60m
table(deaths$Age.group,useNA = "ifany")
deaths$Age.group <- ifelse(deaths$Age.group=="G0",0,deaths$Age.group)
deaths$Age.group <- ifelse(deaths$Age.group=="G1",1,deaths$Age.group)
deaths$Age.group <- ifelse(deaths$Age.group=="G2",2,deaths$Age.group)
deaths$Age.group <- ifelse(deaths$Age.group=="G3",3,deaths$Age.group)



#-----*-----*-----*-----*-----*-----#
# Other Data Cleaning
#-----*-----*-----*-----*-----*-----#

# Convert State.Code from character to numeric in the mortality data
class(deaths$State.Code)
table(deaths$State.Code)
deaths$State.Code <- as.numeric(deaths$State.Code) # 518 ND --> missing
table(deaths$State.Code,useNA = "ifany") # 518 missings
518 / nrow(deaths) * 100 # 0.083%

# Convert month of death from character to numeric in the mortality data
class(deaths$Month)
table(deaths$Month)
deaths$Month <- as.numeric(deaths$Month) # 86 ND --> missing
table(deaths$Month,useNA = "ifany") # 86 missings
86 / nrow(deaths) * 100 # 0.0138%


#-----*-----*-----*-----*-----*-----#
# Merge Datasets
#-----*-----*-----*-----*-----*-----#

# Year
table(deaths$Year, useNA = "ifany") # 2000-2016
table(hdi$Year, useNA = "ifany") # 2008
table(coverage$Year, useNA = "ifany") # 2008-2016

# Age group
table(deaths$Age.group, useNA = "ifany") # 0-3
table(hdi$Age.group, useNA = "ifany") # NA 
table(coverage$Age.group, useNA = "ifany") # NA

# Geographic areas
length(unique(deaths$State.Code)) # 33 including NA (n=518)
length(unique(hdi$State.Code)) # 32
length(unique(coverage$State.Code)) # 32
table(deaths$State.Code, useNA = "ifany")
table(hdi$State.Code, useNA = "ifany")
table(coverage$State.Code, useNA = "ifany")

# Check that the HDI data are available for all age groups, for all years 
# and for all geographic areas
table(hdi$Year, hdi$State.Code, useNA = "ifany") # Only 2008

# Check that the coverage data are available for all age groups, for all years 
# and for all geographic areas
table(coverage$Year, coverage$State.Code, useNA = "ifany") # Only 2008-2016

# Moratlity + hdi
hdi <- subset(hdi, select=-c(Year))
merge2 <- merge(deaths, hdi, by = c('State.Code'), all.x=T)
nrow(deaths) == nrow(merge2) # Should be TRUE

# Moratlity + hdi + coverage
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
colnames(merge3)[which(names(merge3) == "CAUSE.DEATH")] <- "dx1"

# Extract columns relevant for analysis
merge4 <- subset(merge3, select=c(Year, Month, Age.group, #age_m, 
                                  HDI,coverage,
                                  area2,dx1))
merge4$country <- country
merge4$area3<-NA
merge4$age_m<-NA
merge4$dx2 <- NA
merge4$dx3 <- NA
merge4$dx4 <- NA
merge4$dx5 <- NA
merge4$dx6 <- NA
merge4$Age.group2<-NA

# Check
table(merge4$Year, useNA = "ifany") # 2000-2016
table(merge4$Month, useNA = "ifany") # 1-12. 86 missings
table(merge4$Age.group, useNA = "ifany") # 0-3
table(merge4$area2, useNA = "ifany") # 518 missings
#table(merge4$area3, useNA = "ifany")
length(unique(merge4$dx1)) # 3266
length(which(is.na(merge4$dx1))) # 0
summary(merge4$HDI) # 518 missings
summary(merge4$coverage) # 323454 missings


# Create a final version of .csv file
write.csv(merge4, file=paste(country,"_ForAnalysis_15Dec2018.csv",sep=""),row.names=F)

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



