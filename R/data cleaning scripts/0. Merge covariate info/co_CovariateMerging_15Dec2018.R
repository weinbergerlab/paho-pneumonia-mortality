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
# COLOMBIA
#------------------------------------------------------------------------------#

#-----*-----*-----*-----*-----*-----#
# Load datasets 
#-----*-----*-----*-----*-----*-----#

setwd("~/Desktop/PAHO_Mortality_New/Data/co")
country <- "co"
deaths <- read.csv("Mortality_Colombia_May2018.csv", as.is = T)
coverage <- read.csv(paste(country,"_coverage_12Sep2018.csv",sep=""))
hdi <- read.csv(paste(country,"_hdi_26Jul2018.csv",sep=""))

# To merge datasets, column names for key variables should be same 
names(deaths)
names(hdi)
names(coverage)
colnames(coverage)[which(names(coverage) == "year")] <- "Year"
colnames(hdi)[which(names(hdi) == "year")] <- "Year"
colnames(coverage)[which(names(coverage) == "Cobertura.PCV..3rd.dosis..considerando.12.23m.como.denominador...Para.paises.GAVI..12m")] <- "coverage"
colnames(hdi)[which(names(hdi) == "HDI..2010.")] <- "HDI"


#-----*-----*-----*-----*-----*-----#
# Create Age and Age Group
#-----*-----*-----*-----*-----*-----#

# We agreed to use Age.in.months rather than Age.in.years during the call on July 31, 2018.
# This is because Age.in.years seems to be less accurate due to rounding issue.
summary(deaths$Age.in.years) # 15036 missings
summary(deaths$Age.in.months) # 15036 missing
15036/nrow(deaths)*100 # 12.3%
hist(deaths$Age.in.months, col="grey",xlab = "Months", main="Age in month, Colombia")
deaths[1:10,c("Age.in.years","Age.in.months")]

# Is the missing Age.in.months consistent over time?
missingAge.in.months <- deaths[is.na(deaths$Age.in.months),]
hist(missingAge.in.months$Year, col="grey", main="Number of Missing Age.in.months by year of deaths", xlab="") # Not consistent. Age reporting is improving over time.

# Create age group in the mortality data (Version 1)
deaths$Age.group_DODDOB <- NA
deaths$Age.group_DODDOB <- ifelse(deaths$Age.in.months <2,                              0,   deaths$Age.group_DODDOB)
deaths$Age.group_DODDOB <- ifelse(deaths$Age.in.months >=2  & deaths$Age.in.months <12, 1,   deaths$Age.group_DODDOB)
deaths$Age.group_DODDOB <- ifelse(deaths$Age.in.months >=12 & deaths$Age.in.months <24, 2,   deaths$Age.group_DODDOB)
deaths$Age.group_DODDOB <- ifelse(deaths$Age.in.months >=24 & deaths$Age.in.months <60, 3,   deaths$Age.group_DODDOB)

# Create age group in the mortality data (Version 2)
deaths$Age.group_DODDOB2 <- NA
deaths$Age.group_DODDOB2 <- ifelse(deaths$Age.in.months >=12 & deaths$Age.in.months <24, 2,   deaths$Age.group_DODDOB2)
deaths$Age.group_DODDOB2 <- ifelse(deaths$Age.in.months >=24 & deaths$Age.in.months <60, 3,   deaths$Age.group_DODDOB2)
deaths$Age.group_DODDOB2 <- ifelse(deaths$Age.in.months <1,                              4,   deaths$Age.group_DODDOB2)
deaths$Age.group_DODDOB2 <- ifelse(deaths$Age.in.months >=1  & deaths$Age.in.months <2,  5,   deaths$Age.group_DODDOB2)
deaths$Age.group_DODDOB2 <- ifelse(deaths$Age.in.months >=2  & deaths$Age.in.months <6,  6,   deaths$Age.group_DODDOB2)
deaths$Age.group_DODDOB2 <- ifelse(deaths$Age.in.months >=6  & deaths$Age.in.months <12, 7,   deaths$Age.group_DODDOB2)

# Check
table(deaths$Age.group_DODDOB, useNA = "ifany") # 15036 missings
table(deaths$Age.group_DODDOB2, useNA = "ifany") # 15036 missings

# For 15,036 records, use the original age group variable (Age.group)
# According to "Data Status and PCV Intro_INFO_12May2018.docx",
# Age group is incorrect and should not be used, but we decided to use it for 
# 15,036 records.

# # First, we need to clean up the original age group varibale
# table(deaths$Age.group, useNA = "ifany")
# nrow(deaths[which(is.na(deaths$Age.in.months) & deaths$Age.group=="De 0 a antes de 1 año"),])
# nrow(deaths[which(is.na(deaths$Age.in.months) & deaths$Age.group=="De 01 año"),])
# nrow(deaths[which(is.na(deaths$Age.in.months) & deaths$Age.group=="De 02 año"),])
# nrow(deaths[which(is.na(deaths$Age.in.months) & deaths$Age.group=="De 03 año"),])
# nrow(deaths[which(is.na(deaths$Age.in.months) & deaths$Age.group=="De 04 año"),])
# nrow(deaths[which(is.na(deaths$Age.in.months) & deaths$Age.group=="De 05 año"),])
#
# deaths$Age.group <- ifelse(deaths$Age.group=="De 0 a antes de 1 año",1,deaths$Age.group)
# deaths$Age.group <- ifelse(deaths$Age.group=="De 01 año",2,deaths$Age.group)
# deaths$Age.group <- ifelse(deaths$Age.group=="De 02 años",3,deaths$Age.group)
# deaths$Age.group <- ifelse(deaths$Age.group=="De 03 años",3,deaths$Age.group)
# deaths$Age.group <- ifelse(deaths$Age.group=="De 04 años",3,deaths$Age.group)
# deaths$Age.group <- ifelse(deaths$Age.group=="De 05 años",3,deaths$Age.group)
# table(deaths$Age.group) # Good.
# for (i in 1:nrow(deaths)) {
#   if (is.na(deaths$Age.in.months[i])) {deaths$Age.group_combined[i] <- deaths$Age.group[i]}
#   else                                {deaths$Age.group_combined[i] <- deaths$Age.group_DODDOB[i]}
# }
# table(deaths$Age.group_combined, useNA = "ifany")

# # For Cris, create time series plot for all-cause deaths by age group
# table(deaths$Year, useNA = "ifany")
# i=3
# plot(table(deaths$Year[which(deaths$Age.group==i)]), type="o", lwd=2, pch=16, col="blue",
#      bty="l", ylab="No. of deaths per year", #ylim=c(0,1400),
#      main="Time Series for All-cause Deaths")
# lines(table(deaths$Year[which(deaths$Age.group_DODDOB==i)]), type="o", lwd=2, pch=16, col="red")
# lines(table(deaths$Year[which(deaths$Age.group_combined==i)]), type="o", lwd=2, pch=16, col="black")
# legend(x= "bottomleft", lty=1,pch=16,bty="n",
#        legend = c("Original","Age.in.months","Combined"),
#        col=c("blue","red", "black"))

# # Based on these time series plots, we decided that Age.group_combined works well and we
# # will use Age.group_combined for analysis.
#
# # Drop Age.group and Age.group_DODDOB from the dataset, and rename Age.group_combined
# names(deaths)
# deaths <- subset(deaths, select=-c(Age.group, Age.group_DODDOB))
# colnames(deaths)[which(names(deaths) == "Age.group_combined")] <- "Age.group"

names(deaths)
deaths <- subset(deaths, select=-c(Age.group))
colnames(deaths)[which(names(deaths) == "Age.group_DODDOB")] <- "Age.group"
colnames(deaths)[which(names(deaths) == "Age.group_DODDOB2")] <- "Age.group2"


#-----*-----*-----*-----*-----*-----#
# Data Cleaning 
#-----*-----*-----*-----*-----*-----#

# Comments from Cris  (Sun, Aug 5, 2018 at 7:21 PM)
# HDI data for some years/departments missing (Guainia, Guaviare, Vaupes and Vichada) 
summary(hdi$HDI) # 4 missings
# For these 4 departments, assume and use Amazonia Department IDH as they are 
# similar in regards to other socio-economic indicators.
hdi$HDI <- ifelse(hdi$Department.name=="GUAINIA",  hdi$HDI[which(hdi$Department.name=="AMAZONAS")], hdi$HDI)
hdi$HDI <- ifelse(hdi$Department.name=="GUAVIARE", hdi$HDI[which(hdi$Department.name=="AMAZONAS")], hdi$HDI)
hdi$HDI <- ifelse(hdi$Department.name=="VAUPES",   hdi$HDI[which(hdi$Department.name=="AMAZONAS")], hdi$HDI)
hdi$HDI <- ifelse(hdi$Department.name=="VICHADA",  hdi$HDI[which(hdi$Department.name=="AMAZONAS")], hdi$HDI)
# Check
hdi$HDI[which(hdi$Department.name=="GUAINIA")]
hdi$HDI[which(hdi$Department.name=="GUAVIARE")]
hdi$HDI[which(hdi$Department.name=="VAUPES")]
hdi$HDI[which(hdi$Department.name=="VICHADA")]
summary(hdi$HDI) # No missings



#-----*-----*-----*-----*-----*-----#
# Merge Datasets
#-----*-----*-----*-----*-----*-----#

# Year
table(deaths$Year, useNA = "ifany")  # From 2005 to 2015
table(hdi$Year, useNA = "ifany") # 2011
table(coverage$Year, useNA = "ifany") # From 2011 to 2015
 
# Age group
table(deaths$Age.group, useNA = "ifany")
table(deaths$Age.group2, useNA = "ifany")
table(hdi$Age.group, useNA = "ifany") # NA
table(coverage$Age.group, useNA = "ifany") # NA

# Geographic areas
length(unique(deaths$Department.code)) # 33
length(unique(hdi$Department.code)) # 33
length(unique(coverage$Department.code)) # 33
table(deaths$Department.code, useNA = "ifany")
table(hdi$Department.code, useNA = "ifany")
table(coverage$Department.code, useNA = "ifany")

# Check that the HDI data are available for all age groups, for all years 
# and for all geographic areas
table(hdi$Year, hdi$Department.code, useNA = "ifany") # Only 2010

# Check that the coverage data are available for all age groups, for all years 
# and for all geographic areas
table(coverage$Year, coverage$Department.code, useNA = "ifany") # Only 2012-2015

# Moratlity + pop + hdi
hdi <- subset(hdi, select=-c(Year))
merge2 <- merge(deaths, hdi, by = c('Department.code'), all.x=T)
nrow(deaths) == nrow(merge2) # Should be TRUE

# Moratlity + pop + hdi + coverage
merge3 <- merge(merge2, coverage, by = c('Department.code','Year'), all.x=T)
nrow(deaths) == nrow(merge3) # Should be TRUE

##### Save a merged dataset #####

write.csv(merge3, file="co_CovarMerged_15Dec2018.csv",row.names=F)


#-----*-----*-----*-----*-----*-----#
# Extract relevant columns
# (Mortality data)
#-----*-----*-----*-----*-----*-----#

names(merge3)

# Change column names
colnames(merge3)[which(names(merge3) == "Age.in.months")] <- "age_m"
colnames(merge3)[which(names(merge3) == "Department.code")] <- "area2"
colnames(merge3)[which(names(merge3) == "Cause.of.death")] <- "dx1"
colnames(merge3)[which(names(merge3) == "c_dir1")] <- "dx2"
colnames(merge3)[which(names(merge3) == "c_ant1")] <- "dx3"
colnames(merge3)[which(names(merge3) == "c_ant2")] <- "dx4"
colnames(merge3)[which(names(merge3) == "c_ant3")] <- "dx5"
colnames(merge3)[which(names(merge3) == "c_pat1")] <- "dx6"


# Extract columns relevant for analysis
merge4 <- subset(merge3, select=c(Year, Month, Age.group, Age.group2, age_m,
                                  HDI,coverage,
                                  area2,dx1,dx2,dx3,dx4,dx5,dx6))
merge4$country <- "co"
merge4$area3 <- NA

# Check
table(merge4$Year, useNA = "ifany") # 2005-2015
table(merge4$Month, useNA = "ifany") # 1-12
table(merge4$Age.group, useNA = "ifany") # 0-3
table(merge4$Age.group2, useNA = "ifany") # 2-7
table(merge4$area2, useNA = "ifany")
table(merge4$area3, useNA = "ifany") # NA
length(unique(merge4$dx1)) # 774
length(which(is.na(merge4$dx1))) # 0
summary(merge4$HDI)
summary(merge4$coverage)

# Create a final version of .csv file
write.csv(merge4, file="co_ForAnalysis_15Dec2018.csv",row.names=F)

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

