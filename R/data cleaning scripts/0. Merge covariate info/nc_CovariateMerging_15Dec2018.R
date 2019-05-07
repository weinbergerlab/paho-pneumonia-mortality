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
# NICARAGUA
#------------------------------------------------------------------------------#

#-----*-----*-----*-----*-----*-----#
# Load datasets 
#-----*-----*-----*-----*-----*-----#

rm(list = ls(all = TRUE))
country <- "nc"
setwd(paste("~/Desktop/PAHO_Mortality_New/Data/",country,sep=""))
deaths <- read.csv("Mortality_2001_2015_NIC_Sep2018.csv", as.is = T)
coverage <- read.csv(paste(country,"_coverage_18Sep2018.csv",sep=""))
hdi <- read.csv(paste(country,"_hdi_18Sep2018.csv",sep=""))

# To merge datasets, column names for key variables should be same 
colnames(deaths)[which(names(deaths) == "Gedad")] <- "Age.group"
colnames(deaths)[which(names(deaths) == "Year.of.death")] <- "Year"
colnames(deaths)[which(names(deaths) == "Month.of.death")] <- "Month"
colnames(coverage)[which(names(coverage) == "X3rd.dose.coverage.PCV13")] <- "coverage"
colnames(coverage)[which(names(coverage) == "SILAIs..Departments.")] <- "Department.code"
colnames(hdi)[which(names(hdi) == "Estimated.for.2010..considering.national.country.level.increase.of.8.77..in.the.period.")] <- "HDI"
colnames(hdi)[which(names(hdi) == "Codigo....")] <- "Department.code"

# Remove the national-level estimate from HDI
hdi <- hdi[which(!is.na(hdi$Department.code)),]


#-----*-----*-----*-----*-----*-----#
# Create Age and Age Group
#-----*-----*-----*-----*-----*-----#

#' See "Questions_PAHO mortality_Sept11_2018.docx"
#' 
#' Date of birth has been estimated (not originally available in the database), 
#' so DOD-DOB should not be used. 
#' The most reliable available variables in the dataset are Month and Year of 
#' Death (columns J and K) and Month and Year of Birth (columns H and I). As 
#' such, I suggest you use these 4 columns to recalculate the age groups of 
#' interest, as per below:
#'    Age group 1 (< 1 year) = Ages 0-11 months
#'    Age group 2 (1 to < 2years) = Ages 12-23months
#'    Age group 3 (2 years to <5years) = Ages 24-59 months

summary(deaths$Year) # No missings
summary(deaths$Month) # No missings
summary(deaths$Year.of.birth) # No missings
summary(deaths$Month.of.birth) # No missings

# Create DOB and DOD from these 4 variables
# NOTE: Date is 1st for all records.
deaths$DOB <- as.Date(paste(deaths$Year.of.birth,"-",deaths$Month.of.birth,"-01",sep=""))
deaths$DOD <- as.Date(paste(deaths$Year,"-",deaths$Month,"-01",sep=""))
summary(deaths$DOB)
summary(deaths$DOD)

# Calculate age from newly created DOD and DOB
deaths$age_d <- as.numeric(deaths$DOD-deaths$DOB)
deaths$age_m <- as.numeric(deaths$DOD-deaths$DOB) / 30.44
deaths$age_y <- as.numeric(deaths$DOD-deaths$DOB) / 365.25
summary(deaths$age_d) # No missings
summary(deaths$age_m) # No missings
summary(deaths$age_y) # No missings
hist(deaths$age_m,col="grey",xlab="Months", main="Age in Months", breaks=30) # No missings
abline(v=2,col="red")
hist(deaths$age_d,col="grey",xlab="Days", main="Age in Days", breaks=100) # No missings
abline(v=60,col="red")
unique(deaths$age_d)[order(unique(deaths$age_d))]
unique(deaths$age_m)[order(unique(deaths$age_m))]

# Round it, as we don't have that high resolution... See emails from Dan on Sept 19, 2018
deaths$age_m <- round(deaths$age_m,1)

# Create age group in the mortality data (Version 1)
deaths$Age.group <- NA
deaths$Age.group <- ifelse(deaths$age_m <2,                      0,   deaths$Age.group)
deaths$Age.group <- ifelse(deaths$age_m >=2  & deaths$age_m <12, 1,   deaths$Age.group)
deaths$Age.group <- ifelse(deaths$age_m >=12 & deaths$age_m <24, 2,   deaths$Age.group)
deaths$Age.group <- ifelse(deaths$age_m >=24 & deaths$age_m <=60, 3,   deaths$Age.group)

# Create age group in the mortality data (Version 2)
deaths$Age.group2 <- NA
deaths$Age.group2 <- ifelse(deaths$age_m >=12 & deaths$age_m <24, 2,   deaths$Age.group2)
deaths$Age.group2 <- ifelse(deaths$age_m >=24 & deaths$age_m <=60, 3,   deaths$Age.group2)
deaths$Age.group2 <- ifelse(deaths$age_m <1,                      4,   deaths$Age.group2)
deaths$Age.group2 <- ifelse(deaths$age_m >=1  & deaths$age_m <2,  5,   deaths$Age.group2)
deaths$Age.group2 <- ifelse(deaths$age_m >=2  & deaths$age_m <6,  6,   deaths$Age.group2)
deaths$Age.group2 <- ifelse(deaths$age_m >=6  & deaths$age_m <12, 7,   deaths$Age.group2)

# Check
table(deaths$Age.group, useNA = "ifany") # No missings
table(deaths$Age.group2, useNA = "ifany") # No missings


#-----*-----*-----*-----*-----*-----#
# Other data cleaning
#-----*-----*-----*-----*-----*-----#

##### Department Code #####

# 2 states were generated in 2013-2014

table(deaths$Department.code)
table(hdi$Department.code)
table(coverage$Department.code) # 2 extra depts

#' See Cris' email on Sept 5, 2018
#' Regarding the additional departaments created in 2013 and 2104 in Nicaragua:
#'     - SILAI Atlántico Norte (1) was divided in 2013 generating an additional 
#'       SILAI Las Minas (18)
#'     - SILAI Chontales (6) was divided in 2014 generating an additional SILAI 
#'       Zelaya Central (19)

# --> Thus, we decided to calculate weighted average of the vaccine coverage
#     using population as weight.

# Load population data
setwd(paste("~/Desktop/PAHO_Mortality_New/Data/",country,sep=""))
pop <- read.csv(paste(country,"_pop_18Sep2018.csv",sep=""))
colnames(pop)[which(names(pop) == "SILAIs..Departments.")] <- "Department.code"
colnames(pop)[which(names(pop) == "Age.group.1...12m.")] <- "population.Age1"
colnames(pop)[which(names(pop) == "Age.group.2..12.23m.")] <- "population.Age2"
colnames(pop)[which(names(pop) == "Age.group.3..24.59m.")] <- "population.Age3"

# Clean population data
summary(pop$population.Age1)
summary(pop$population.Age2)
summary(pop$population.Age3)

# Extract each age group
pop1 <- subset(pop, select=c(Year, Department.code, population.Age1))
pop2 <- subset(pop, select=c(Year, Department.code, population.Age2))
pop3 <- subset(pop, select=c(Year, Department.code, population.Age3))

# Create an age group variable
pop1$Age.group <- 1
pop2$Age.group <- 2
pop3$Age.group <- 3

# Rename columns
names(pop1) <- c("Year","Department.code","population","Age.group")
names(pop2) <- c("Year","Department.code","population","Age.group")
names(pop3) <- c("Year","Department.code","population","Age.group")

# Merge them
newpop <- rbind(pop1,pop2,pop3)

# Check
head(newpop)
summary(newpop$population[newpop$Age.group==1]) # Same as above. Good. 
summary(newpop$population[newpop$Age.group==2]) # Same as above. Good. 
summary(newpop$population[newpop$Age.group==3]) # Same as above. Good. 

# Rename the dataset
pop <- newpop


### Coverage data
coverage.orig <- coverage

# Check coverage data in Deparrment 18 and 19
coverage.orig[which(coverage.orig$Department.code==18),]
coverage.orig[which(coverage.orig$Department.code==1 & coverage.orig$Year>=2013),]
coverage.orig[which(coverage.orig$Department.code==19),]
coverage.orig[which(coverage.orig$Department.code==6 & coverage.orig$Year>=2014),]

# Calculate the weighted average of vaccine coverage for Dept 18 and 19
for (i in 2013:2015) {
  pop_dept1 <- sum(pop$population[which(pop$Department.code==1 & pop$Year==i)])
  pop_dept18<- sum(pop$population[which(pop$Department.code==18& pop$Year==i)])
  weight_dept1 <- pop_dept1 /sum(pop_dept1, pop_dept18)
  weight_dept18<- pop_dept18/sum(pop_dept1, pop_dept18)
  coverage$coverage[which(coverage$Department.code==1 & coverage$Year==i)] <- weight_dept1*coverage.orig$coverage[which(coverage.orig$Department.code==1 & coverage.orig$Year==i)] + weight_dept18*coverage.orig$coverage[which(coverage.orig$Department.code==18 & coverage.orig$Year==i)]
}
for (i in 2014:2015) {
  pop_dept6 <- sum(pop$population[which(pop$Department.code==6 & pop$Year==i)])
  pop_dept19<- sum(pop$population[which(pop$Department.code==19& pop$Year==i)])
  weight_dept6 <- pop_dept6 /sum(pop_dept6, pop_dept19)
  weight_dept19<- pop_dept19/sum(pop_dept6, pop_dept19)
  coverage$coverage[which(coverage$Department.code==6 & coverage$Year==i)] <- weight_dept6*coverage.orig$coverage[which(coverage.orig$Department.code==6 & coverage.orig$Year==i)] + weight_dept19*coverage.orig$coverage[which(coverage.orig$Department.code==19 & coverage.orig$Year==i)]
}
# Remove 2 extra departments
coverage <- coverage[which(coverage$Department.code %in% c(1:17)),]

### Popualtion data

# Check population data in Department 18 and 19
pop[which(pop$Department.code==18),]
pop[which(pop$Department.code==1 & pop$Year>=2013),]
pop[which(pop$Department.code==19),]
pop[which(pop$Department.code==6 & pop$Year>=2014),]
# Add population in 2 divided departments (Dept 1 + 18, Dept 6 + 19)
pop$population[which(pop$Department.code==1 & pop$Year>=2013)] <- pop$population[which(pop$Department.code==1 & pop$Year>=2013)] + pop$population[which(pop$Department.code==18)]
pop$population[which(pop$Department.code==6 & pop$Year>=2014)] <- pop$population[which(pop$Department.code==6 & pop$Year>=2014)] + pop$population[which(pop$Department.code==19)]
# Remove 2 extra departments
pop <- pop[which(pop$Department.code %in% c(1:17)),]


#-----*-----*-----*-----*-----*-----#
# Merge Datasets
#-----*-----*-----*-----*-----*-----#

# Year
table(deaths$Year, useNA = "ifany") # 2005-2015
table(hdi$Year, useNA = "ifany") # NA
table(coverage$Year, useNA = "ifany") # 2011-2015

# Age group
table(deaths$Age.group, useNA = "ifany")
table(deaths$Age.group2, useNA = "ifany")
table(hdi$Age.group, useNA = "ifany") # NA
table(coverage$Age.group, useNA = "ifany") # NA

# Geographic areas
length(unique(deaths$Department.code)) # 17
length(unique(pop$Department.code)) # 17
length(unique(hdi$Department.code)) # 17
length(unique(coverage$Department.code)) # 17
table(deaths$Department.code, useNA = "ifany")
table(pop$Department.code, useNA = "ifany")
table(hdi$Department.code, useNA = "ifany")
table(coverage$Department.code, useNA = "ifany")

# Check that the HDI data are available for all age groups, for all years 
# and for all geographic areas
table(hdi$Year, hdi$Department.code, useNA = "ifany") # NA

# Check that the coverage data are available for all age groups, for all years 
# and for all geographic areas
table(coverage$Year, coverage$Department.code, useNA = "ifany") # Only 2011-2015

# Moratlity + pop + hdi
merge2 <- merge(deaths, hdi, by = c('Department.code'), all.x=T)
nrow(deaths) == nrow(merge2) # Should be TRUE

# Moratlity + pop + hdi + coverage
merge3 <- merge(merge2, coverage, by = c('Department.code','Year'), all.x=T)
nrow(deaths) == nrow(merge3) # Should be TRUE


##### Save a merged dataset #####

write.csv(merge3, file="nc_CovarMerged_15Dec2018.csv",row.names=F)


#-----*-----*-----*-----*-----*-----#
# Extract relevant columns
# (Mortality data)
#-----*-----*-----*-----*-----*-----#

names(merge3)

# Change column names
colnames(merge3)[which(names(merge3) == "Department.code")] <- "area2"
colnames(merge3)[which(names(merge3) == "Cause.of.Death")] <- "dx1"

# During the meeting in New Haven in Oct 2018, we decided not to use non-primary
# causes of deaths for Nicaragua, as it started reporting non-primary causes 
# in the last few years. 
# colnames(merge3)[which(names(merge3) == "m_med_sub")] <- "dx2"
# colnames(merge3)[which(names(merge3) == "m_inm_sub")] <- "dx3"

# Extract columns relevant for analysis
merge4 <- subset(merge3, select=c(Year, Month, Age.group, Age.group2, age_m, 
                                  HDI,coverage,
                                  area2,dx1))
merge4$country <- "nc"
merge4$area3<-NA
merge4$dx2<-NA
merge4$dx3<-NA
merge4$dx4<-NA
merge4$dx5<-NA
merge4$dx6<-NA

# Check
table(merge4$Year, useNA = "ifany") # 2005-2015
table(merge4$Month, useNA = "ifany") # 1-12
table(merge4$Age.group, useNA = "ifany") # 0-3
table(merge4$Age.group2, useNA = "ifany") # 2-7
table(merge4$area2, useNA = "ifany") 
table(merge4$area3, useNA = "ifany") # NA
length(unique(merge4$dx1)) # 986
length(which(is.na(merge4$dx1))) # 0
summary(merge4$HDI)
summary(merge4$coverage) # 13317 missings

# Create a final version of .csv file
write.csv(merge4, file="nc_ForAnalysis_15Dec2018.csv",row.names=F)

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


