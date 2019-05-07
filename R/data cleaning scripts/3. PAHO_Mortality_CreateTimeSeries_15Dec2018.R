################################################################################
# PAHO Mortality Data                                                          #
#                                                                              #
#        DATE: December 2018                                                     #
#    CODED BY: Kayoko Shioda (kayoko.shioda@yale.edu)                          #
#     ADVISOR: Dan Weinberger (daniel.weinberger@yale.edu)                     #
################################################################################

#------------------------------------------------------------------------------#
# DESCRIPTION
#------------------------------------------------------------------------------#

# This script creates time series data for PAHO mortality data by HDI level 
# (Low, Med, High) and age group.


#------------------------------------------------------------------------------#
# Set up
#------------------------------------------------------------------------------#

rm(list = ls(all = TRUE))
d0 <- read.csv("~/Desktop/PAHO_Mortality_New/Data/PAHO_10countries_IndividualLevel_15Dec2018_ICD10reformatted_subchapters.csv", as.is = T)
table(d0$country, useNA='ifany') # ar, br, ..., pr
table(d0$country, d0$area2, useNA='ifany')
table(d0$area2, useNA = "ifany") # 5383 missings
table(d0$Age.group, useNA = "ifany") # 15054 missings
table(d0$Age.group2, useNA = "ifany") # 666631 missings
names(d0)


#------------------------------------------------------------------------------#
# Create functions
#------------------------------------------------------------------------------#

# Create time series data stratified by country, Age.group, HDI level, and monthdate
FUN_Monthly_ByHDI_AgeGroup1 <- function (dat) {
  for (i in 1:length(vec_countries)) {
    # Load a dataset for country i
    cntry <- vec_countries[i]
    hdi <- read.csv(paste("~/Desktop/PAHO_Mortality_NEW/Data/",cntry,"/",cntry,"_hdi_26Jul2018_ColNamesCorrected.csv",sep=""))

    # Merge the population and HDI with the monthly time series
    cntrydata <- dat[which(dat$country==cntry),]
    cntrydata <- subset(cntrydata, select=-c(Age.group2)) # There are two age group variables, but Age.group will be used here.
    merge2 <- merge(cntrydata, hdi, by = c("country","area2"), all.x=T)
    
    # Create HDI caterogires: Low, mediun, high
    merge2$HDIcat <- ifelse(merge2$HDI>=0   & merge2$HDI <0.7, 0, NA)            # low, very low
    merge2$HDIcat <- ifelse(merge2$HDI>=0.7 & merge2$HDI <0.8, 1, merge2$HDIcat) # medium
    merge2$HDIcat <- ifelse(merge2$HDI>=0.8 & merge2$HDI<=1.0, 2, merge2$HDIcat) # high, very high

    # Remove the following variables 
    merge3 <- subset(merge2, select=-c(area2, HDI))
    
    # Aggregate by country, age group, HDI level, and monthdate
    mergelist[[i]] <- aggregate(. ~ country + Age.group + HDIcat + monthdate, data = merge3, sum, na.action = na.pass)
  }
  
  # Create one big dataset with all countries
  MonthlyTScovar <- do.call(rbind, mergelist)
  
  # Create age_group (e.g., ar_1_0)
  MonthlyTScovar$age_group <- paste(MonthlyTScovar$country,MonthlyTScovar$Age.group,MonthlyTScovar$HDIcat,sep="_")
  
  # Remve the following variables
  MonthlyTScovar <- subset(MonthlyTScovar, select=-c(country, HDIcat, Age.group))
  
  # Reorder columns
  final <- data.frame(age_group=MonthlyTScovar$age_group, MonthlyTScovar[,1:(ncol(MonthlyTScovar)-1)])
  return(final)
} 

# Create time series data stratified by country, Age.group2, HDI level, and monthdate
FUN_Monthly_ByHDI_AgeGroup2 <- function (dat) {
  for (i in 1:length(vec_countries)) {
    # Load a dataset for country i
    cntry <- vec_countries[i]
    hdi <- read.csv(paste("~/Desktop/PAHO_Mortality_NEW/Data/",cntry,"/",cntry,"_hdi_26Jul2018_ColNamesCorrected.csv",sep=""))
    
    # Merge the population and HDI with the monthly time series
    cntrydata <- dat[which(dat$country==cntry),]
    cntrydata <- subset(cntrydata, select=-c(Age.group)) # There are two age group variables, but Age.group2 will be used here.
    merge2 <- merge(cntrydata, hdi, by = c("country","area2"), all.x=T)
    
    # Create HDI caterogires: Low, mediun, high
    merge2$HDIcat <- ifelse(merge2$HDI>=0   & merge2$HDI <0.7, 0, NA)            # low, very low
    merge2$HDIcat <- ifelse(merge2$HDI>=0.7 & merge2$HDI <0.8, 1, merge2$HDIcat) # medium
    merge2$HDIcat <- ifelse(merge2$HDI>=0.8 & merge2$HDI<=1.0, 2, merge2$HDIcat) # high, very high
    
    # Remove the following variables 
    merge3 <- subset(merge2, select=-c(area2, HDI))
    
    # Aggregate by country, age group, HDI level, and monthdate
    mergelist[[i]] <- aggregate(. ~ country + Age.group2 + HDIcat + monthdate, data = merge3, sum, na.action = na.pass)
  }
  
  # Create one big dataset with all countries
  MonthlyTScovar <- do.call(rbind, mergelist)
  
  # Create age_group (e.g., ar_1_0)
  MonthlyTScovar$age_group <- paste(MonthlyTScovar$country,MonthlyTScovar$Age.group2,MonthlyTScovar$HDIcat,sep="_")
  
  # Remve the following variables
  MonthlyTScovar <- subset(MonthlyTScovar, select=-c(country, HDIcat, Age.group2))
  
  # Reorder columns
  final <- data.frame(age_group=MonthlyTScovar$age_group, MonthlyTScovar[,1:(ncol(MonthlyTScovar)-1)])
  return(final)
} 


# Create time series data stratified by country, Age.group, HDI level, and quarterdate
FUN_Quarter_ByHDI_AgeGroup1 <- function (dat) {
  for (i in 1:length(vec_countries)) {
    # Load a dataset for country i
    cntry <- vec_countries[i]
    hdi <- read.csv(paste("~/Desktop/PAHO_Mortality_NEW/Data/",cntry,"/",cntry,"_hdi_26Jul2018_ColNamesCorrected.csv",sep=""))
    
    # Merge the population and HDI with the monthly time series
    cntrydata <- dat[which(dat$country==cntry),]
    cntrydata <- subset(cntrydata, select=-c(Age.group2)) # There are two age group variables, but Age.group will be used here.
    merge2 <- merge(cntrydata, hdi, by = c("country","area2"), all.x=T)
    
    # Create HDI caterogires: Low, mediun, high
    merge2$HDIcat <- ifelse(merge2$HDI>=0   & merge2$HDI <0.7, 0, NA)            # low, very low
    merge2$HDIcat <- ifelse(merge2$HDI>=0.7 & merge2$HDI <0.8, 1, merge2$HDIcat) # medium
    merge2$HDIcat <- ifelse(merge2$HDI>=0.8 & merge2$HDI<=1.0, 2, merge2$HDIcat) # high, very high
    
    # Remove the following variables 
    merge3 <- subset(merge2, select=-c(area2, HDI))
    
    # Aggregate by country, age group, HDI level, and monthdate
    mergelist[[i]] <- aggregate(. ~ country + Age.group + HDIcat + quarterdate, data = merge3, sum, na.action = na.pass)
  }
  
  # Create one big dataset with all countries
  QuarterTScovar <- do.call(rbind, mergelist)
  
  # Create age_group (e.g., ar_1_0)
  QuarterTScovar$age_group <- paste(QuarterTScovar$country,QuarterTScovar$Age.group,QuarterTScovar$HDIcat,sep="_")
  
  # Remve the following variables
  QuarterTScovar <- subset(QuarterTScovar, select=-c(country, HDIcat, Age.group))
  
  # Reorder columns
  final <- data.frame(age_group=QuarterTScovar$age_group, QuarterTScovar[,1:(ncol(QuarterTScovar)-1)])
  return(final)
} 



# Create time series data stratified by country, Age.group, HDI level, and quarterdate
FUN_Quarter_ByHDI_AgeGroup2 <- function (dat) {
  for (i in 1:length(vec_countries)) {
    # Load a dataset for country i
    cntry <- vec_countries[i]
    hdi <- read.csv(paste("~/Desktop/PAHO_Mortality_NEW/Data/",cntry,"/",cntry,"_hdi_26Jul2018_ColNamesCorrected.csv",sep=""))
    
    # Merge the population and HDI with the monthly time series
    cntrydata <- dat[which(dat$country==cntry),]
    cntrydata <- subset(cntrydata, select=-c(Age.group)) # There are two age group variables, but Age.group2 will be used here.
    merge2 <- merge(cntrydata, hdi, by = c("country","area2"), all.x=T)
    
    # Create HDI caterogires: Low, mediun, high
    merge2$HDIcat <- ifelse(merge2$HDI>=0   & merge2$HDI <0.7, 0, NA)            # low, very low
    merge2$HDIcat <- ifelse(merge2$HDI>=0.7 & merge2$HDI <0.8, 1, merge2$HDIcat) # medium
    merge2$HDIcat <- ifelse(merge2$HDI>=0.8 & merge2$HDI<=1.0, 2, merge2$HDIcat) # high, very high
    
    # Remove the following variables 
    merge3 <- subset(merge2, select=-c(area2, HDI))
    
    # Aggregate by country, age group, HDI level, and monthdate
    mergelist[[i]] <- aggregate(. ~ country + Age.group2 + HDIcat + quarterdate, data = merge3, sum, na.action = na.pass)
  }
  
  # Create one big dataset with all countries
  QuarterTScovar <- do.call(rbind, mergelist)
  
  # Create age_group (e.g., ar_1_0)
  QuarterTScovar$age_group <- paste(QuarterTScovar$country,QuarterTScovar$Age.group2,QuarterTScovar$HDIcat,sep="_")
  
  # Remve the following variables
  QuarterTScovar <- subset(QuarterTScovar, select=-c(country, HDIcat, Age.group2))
  
  # Reorder columns
  final <- data.frame(age_group=QuarterTScovar$age_group, QuarterTScovar[,1:(ncol(QuarterTScovar)-1)])
  return(final)
} 


# Create time series data stratified by country, Age.group, HDI level, coverage, and monthdate
FUN_Monthly_ByHDIandCov_AgeGroup1 <- function (dat) {
  for (i in 1:length(vec_countries)) {
    # Load a dataset for country i
    cntry <- vec_countries[i]
    hdi <- read.csv(paste("~/Desktop/PAHO_Mortality_NEW/Data/",cntry,"/",cntry,"_hdi_26Jul2018_ColNamesCorrected.csv",sep=""))
    coverage <- read.csv(paste("~/Desktop/PAHO_Mortality_NEW/Data/",cntry,"/",cntry,"_coverage_26Jul2018_ColNamesCorrected.csv",sep=""))

    # If coverage is >100%, set them to 100%.
    coverage$coverage <- ifelse(coverage$coverage>100, 100, coverage$coverage)
    
    # Select years to include in the average calculation
    # (from 2 yrs after introduction to the end)
    if (cntry %in% c("nc","pr")) {
      cov2yrs <- coverage[which(coverage$Year %in% unique(coverage$Year)[-c(1:3)]),] # Excluding the first 3 years
    } else {
      cov2yrs <- coverage[which(coverage$Year %in% unique(coverage$Year)[-c(1:2)]),] # Excluding the first 2 years
    }

    # Calculate average vaccine converage in these selected years
    avgcov <- aggregate(cov2yrs$coverage ~ cov2yrs$area2,FUN=mean)
    colnames(avgcov) <- c("area2","coverage")
    avgcov$country <- cntry
    print(hist(avgcov$coverage, main=paste(cntry), col="grey", xlab="Average coverage in each area",
               ylab="Number of areas", xlim=c(0,100), breaks=c(seq(0,100,2.5))))
    abline(v=80, col="red")
    
    # Merge the population and coverage data with the monthly time series
    #dat <- MonthlyTS_area2 # <-- This is to check if the for loop works & fix bugs. Deactivate this line when you are running the whole code to create datasets.
    cntrydata <- dat[which(dat$country==cntry),]
    cntrydata <- subset(cntrydata, select=-c(Age.group2)) # There are two age group variables, but Age.group will be used here.
    merge2 <- merge(cntrydata, avgcov, by = c("country","area2"), all.x=T)
    merge3 <- merge(merge2,    hdi,    by = c("country","area2"), all.x=T)
    
    # Create HDI caterogires: Low, mediun, high
    merge3$HDIcat <- ifelse(merge3$HDI>=0    & merge3$HDI <0.71,  0, NA) # low
    merge3$HDIcat <- ifelse(merge3$HDI>=0.71 & merge3$HDI <=1,    1, merge3$HDIcat) # med
    
    # Create coverage categories: Low, high
    merge3$covlevel <- ifelse(merge3$coverage>=0  & merge3$coverage <85,  0, NA) # low
    merge3$covlevel <- ifelse(merge3$coverage>=85 & merge3$coverage<=100, 1, merge3$covlevel) # high
    
    # Remove the following variables
    merge4 <- subset(merge3, select=-c(area2, coverage, HDI))
    
    # Aggregate data by country, age group, covlevel, HDIcat, and monthdate
    mergelist[[i]] <- aggregate(. ~ country + Age.group + covlevel + HDIcat + monthdate, data = merge4, sum, na.action = na.pass)
  }
  
  # Create one big dataset for all countries
  MonthlyTScovar <- do.call(rbind, mergelist)
  # Create age_group
  MonthlyTScovar$age_group <- paste(MonthlyTScovar$country,MonthlyTScovar$Age.group,MonthlyTScovar$covlevel,MonthlyTScovar$HDIcat,sep="_")
  # Remove the following variables
  MonthlyTScovar <- subset(MonthlyTScovar, select=-c(country, covlevel, HDIcat, Age.group))
  # Reorder colomns
  final <- data.frame(age_group=MonthlyTScovar$age_group, MonthlyTScovar[,1:(ncol(MonthlyTScovar)-1)])
  return(final)
} 

# Create time series data stratified by country, Age.group, HDI level, coverage, and monthdate
FUN_Monthly_ByHDIandCov_AgeGroup2 <- function (dat) {
  for (i in 1:length(vec_countries)) {
    # Load a dataset for country i
    cntry <- vec_countries[i]
    hdi <- read.csv(paste("~/Desktop/PAHO_Mortality_NEW/Data/",cntry,"/",cntry,"_hdi_26Jul2018_ColNamesCorrected.csv",sep=""))
    coverage <- read.csv(paste("~/Desktop/PAHO_Mortality_NEW/Data/",cntry,"/",cntry,"_coverage_26Jul2018_ColNamesCorrected.csv",sep=""))
    
    # If coverage is >100%, set them to 100%.
    coverage$coverage <- ifelse(coverage$coverage>100, 100, coverage$coverage)
    
    # Select years to include in the average calculation
    # (from 2 yrs after introduction to the end)
    if (cntry %in% c("nc","pr")) {
      cov2yrs <- coverage[which(coverage$Year %in% unique(coverage$Year)[-c(1:3)]),] # Excluding the first 3 years
    } else {
      cov2yrs <- coverage[which(coverage$Year %in% unique(coverage$Year)[-c(1:2)]),] # Excluding the first 2 years
    }
    
    # Calculate average vaccine converage in these selected years
    avgcov <- aggregate(cov2yrs$coverage ~ cov2yrs$area2,FUN=mean)
    colnames(avgcov) <- c("area2","coverage")
    avgcov$country <- cntry
    print(hist(avgcov$coverage, main=paste(cntry), col="grey", xlab="Average coverage in each area",
               ylab="Number of areas", xlim=c(0,100), breaks=c(seq(0,100,2.5))))
    abline(v=80, col="red")
    
    # Merge the population and coverage data with the monthly time series
    #dat <- MonthlyTS_area2 # <-- This is to check if the for loop works & fix bugs. Deactivate this line when you are running the whole code to create datasets.
    cntrydata <- dat[which(dat$country==cntry),]
    cntrydata <- subset(cntrydata, select=-c(Age.group)) # There are two age group variables, but Age.group2 will be used here.
    merge2 <- merge(cntrydata, avgcov, by = c("country","area2"), all.x=T)
    merge3 <- merge(merge2,    hdi,    by = c("country","area2"), all.x=T)
    
    # Create HDI caterogires: Low, mediun, high
    merge3$HDIcat <- ifelse(merge3$HDI>=0    & merge3$HDI <0.71,  0, NA) # low
    merge3$HDIcat <- ifelse(merge3$HDI>=0.71 & merge3$HDI <=1,    1, merge3$HDIcat) # med
    
    # Create coverage categories: Low, high
    merge3$covlevel <- ifelse(merge3$coverage>=0  & merge3$coverage <85,  0, NA) # low
    merge3$covlevel <- ifelse(merge3$coverage>=85 & merge3$coverage<=100, 1, merge3$covlevel) # high
    
    # Remove the following variables
    merge4 <- subset(merge3, select=-c(area2, coverage, HDI))
    
    # Aggregate data by country, age group, covlevel, HDIcat, and monthdate
    mergelist[[i]] <- aggregate(. ~ country + Age.group2 + covlevel + HDIcat + monthdate, data = merge4, sum, na.action = na.pass)
  }
  
  # Create one big dataset for all countries
  MonthlyTScovar <- do.call(rbind, mergelist)
  # Create age_group
  MonthlyTScovar$age_group <- paste(MonthlyTScovar$country,MonthlyTScovar$Age.group2,MonthlyTScovar$covlevel,MonthlyTScovar$HDIcat,sep="_")
  # Remove the following variables
  MonthlyTScovar <- subset(MonthlyTScovar, select=-c(country, covlevel, HDIcat, Age.group2))
  # Reorder colomns
  final <- data.frame(age_group=MonthlyTScovar$age_group, MonthlyTScovar[,1:(ncol(MonthlyTScovar)-1)])
  return(final)
} 


# Create time series data stratified by country, Age.group, coverage, and monthdate
FUN_Monthly_ByCov_AgeGroup1 <- function (dat) {
  for (i in 1:length(vec_countries)) {
    # Load a dataset for country i
    cntry <- vec_countries[i]
    coverage <- read.csv(paste("~/Desktop/PAHO_Mortality_NEW/Data/",cntry,"/",cntry,"_coverage_26Jul2018_ColNamesCorrected.csv",sep=""))
    
    # If coverage is >100%, set them to 100%.
    coverage$coverage <- ifelse(coverage$coverage>100, 100, coverage$coverage)
    
    # Select years to include in the average calculation
    # (from 2 yrs after introduction to the end)
    if (cntry %in% c("nc","pr")) {
      cov2yrs <- coverage[which(coverage$Year %in% unique(coverage$Year)[-c(1:3)]),] # Excluding the first 3 years
    } else {
      cov2yrs <- coverage[which(coverage$Year %in% unique(coverage$Year)[-c(1:2)]),] # Excluding the first 2 years
    }
    
    # Calculate average vaccine converage in these selected years
    avgcov <- aggregate(cov2yrs$coverage ~ cov2yrs$area2,FUN=mean)
    colnames(avgcov) <- c("area2","coverage")
    avgcov$country <- cntry
    print(hist(avgcov$coverage, main=paste(cntry), col="grey", xlab="Average coverage in each area",
               ylab="Number of areas", xlim=c(0,100), breaks=c(seq(0,100,2.5))))
    abline(v=80, col="red")
    
    # Merge the population and coverage data with the monthly time series
    #dat <- MonthlyTS_area2 # <-- This is to check if the for loop works & fix bugs. Deactivate this line when you are running the whole code to create datasets.
    cntrydata <- dat[which(dat$country==cntry),]
    cntrydata <- subset(cntrydata, select=-c(Age.group2)) # There are two age group variables, but Age.group will be used here.
    merge3 <- merge(cntrydata, avgcov, by = c("country","area2"), all.x=T)
    
    # Create coverage categories: Low, high
    merge3$covlevel <- ifelse(merge3$coverage>=0  & merge3$coverage <85,  0, NA) # low
    merge3$covlevel <- ifelse(merge3$coverage>=85 & merge3$coverage<=100, 1, merge3$covlevel) # high
    
    # Remove the following variables
    merge4 <- subset(merge3, select=-c(area2, coverage))
    
    # Aggregate data by country, age group, covlevel, and monthdate
    mergelist[[i]] <- aggregate(. ~ country + Age.group + covlevel + monthdate, data = merge4, sum, na.action = na.pass)
  }
  
  # Create one big dataset for all countries
  MonthlyTScovar <- do.call(rbind, mergelist)
  # Create age_group
  MonthlyTScovar$age_group <- paste(MonthlyTScovar$country,MonthlyTScovar$Age.group,MonthlyTScovar$covlevel,sep="_")
  # Remove the following variables
  MonthlyTScovar <- subset(MonthlyTScovar, select=-c(country, covlevel, Age.group))
  # Reorder colomns
  final <- data.frame(age_group=MonthlyTScovar$age_group, MonthlyTScovar[,1:(ncol(MonthlyTScovar)-1)])
  return(final)
} 


# Create time series data stratified by country, Age.group2, coverage, and monthdate
FUN_Monthly_ByCov_AgeGroup2 <- function (dat) {
  for (i in 1:length(vec_countries)) {
    # Load a dataset for country i
    cntry <- vec_countries[i]
    coverage <- read.csv(paste("~/Desktop/PAHO_Mortality_NEW/Data/",cntry,"/",cntry,"_coverage_26Jul2018_ColNamesCorrected.csv",sep=""))
    
    # If coverage is >100%, set them to 100%.
    coverage$coverage <- ifelse(coverage$coverage>100, 100, coverage$coverage)
    
    # Select years to include in the average calculation
    # (from 2 yrs after introduction to the end)
    if (cntry %in% c("nc","pr")) {
      cov2yrs <- coverage[which(coverage$Year %in% unique(coverage$Year)[-c(1:3)]),] # Excluding the first 3 years
    } else {
      cov2yrs <- coverage[which(coverage$Year %in% unique(coverage$Year)[-c(1:2)]),] # Excluding the first 2 years
    }
    
    # Calculate average vaccine converage in these selected years
    avgcov <- aggregate(cov2yrs$coverage ~ cov2yrs$area2,FUN=mean)
    colnames(avgcov) <- c("area2","coverage")
    avgcov$country <- cntry
    print(hist(avgcov$coverage, main=paste(cntry), col="grey", xlab="Average coverage in each area",
               ylab="Number of areas", xlim=c(0,100), breaks=c(seq(0,100,2.5))))
    abline(v=80, col="red")
    
    # Merge the population and coverage data with the monthly time series
    #dat <- MonthlyTS_area2 # <-- This is to check if the for loop works & fix bugs. Deactivate this line when you are running the whole code to create datasets.
    cntrydata <- dat[which(dat$country==cntry),]
    cntrydata <- subset(cntrydata, select=-c(Age.group)) # There are two age group variables, but Age.group2 will be used here.
    merge3 <- merge(cntrydata, avgcov, by = c("country","area2"), all.x=T)
    
    # Create coverage categories: Low, high
    merge3$covlevel <- ifelse(merge3$coverage>=0  & merge3$coverage <85,  0, NA) # low
    merge3$covlevel <- ifelse(merge3$coverage>=85 & merge3$coverage<=100, 1, merge3$covlevel) # high
    
    # Remove the following variables
    merge4 <- subset(merge3, select=-c(area2, coverage))
    
    # Aggregate data by country, age group, covlevel, and monthdate
    mergelist[[i]] <- aggregate(. ~ country + Age.group2 + covlevel + monthdate, data = merge4, sum, na.action = na.pass)
  }
  
  # Create one big dataset for all countries
  MonthlyTScovar <- do.call(rbind, mergelist)
  # Create age_group
  MonthlyTScovar$age_group <- paste(MonthlyTScovar$country,MonthlyTScovar$Age.group2,MonthlyTScovar$covlevel,sep="_")
  # Remove the following variables
  MonthlyTScovar <- subset(MonthlyTScovar, select=-c(country, covlevel, Age.group2))
  # Reorder colomns
  final <- data.frame(age_group=MonthlyTScovar$age_group, MonthlyTScovar[,1:(ncol(MonthlyTScovar)-1)])
  return(final)
} 




# Create time series data stratified by country, Age.group, coverage, and quarterdate
FUN_Quarter_ByCov_AgeGroup1 <- function (dat) {
  for (i in 1:length(vec_countries)) {
    # Load a dataset for country i
    cntry <- vec_countries[i]
    coverage <- read.csv(paste("~/Desktop/PAHO_Mortality_NEW/Data/",cntry,"/",cntry,"_coverage_26Jul2018_ColNamesCorrected.csv",sep=""))
    
    # If coverage is >100%, set them to 100%.
    coverage$coverage <- ifelse(coverage$coverage>100, 100, coverage$coverage)
    
    # Select years to include in the average calculation
    # (from 2 yrs after introduction to the end)
    if (cntry %in% c("nc","pr")) {
      cov2yrs <- coverage[which(coverage$Year %in% unique(coverage$Year)[-c(1:3)]),] # Excluding the first 3 years
    } else {
      cov2yrs <- coverage[which(coverage$Year %in% unique(coverage$Year)[-c(1:2)]),] # Excluding the first 2 years
    }
    
    # Calculate average vaccine converage in these selected years
    avgcov <- aggregate(cov2yrs$coverage ~ cov2yrs$area2,FUN=mean)
    colnames(avgcov) <- c("area2","coverage")
    avgcov$country <- cntry
    print(hist(avgcov$coverage, main=paste(cntry), col="grey", xlab="Average coverage in each area",
               ylab="Number of areas", xlim=c(0,100), breaks=c(seq(0,100,2.5))))
    abline(v=80, col="red")
    
    # Merge the population and coverage data with the monthly time series
    #dat <- MonthlyTS_area2 # <-- This is to check if the for loop works & fix bugs. Deactivate this line when you are running the whole code to create datasets.
    cntrydata <- dat[which(dat$country==cntry),]
    cntrydata <- subset(cntrydata, select=-c(Age.group2)) # There are two age group variables, but Age.group will be used here.
    merge3 <- merge(cntrydata, avgcov, by = c("country","area2"), all.x=T)
    
    # Create coverage categories: Low, high
    merge3$covlevel <- ifelse(merge3$coverage>=0  & merge3$coverage <85,  0, NA) # low
    merge3$covlevel <- ifelse(merge3$coverage>=85 & merge3$coverage<=100, 1, merge3$covlevel) # high
    
    # Remove the following variables
    merge4 <- subset(merge3, select=-c(area2, coverage))
    
    # Aggregate data by country, age group, covlevel, and quarterdate
    mergelist[[i]] <- aggregate(. ~ country + Age.group + covlevel + quarterdate, data = merge4, sum, na.action = na.pass)
  }
  
  # Create one big dataset for all countries
  QuarterTScovar <- do.call(rbind, mergelist)
  # Create age_group
  QuarterTScovar$age_group <- paste(QuarterTScovar$country,QuarterTScovar$Age.group,QuarterTScovar$covlevel,sep="_")
  # Remove the following variables
  QuarterTScovar <- subset(QuarterTScovar, select=-c(country, covlevel, Age.group))
  # Reorder colomns
  final <- data.frame(age_group=QuarterTScovar$age_group, QuarterTScovar[,1:(ncol(QuarterTScovar)-1)])
  return(final)
} 

# Create time series data stratified by country, Age.group2, coverage, and quarterdate
FUN_Quarter_ByCov_AgeGroup2 <- function (dat) {
  for (i in 1:length(vec_countries)) {
    # Load a dataset for country i
    cntry <- vec_countries[i]
    coverage <- read.csv(paste("~/Desktop/PAHO_Mortality_NEW/Data/",cntry,"/",cntry,"_coverage_26Jul2018_ColNamesCorrected.csv",sep=""))
    
    # If coverage is >100%, set them to 100%.
    coverage$coverage <- ifelse(coverage$coverage>100, 100, coverage$coverage)
    
    # Select years to include in the average calculation
    # (from 2 yrs after introduction to the end)
    if (cntry %in% c("nc","pr")) {
      cov2yrs <- coverage[which(coverage$Year %in% unique(coverage$Year)[-c(1:3)]),] # Excluding the first 3 years
    } else {
      cov2yrs <- coverage[which(coverage$Year %in% unique(coverage$Year)[-c(1:2)]),] # Excluding the first 2 years
    }
    
    # Calculate average vaccine converage in these selected years
    avgcov <- aggregate(cov2yrs$coverage ~ cov2yrs$area2,FUN=mean)
    colnames(avgcov) <- c("area2","coverage")
    avgcov$country <- cntry
    print(hist(avgcov$coverage, main=paste(cntry), col="grey", xlab="Average coverage in each area",
               ylab="Number of areas", xlim=c(0,100), breaks=c(seq(0,100,2.5))))
    abline(v=80, col="red")
    
    # Merge the population and coverage data with the monthly time series
    #dat <- MonthlyTS_area2 # <-- This is to check if the for loop works & fix bugs. Deactivate this line when you are running the whole code to create datasets.
    cntrydata <- dat[which(dat$country==cntry),]
    cntrydata <- subset(cntrydata, select=-c(Age.group)) # There are two age group variables, but Age.group2 will be used here.
    merge3 <- merge(cntrydata, avgcov, by = c("country","area2"), all.x=T)
    
    # Create coverage categories: Low, high
    merge3$covlevel <- ifelse(merge3$coverage>=0  & merge3$coverage <85,  0, NA) # low
    merge3$covlevel <- ifelse(merge3$coverage>=85 & merge3$coverage<=100, 1, merge3$covlevel) # high
    
    # Remove the following variables
    merge4 <- subset(merge3, select=-c(area2, coverage))
    
    # Aggregate data by country, age group, covlevel, and quarterdate
    mergelist[[i]] <- aggregate(. ~ country + Age.group2 + covlevel + quarterdate, data = merge4, sum, na.action = na.pass)
  }
  
  # Create one big dataset for all countries
  QuarterTScovar <- do.call(rbind, mergelist)
  # Create age_group
  QuarterTScovar$age_group <- paste(QuarterTScovar$country,QuarterTScovar$Age.group2,QuarterTScovar$covlevel,sep="_")
  # Remove the following variables
  QuarterTScovar <- subset(QuarterTScovar, select=-c(country, covlevel, Age.group2))
  # Reorder colomns
  final <- data.frame(age_group=QuarterTScovar$age_group, QuarterTScovar[,1:(ncol(QuarterTScovar)-1)])
  return(final)
} 















#------------------------------------------------------------------------------#
# Create monthly time series stratified by country, HDI level, and Age.group
#------------------------------------------------------------------------------#

names(d0)
vec_countries <- c("ar","br","co","dr","ec","gy","hr","mx","nc","pr")

# Remove unrelated columns
d0_sub <- subset(d0, select=-c(Year, Month, age_m, area3,
                               HDI, coverage,
                               dx1, dx2, dx3, dx4, dx5, dx6,
                               quarterdate, monthq, quarter))

# Create time series
mergelist <- list()
TimeSeries <- FUN_Monthly_ByHDI_AgeGroup1(d0_sub)
table(TimeSeries$age_group)

# Save it
setwd("~/Desktop/PAHO_Mortality_New/Data")
write.csv(TimeSeries, file="PAHO_MonthlyTS_HDIcat_15Dec2018_SubChapters_Age.group.csv",row.names=F)


#------------------------------------------------------------------------------#
# Create monthly time series stratified by country, HDI level, and Age.group2
#------------------------------------------------------------------------------#

vec_countries <- c("ar","br","co","dr","ec","gy","hr","mx","nc","pr")

# Remove unrelated columns
d0_sub <- subset(d0, select=-c(Year, Month, age_m, area3,
                               HDI, coverage,
                               dx1, dx2, dx3, dx4, dx5, dx6,
                               quarterdate, monthq, quarter))

# Create time series
mergelist <- list()
TimeSeries <- FUN_Monthly_ByHDI_AgeGroup2(d0_sub)
table(TimeSeries$age_group)

# Save it
setwd("~/Desktop/PAHO_Mortality_New/Data")
write.csv(TimeSeries, file="PAHO_MonthlyTS_HDIcat_15Dec2018_SubChapters_Age.group2.csv",row.names=F)



#------------------------------------------------------------------------------#
# Create quarterly time series stratified by country, HDI level, and Age.group
#------------------------------------------------------------------------------#

vec_countries <- c("ar","br","co","dr","ec","gy","hr","mx","nc","pr")

# Remove unrelated columns
d0_sub <- subset(d0, select=-c(Year, Month, age_m, area3,
                               HDI, coverage,
                               dx1, dx2, dx3, dx4, dx5, dx6,
                               monthdate, monthq, quarter))

# Create time series
mergelist <- list()
TimeSeries <- FUN_Quarter_ByHDI_AgeGroup1(d0_sub)
table(TimeSeries$age_group)

# Save it
setwd("~/Desktop/PAHO_Mortality_New/Data")
write.csv(TimeSeries, file="PAHO_QuarterTS_HDIcat_15Dec2018_SubChapters_Age.group.csv",row.names=F)


#------------------------------------------------------------------------------#
# Create quarterly time series stratified by country, HDI level, and Age.group2
#------------------------------------------------------------------------------#

vec_countries <- c("ar","br","co","dr","ec","gy","hr","mx","nc","pr")

# Remove unrelated columns
d0_sub <- subset(d0, select=-c(Year, Month, age_m, area3,
                               HDI, coverage,
                               dx1, dx2, dx3, dx4, dx5, dx6,
                               monthdate, monthq, quarter))

# Create time series
mergelist <- list()
TimeSeries <- FUN_Quarter_ByHDI_AgeGroup2(d0_sub)
table(TimeSeries$age_group)

# Save it
setwd("~/Desktop/PAHO_Mortality_New/Data")
write.csv(TimeSeries, file="PAHO_QuarterTS_HDIcat_15Dec2018_SubChapters_Age.group2.csv",row.names=F)





#------------------------------------------------------------------------------#
# Create monthly time series stratified by country, HDI level, coverage, 
# and Age.group
#------------------------------------------------------------------------------#

vec_countries <- c("br","mx")

# Remove unrelated columns
d0_sub <- subset(d0, select=-c(Year, Month, age_m, area3,
                               HDI, coverage,
                               dx1, dx2, dx3, dx4, dx5, dx6,
                               quarterdate, monthq, quarter))

d0_sub <- d0_sub[which(d0_sub$country=="br" | d0_sub$country=="mx"),]

# Create time series
mergelist <- list()
TimeSeries <- FUN_Monthly_ByHDIandCov_AgeGroup1(d0_sub)
table(TimeSeries$age_group)

# Save it
setwd("~/Desktop/PAHO_Mortality_New/Data")
write.csv(TimeSeries, file="br_mx_ByHDIandCoverage.csv",row.names=F)



#------------------------------------------------------------------------------#
# Create monthly time series stratified by country, coverage level, and Age.group
#------------------------------------------------------------------------------#

vec_countries <- c("ar","br","co","dr","ec","gy","hr","mx","nc","pr")

# Remove unrelated columns
d0_sub <- subset(d0, select=-c(Year, Month, age_m, area3,
                               HDI, coverage,
                               dx1, dx2, dx3, dx4, dx5, dx6,
                               quarterdate, monthq, quarter))

# Create time series
mergelist <- list()
TimeSeries <- FUN_Monthly_ByCov_AgeGroup1(d0_sub)
table(TimeSeries$age_group)

# Save it
setwd("~/Desktop/PAHO_Mortality_New/Data")
write.csv(TimeSeries, file="PAHO_MonthlyTS_coverage_15Dec2018_SubChapters_Age.group.csv",row.names=F)


#------------------------------------------------------------------------------#
# Create monthly time series stratified by country, coverage level, and Age.group2
#------------------------------------------------------------------------------#

vec_countries <- c("ar","br","co","dr","ec","gy","hr","mx","nc","pr")

# Remove unrelated columns
d0_sub <- subset(d0, select=-c(Year, Month, age_m, area3,
                               HDI, coverage,
                               dx1, dx2, dx3, dx4, dx5, dx6,
                               quarterdate, monthq, quarter))

# Create time series
mergelist <- list()
TimeSeries <- FUN_Monthly_ByCov_AgeGroup2(d0_sub)
table(TimeSeries$age_group)

# Save it
setwd("~/Desktop/PAHO_Mortality_New/Data")
write.csv(TimeSeries, file="PAHO_MonthlyTS_coverage_15Dec2018_SubChapters_Age.group2.csv",row.names=F)








#------------------------------------------------------------------------------#
# Create quarterly time series stratified by country, coverage level, and Age.group
#------------------------------------------------------------------------------#

vec_countries <- c("ar","br","co","dr","ec","gy","hr","mx","nc","pr")

# Remove unrelated columns
d0_sub <- subset(d0, select=-c(Year, Month, age_m, area3,
                               HDI, coverage,
                               dx1, dx2, dx3, dx4, dx5, dx6,
                               monthdate, monthq, quarter))

# Create time series
mergelist <- list()
TimeSeries <- FUN_Quarter_ByCov_AgeGroup1(d0_sub)
table(TimeSeries$age_group)

# Save it
setwd("~/Desktop/PAHO_Mortality_New/Data")
write.csv(TimeSeries, file="PAHO_QuarterTS_coverage_15Dec2018_SubChapters_Age.group.csv",row.names=F)

#------------------------------------------------------------------------------#
# Create quarterly time series stratified by country, coverage level, and Age.group2
#------------------------------------------------------------------------------#

vec_countries <- c("ar","br","co","dr","ec","gy","hr","mx","nc","pr")

# Remove unrelated columns
d0_sub <- subset(d0, select=-c(Year, Month, age_m, area3,
                               HDI, coverage,
                               dx1, dx2, dx3, dx4, dx5, dx6,
                               monthdate, monthq, quarter))

# Create time series
mergelist <- list()
TimeSeries <- FUN_Quarter_ByCov_AgeGroup2(d0_sub)
table(TimeSeries$age_group)

# Save it
setwd("~/Desktop/PAHO_Mortality_New/Data")
write.csv(TimeSeries, file="PAHO_QuarterTS_coverage_15Dec2018_SubChapters_Age.group2.csv",row.names=F)
