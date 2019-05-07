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

# This script merges the final versions of the datasets from 10 countries into 
# one .csv file, and then, remove "unknown" areas.
#
# Final versions of the datasets were created by Kayoko using R. See her scripts 
# "xx_CovariateMerging_15Dec2018.R" for each country.



#------------------------------------------------------------------------------#
# SET UP
#------------------------------------------------------------------------------#

rm(list = ls(all = TRUE))
# Load final, cleaned versions of datasets for 10 countries
l <- list()
l[[1]] <- read.csv("~/Desktop/PAHO_Mortality_New/Data/ar/ar_ForAnalysis_15Dec2018.csv", as.is = T)
l[[2]] <- read.csv("~/Desktop/PAHO_Mortality_New/Data/br/br_ForAnalysis_15Dec2018.csv", as.is = T)
l[[3]] <- read.csv("~/Desktop/PAHO_Mortality_New/Data/co/co_ForAnalysis_15Dec2018.csv", as.is = T)
l[[4]] <- read.csv("~/Desktop/PAHO_Mortality_New/Data/dr/dr_ForAnalysis_15Dec2018.csv", as.is = T)
l[[5]] <- read.csv("~/Desktop/PAHO_Mortality_New/Data/ec/ec_ForAnalysis_15Dec2018.csv", as.is = T)
l[[6]] <- read.csv("~/Desktop/PAHO_Mortality_New/Data/gy/gy_ForAnalysis_15Dec2018.csv", as.is = T)
l[[7]] <- read.csv("~/Desktop/PAHO_Mortality_New/Data/hr/hr_ForAnalysis_15Dec2018.csv", as.is = T)
l[[8]] <- read.csv("~/Desktop/PAHO_Mortality_New/Data/mx/mx_ForAnalysis_15Dec2018.csv", as.is = T)
l[[9]] <- read.csv("~/Desktop/PAHO_Mortality_New/Data/nc/nc_ForAnalysis_15Dec2018.csv", as.is = T)
l[[10]] <- read.csv("~/Desktop/PAHO_Mortality_New/Data/pr/pr_ForAnalysis_15Dec2018.csv", as.is = T)

# Check column names before merging
which(!c(names(l[[1]]) %in% names(l[[2]]))) # Should be zero
which(!c(names(l[[1]]) %in% names(l[[3]]))) # Should be zero
which(!c(names(l[[1]]) %in% names(l[[4]]))) # Should be zero
which(!c(names(l[[1]]) %in% names(l[[5]]))) # Should be zero
which(!c(names(l[[1]]) %in% names(l[[6]]))) # Should be zero
which(!c(names(l[[1]]) %in% names(l[[7]]))) # Should be zero
which(!c(names(l[[1]]) %in% names(l[[8]]))) # Should be zero
which(!c(names(l[[1]]) %in% names(l[[9]]))) # Should be zero
which(!c(names(l[[1]]) %in% names(l[[10]]))) # Should be zero

#------------------------------------------------------------------------------#
# Merge 10 datasets
#------------------------------------------------------------------------------#

# Merge these datasets from 10 countries
merge10countries <- do.call(rbind, lapply(l, function(x) x[match(names(l[[1]]), names(x))]))

# Check: The following line should be TRUE
nrow(l[[1]])+nrow(l[[2]])+nrow(l[[3]])+nrow(l[[4]])+nrow(l[[5]])+nrow(l[[6]])+nrow(l[[7]])+nrow(l[[8]])+nrow(l[[9]])+nrow(l[[10]]) == nrow(merge10countries)


#------------------------------------------------------------------------------#
# Remove unknwon areas
#------------------------------------------------------------------------------#

d0 <- merge10countries

# Argentina
table(d0$area2[d0$country=="ar"], useNA = "ifany") # There are 98 and 99, which should be removed
length(which(d0$country=="ar" & c(d0$area2==98 | d0$area2==99))) # 544
View(d0[which(d0$country=="ar" & c(d0$area2==98 | d0$area2==99)),])
ar_remove <- ifelse(d0$country=="ar" & c(d0$area2==98 | d0$area2==99), 1, 0)
table(ar_remove, useNA = "ifany") # 544 ones
table(d0$area2, ar_remove, useNA = "ifany")
d0.1 <- d0[which(ar_remove!=1),]
nrow(d0)-nrow(d0.1) == length(which(d0$country=="ar" & c(d0$area2==98 | d0$area2==99))) # Should be TRUE

# DR
table(d0.1$area2[d0.1$country=="dr"], useNA = "ifany") # There is 99, which should be removed
length(which(d0.1$country=="dr" & d0.1$area2==99)) # 72
View(d0[which(d0.1$country=="dr" & d0.1$area2==99),])
dr_remove <- ifelse(d0.1$country=="dr" & d0.1$area2==99, 1, 0)
table(dr_remove, useNA = "ifany") # 72 ones
table(d0.1$area2, dr_remove, useNA = "ifany")
d0.2 <- d0.1[which(dr_remove==0 | is.na(dr_remove)),]
nrow(d0.1)-nrow(d0.2) == length(which(d0.1$country=="dr" & d0.1$area2==99)) # Should be TRUE

# Ecuador
table(d0.2$area2[d0.2$country=="ec"], useNA = "ifany") # there are 88 and 90, which should be removed
length(which(d0.2$country=="ec" & c(d0.2$area2==88 | d0.2$area2==90))) # 52
View(d0.2[which(d0.2$country=="ec" & c(d0.2$area2==88 | d0.2$area2==90)),])
ec_remove <- ifelse(d0.2$country=="ec" & c(d0.2$area2==88 | d0.2$area2==90), 1, 0)
table(ec_remove, useNA = "ifany") # 52 ones
table(d0.2$area2, ec_remove, useNA = "ifany")
d0.3 <- d0.2[which(ec_remove!=1),]
nrow(d0.2)-nrow(d0.3) == length(which(d0.2$country=="ec" & c(d0.2$area2==88 | d0.2$area2==90))) # Should be TRUE

table(d0.2$country[which(is.na(d0.2$area2))])

d <- d0.2

# Create a final version of .csv file
setwd("~/Desktop/PAHO_Mortality_New/Data")
write.csv(d, file="PAHO_10countries_IndividualLevel_15Dec2018.csv",row.names=F)

