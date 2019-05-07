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

# This script reformat time series data by coverage and Age.group2


#------------------------------------------------------------------------------#
# Set up
#------------------------------------------------------------------------------#

rm(list = ls(all = TRUE))
setwd("~/Desktop/PAHO_Mortality_New/Data")
col1 <- read.csv("PAHO_QuarterTS_coverage_15Dec2018_SubChapters_Age.group2.csv", as.is = T)

# Check
table(col1$age_group)

# Create the followin variables
age_cat<-substr(col1$age_group,4,4)
country_code<-substr(col1$age_group,1,2)
cov_cat<-substr(col1$age_group,6,6)

# Check
unique(country_code) # Should be ""ar" "br" "co" "dr" "ec" "nc" "pr"
unique(age_cat) # Should be  "2" "3" "4" "5" "6" "7"
unique(cov_cat) # Should be  "0" "1"
table(age_cat, country_code)
table(cov_cat, country_code)

# Create age_cat_char
age_cat_char<-rep(NA, length(age_cat) )
age_cat_char[age_cat=='2']<-'12-23m'
age_cat_char[age_cat=='3']<-'24-59m'
age_cat_char[age_cat=='4']<-'<1m'
age_cat_char[age_cat=='5']<-'1m'
age_cat_char[age_cat=='6']<-'2-5m'
age_cat_char[age_cat=='7']<-'6-11m'
table(age_cat, age_cat_char)

# Create cov_cat_char
cov_cat_char<-rep(NA, length(cov_cat) )
cov_cat_char[cov_cat=='0']<-'Low cov'
cov_cat_char[cov_cat=='1']<-'Hig cov'
table(cov_cat, cov_cat_char)

#------------------------------------------------------------------------------#
# Create functions
#------------------------------------------------------------------------------#

# Create functions to aggregate by country
agg_ntnl <- function(dt,select_agegrp,name_agegrp) {
  # Aggregate data by country and date
  agg<- aggregate(dt[age_cat_char %in% select_agegrp,-c(1:2)], # Remove 1st and 2nd columns as they are age_group and date which should not be aggregated
                  by=list(country_code[age_cat_char %in% select_agegrp],
                          dt$quarterdate[age_cat_char %in% select_agegrp]), FUN=sum)
  # Add age_group as a first colomn
  agg <-cbind.data.frame(paste(agg$Group.1,name_agegrp,'A' ,sep=" "), agg)
  # Rename columns
  names(agg)[1:3]<- c('age_group', 'country_code', 'quarterdate')
  return(agg)
}

# Create functions to aggregate by country and coverage
agg_cov <- function(dt,select_agegrp,name_agegrp) {
  # Aggregate data by country, date, and coverage
  agg<- aggregate(dt[age_cat_char %in% select_agegrp,-c(1:2)],  # Remove 1st and 2nd columns as they are age_group and date which should not be aggregated
                  by=list(country_code[age_cat_char %in% select_agegrp],
                          col1$quarterdate[age_cat_char %in% select_agegrp],
                          cov_cat_char[age_cat_char %in% select_agegrp]), FUN=sum)
  # Add age_group as a first colomn
  agg <-cbind.data.frame(paste(agg$Group.1,name_agegrp,agg$Group.3,sep=" "), agg)
  # Rename columns
  names(agg)[1:3]<- c('age_group', 'country_code', 'quarterdate')
  # Remove coverage
  agg$Group.3<-NULL
  return(agg)
}

#------------------------------------------------------------------------------#
# Reformat time series data by coverage and Age.group
#------------------------------------------------------------------------------#

#-----*-----*-----*-----*-----#
# For <1m
#-----*-----*-----*-----*-----#

select_agegrp <- c('<1m')
name_agegrp <- "<1m"
# Country-level
ntnl <- agg_ntnl(col1, select_agegrp, name_agegrp)
# cov-level
cov <- agg_cov(col1, select_agegrp, name_agegrp)
# Merge them
all.agg<- rbind.data.frame(ntnl,cov)

#-----*-----*-----*-----*-----#
# For 1m
#-----*-----*-----*-----*-----#

select_agegrp <- c('1m')
name_agegrp <- "1m"
# Country-level
ntnl <- agg_ntnl(col1, select_agegrp, name_agegrp)
# cov-level
cov <- agg_cov(col1, select_agegrp, name_agegrp)
# Merge them
all.agg<- rbind.data.frame(all.agg,ntnl,cov)

#-----*-----*-----*-----*-----#
# For 2-5m
#-----*-----*-----*-----*-----#

select_agegrp <- c('2-5m')
name_agegrp <- "2-5m"
# Country-level
ntnl <- agg_ntnl(col1, select_agegrp, name_agegrp)
# cov-level
cov <- agg_cov(col1, select_agegrp, name_agegrp)
# Merge them
all.agg<- rbind.data.frame(all.agg,ntnl,cov)

#-----*-----*-----*-----*-----#
# For 6-11m
#-----*-----*-----*-----*-----#

select_agegrp <- c('6-11m')
name_agegrp <- "6-11m"
# Country-level
ntnl <- agg_ntnl(col1, select_agegrp, name_agegrp)
# cov-level
cov <- agg_cov(col1, select_agegrp, name_agegrp)
# Merge them
all.agg<- rbind.data.frame(all.agg,ntnl,cov)

#-----*-----*-----*-----*-----#
# For 12-23m
#-----*-----*-----*-----*-----#

select_agegrp <- c('12-23m')
name_agegrp <- "12-23m"
# Country-level
ntnl <- agg_ntnl(col1, select_agegrp, name_agegrp)
# cov-level
cov <- agg_cov(col1, select_agegrp, name_agegrp)
# Merge them
all.agg<- rbind.data.frame(all.agg,ntnl,cov)

#-----*-----*-----*-----*-----#
# For 24-59m
#-----*-----*-----*-----*-----#

select_agegrp <- c('24-59m')
name_agegrp <- "24-59m"
# Country-level
ntnl <- agg_ntnl(col1, select_agegrp, name_agegrp)
# cov-level
cov <- agg_cov(col1, select_agegrp, name_agegrp)
# Merge them
all.agg<- rbind.data.frame(all.agg,ntnl,cov)

#-----*-----*-----*-----*-----#
# For <2m
#-----*-----*-----*-----*-----#

select_agegrp <- c('<1m','1m')
name_agegrp <- "<2m"
# Country-level
ntnl <- agg_ntnl(col1, select_agegrp, name_agegrp)
# cov-level
cov <- agg_cov(col1, select_agegrp, name_agegrp)
# Merge them
all.agg<- rbind.data.frame(all.agg,ntnl,cov)

#-----*-----*-----*-----*-----#
# For 2-11m
#-----*-----*-----*-----*-----#

select_agegrp <- c('2-5m','6-11m')
name_agegrp <- "2-11m"
# Country-level
ntnl <- agg_ntnl(col1, select_agegrp, name_agegrp)
# cov-level
cov <- agg_cov(col1, select_agegrp, name_agegrp)
# Merge them
all.agg<- rbind.data.frame(all.agg,ntnl,cov)

#-----*-----*-----*-----*-----#
# For 2-24m
#-----*-----*-----*-----*-----#

select_agegrp <- c('2-5m','6-11m','12-23m')
name_agegrp <- "2-24m"
# Country-level
ntnl <- agg_ntnl(col1, select_agegrp, name_agegrp)
# cov-level
cov <- agg_cov(col1, select_agegrp, name_agegrp)
# Merge them
all.agg<- rbind.data.frame(all.agg,ntnl,cov)

#-----*-----*-----*-----*-----#
# For 2-59m
#-----*-----*-----*-----*-----#

select_agegrp <- c('2-5m','6-11m','12-23m','24-59m')
name_agegrp <- "2-59m"
# Country-level
ntnl <- agg_ntnl(col1, select_agegrp, name_agegrp)
# cov-level
cov <- agg_cov(col1, select_agegrp, name_agegrp)
# Merge them
all.agg<- rbind.data.frame(all.agg,ntnl,cov)

#------------------------------------------------------------------------------#
# Check and save
#------------------------------------------------------------------------------#

# Order by age_group and quarterdate
all.agg<-all.agg[order(all.agg$age_group, all.agg$quarterdate),]

# Remove country_code
all.agg<-subset(all.agg, select=-c(country_code))

# Check
table(all.agg$age_group)
table(all.agg$age_group[which(substr(all.agg$age_group,1,2)=="mx")])[table(all.agg$age_group[which(substr(all.agg$age_group,1,2)=="mx")])>0]

setwd("~/Desktop/PAHO_Mortality_New/Data")
write.csv(all.agg, 'Quarterly_ByAgeGroup2_ByCoverage_15Dec2018.csv', row.names=F)
