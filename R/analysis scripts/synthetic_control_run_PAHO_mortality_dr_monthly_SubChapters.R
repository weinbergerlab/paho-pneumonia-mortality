
#MOST DATES MUST BE IN FORMAT "YYYY-MM-01", exception is end of pre period, which is 1 day before end of post period
start_date        <- as.Date('2005-01-01') #Indicates the date of the first data point.
intervention_date <- as.Date('2013-08-31') #Indicates the date of intervention in the data.
end_date          <- as.Date('2015-12-01') #Indicates the date of the last data point.
pre_period        <- as.Date(c('2005-01-01', '2013-08-31')) #Range over which the data is trained for the CausalImpact model.
post_period       <- as.Date(c('2013-09-01', '2015-12-01')) #Range from the intervention date to the end date.
eval_period       <- as.Date(c('2014-09-01', '2015-12-01')) #Range over which rate ratio calculation will be performed.
year_def   <-'cal_year'  #Can be cal_year to aggregate results by Jan-Dec; 'epi_year' to aggregate July-June
