library(purrr)
library(readr)
library(dplyr)
library(stats)
library(DataCombine)

files <- list.files(path ='~/Documents/ISYE 4031/Project/ACS_5year_folder',full.names=TRUE)

data_2010 = read.csv(files[1],skip=1,header=TRUE)
for (i in c(2,19,20))
{ data_2010=full_join(data_2010,read.csv(files[i],skip=1, header=TRUE), by= "id")
}
names(data_2010)[3:length(names(data_2010))] <- paste0(names(data_2010)[3:length(names(data_2010))], ".2010")

data_2011 = read.csv(files[3],skip=1,header=TRUE)
for (i in c(4,21,22))
{ data_2011=full_join(data_2011,read.csv(files[i],skip=1, header=TRUE), by= "id")
}
names(data_2011)[3:length(names(data_2011))] <- paste0(names(data_2011)[3:length(names(data_2011))], ".2011")

data_2012 = read.csv(files[5],skip=1,header=TRUE)
for (i in c(6,23,24))
{ data_2012=full_join(data_2012,read.csv(files[i],skip=1, header=TRUE), by= "id")
}
names(data_2012)[3:length(names(data_2012))] <- paste0(names(data_2012)[3:length(names(data_2012))], ".2012")

data_2013 = read.csv(files[7],skip=1,header=TRUE)
for (i in c(8,25,26))
{ data_2013=full_join(data_2013,read.csv(files[i],skip=1, header=TRUE), by= "id")
}
names(data_2013)[3:length(names(data_2013))] <- paste0(names(data_2013)[3:length(names(data_2013))], ".2013")

data_2014 = read.csv(files[9],skip=1,header=TRUE)
for (i in c(10,27,28))
{ data_2014=full_join(data_2014,read.csv(files[i],skip=1, header=TRUE), by= "id")
}
names(data_2014)[3:length(names(data_2014))] <- paste0(names(data_2014)[3:length(names(data_2014))], ".2014")

data_2015 = read.csv(files[11],skip=1,header=TRUE)
for (i in c(12,29,30))
{ data_2015=full_join(data_2015,read.csv(files[i],skip=1, header=TRUE), by= "id")
}
names(data_2015)[3:length(names(data_2015))] <- paste0(names(data_2015)[3:length(names(data_2015))], ".2015")

data_2016 = read.csv(files[13],skip=1,header=TRUE)
for (i in c(14,31,32))
{ data_2016=full_join(data_2016,read.csv(files[i],skip=1, header=TRUE), by= "id")
}
names(data_2016)[3:length(names(data_2016))] <- paste0(names(data_2016)[3:length(names(data_2016))], ".2016")

data_2017 = read.csv(files[15],skip=1,header=TRUE)
for (i in c(16,33,34))
{ data_2017=full_join(data_2017,read.csv(files[i],skip=1, header=TRUE), by= "id")
}
names(data_2017)[3:length(names(data_2017))] <- paste0(names(data_2017)[3:length(names(data_2017))], ".2017")

data_2018 = read.csv(files[17],skip=1,header=TRUE)
for (i in c(18,35,36))
{ data_2018=full_join(data_2018,read.csv(files[i],skip=1, header=TRUE), by= "id")
}
names(data_2018)[3:length(names(data_2018))] <- paste0(names(data_2018)[3:length(names(data_2018))], ".2018")

all_data <- Reduce(function(x, y) merge(x, y, all=TRUE), list(data_2010, data_2011, data_2012,data_2013,data_2014,data_2015,data_2016,data_2017, data_2018))

all_data[is.na(all_data)] = 0
i = c(3:13877)
all_data[,i] <- apply(all_data[ , i], 2, function(x) as.numeric(as.character(x)))
all_data[is.na(all_data)] = 0

names = names(all_data)
bad_names = names[grepl("Margin.of.Error",names, fixed=TRUE)]
ix <- which(names %in% bad_names)
all_data <- all_data[,-ix]
all_data = all_data[, colSums(all_data != 0) > 0]
for (i in (names(all_data)))
{str_replace(i, '-', '')
  str_replace(i, '\\*', '')
  str_replace(i, '\\+', '')}

i = c(3:6719)
all_data[,i] <- apply(all_data[ , i], 2, function(x) as.numeric(as.character(x)))
all_data$Percent_Change_in_Median_Home_Value_2010_to_2014 <- (all_data$Estimate..VALUE..Owner.occupied.units..Median..dollars..2014 - all_data$Estimate..VALUE..Median..dollars..2010)/(all_data$Estimate..VALUE..Median..dollars..2010)*100
all_data$Percent_Change_in_Median_Home_Value_2014_to_2018 <- (all_data$Estimate..VALUE..Owner.occupied.units..Median..dollars..2018 - all_data$Estimate..VALUE..Owner.occupied.units..Median..dollars..2014)/(all_data$Estimate..VALUE..Owner.occupied.units..Median..dollars..2014)*100

all_data <- all_data[!is.na(all_data$Percent_Change_in_Median_Home_Value_2010_to_2014),]
all_data <- all_data[!is.na(all_data$Percent_Change_in_Median_Home_Value_2014_to_2018),]
all_data <- all_data[!is.infinite(all_data$Percent_Change_in_Median_Home_Value_2010_to_2014),]
all_data <- all_data[!is.infinite(all_data$Percent_Change_in_Median_Home_Value_2014_to_2018),]

g = lm(all_data$Percent_Change_in_Median_Home_Value_2010_to_2018~all_data$Estimate..UNITS.IN.STRUCTURE..Total.housing.units.2010, na.action=na.exclude)
summary(g)

names = names(all_data)
good_names1 = names[grepl('Percent..HOUSING.OCCUPANCY..Vacant.housing.units',names,fixed=TRUE)] #2010-2012
good_names2 = names[grepl('Estimate..HOUSING.OCCUPANCY..Homeowner.vacancy.rate',names,fixed=TRUE)] #2010-2016
good_names3 = names[grepl('Estimate..HOUSING.OCCUPANCY..Total.housing.units.201',names,fixed=TRUE)] #2010-2018 (use only estimate. not percent.estimate)
good_names4 = names[grepl('Percent..HOUSING.OCCUPANCY..Occupied.housing.units',names,fixed=TRUE)] #2010-2012
good_names5 = names[grepl('Percent..HOUSING.TENURE..Renter.occupied',names,fixed=TRUE)] #2010-2012
good_names6 = names[grepl('Percent..VEHICLES.AVAILABLE..No.vehicles.available',names,fixed=TRUE)] #2010-2012
good_names7 = names[grepl('Percent..SELECTED.CHARACTERISTICS..No.telephone.service.available',names,fixed=TRUE)] #2010-2012
good_names8 = names[grepl('Percent..SEX.AND.AGE..Male',names,fixed=TRUE)] #2010-2012
good_names9 = names[grepl('Percent..UNITS.IN.STRUCTURE..1.unit..detached',names,fixed=TRUE)]
good_names10 = names[grepl('Estimate..GROSS.RENT..No.rent.paid',names,fixed=TRUE)]
good_names11 = names[grepl('Percent..YEAR.STRUCTURE.BUILT..Built.2005.or.later',names,fixed=TRUE)]
good_names12 = names[grepl('Estimate..ROOMS..Median.rooms',names,fixed=TRUE)]
good_names13 = names[grepl('Estimate..HOUSING.TENURE..Average.household.size.of.owner.occupied.unit',names,fixed=TRUE)]
#14 covers 2010
good_names14 = names[grepl('Percent..YEAR.HOUSEHOLDER.MOVED.INTO.UNIT..Moved.in.2005.or.later',names,fixed=TRUE)]
#15 covers 2010
good_names15 = names[grepl('Percent..HOUSE.HEATING.FUEL..Utility.gas',names,fixed=TRUE)]
#16 covers 2010
good_names16 = names[grepl('Percent..SELECTED.CHARACTERISTICS..Lacking.complete.plumbing.facilities',names,fixed=TRUE)]
#17 covers 2010,2014
good_names17 = names[grepl('Renter.occupied.housing.units..Estimate..HOUSEHOLD.SIZE..4.or.more.person.household',names,fixed=TRUE)]
#18 covers 2010,2014
good_names18 = names[grepl('Renter.occupied.housing.units..Estimate..FAMILY.TYPE.AND.PRESENCE.OF.OWN.CHILDREN..No.related.children.under.18.years',names,fixed=TRUE)]
#19 covers 2010
good_names19 = names[grepl('Estimate..VALUE..Median..dollars',names,fixed=TRUE)]
#20 covers 2014,2018
good_names20 = names[grepl('Estimate..VALUE..Owner.occupied.units..Median..dollars',names,fixed=TRUE)]
#21 covers 2010
good_names21 = names[grepl('Estimate..GROSS.RENT..Median..dollars',names,fixed=TRUE)]
#22 covers 2014
good_names22 = names[grepl('Occupied.housing.units..Estimate..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2014.INFLATION.ADJUSTED.DOLLARS...Median.household.income..dollars',names,fixed=TRUE)]
#25 covers 2014
good_names25 = names[grepl('Percent..YEAR.HOUSEHOLDER.MOVED.INTO.UNIT..Occupied.housing.units..Moved.in.2010.or.later',names,fixed=TRUE)]
#28 covers 2014
good_names28 = names[grepl('Percent..HOUSE.HEATING.FUEL..Occupied.housing.units..Utility.gas',names,fixed=TRUE)]
#29 covers 2014
good_names29 = names[grepl('Percent..SELECTED.CHARACTERISTICS..Occupied.housing.units..Lacking.complete.plumbing.facilities',names,fixed=TRUE)]
#33 covers 2014,2018
good_names33 = names[grepl('Estimate..GROSS.RENT..Occupied.units.paying.rent..Median..dollars',names,fixed=TRUE)]
#34 covers 2010
good_names34= names[grepl('Occupied.housing.units..Estimate..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2010.INFLATION.ADJUSTED.DOLLARS...Median.household.income..dollars',names,fixed=TRUE)]
good_names36 = names[grepl('Percent..HOUSING.OCCUPANCY..Total.housing.units..Vacant.housing.units',names,fixed=TRUE)] #2013-2016
good_names37 = names[grepl('Percent.Estimate..HOUSING.OCCUPANCY..Total.housing.units..Vacant.housing.units',names,fixed=TRUE)] #2017, 2018
good_names38 = names[grepl('Estimate..HOUSING.OCCUPANCY..Total.housing.units..Homeowner.vacancy.rate',names,fixed=TRUE)] #2017,2018 (Use Estimate not Percent.Estimate)
good_names39 = names[grepl('Percent..HOUSING.OCCUPANCY..Total.housing.units..Occupied.housing.units',names,fixed=TRUE)] #2013-2016
good_names40 = names[grepl('Percent.Estimate..HOUSING.OCCUPANCY..Total.housing.units..Occupied.housing.units',names,fixed=TRUE)] #2017-2018
good_names41 = names[grepl('Percent..HOUSING.TENURE..Occupied.housing.units..Renter.occupied',names,fixed=TRUE)] #2013-2016
good_names42 = names[grepl('Percent.Estimate..HOUSING.TENURE..Occupied.housing.units..Renter.occupied.2018',names,fixed=TRUE)] #2018
good_names43 = names[grepl('Percent..VEHICLES.AVAILABLE..Occupied.housing.units..No.vehicles.available',names,fixed=TRUE)] #2013-2016
good_names44 = names[grepl('Percent.Estimate..VEHICLES.AVAILABLE..Occupied.housing.units..No.vehicles.available',names,fixed=TRUE)] #2017-2018
good_names45 = names[grepl('Percent..SELECTED.CHARACTERISTICS..Occupied.housing.units..No.telephone.service.available',names,fixed=TRUE)] #2013-2016
good_names46 = names[grepl('Percent.Estimate..SELECTED.CHARACTERISTICS..Occupied.housing.units..No.telephone.service.available',names,fixed=TRUE)] #2017-2018
good_names47 = names[grepl('Percent..RACE..White',names,fixed=TRUE)] #2010-2012
good_names48 = names[grepl("Percent..RACE..Race.alone.or.in.combination.with.one.or.more.other.races..Total.population..White",names,fixed=TRUE)] #2013-2016
good_names49 = names[grepl("Percent.Estimate..Race.alone.or.in.combination.with.one.or.more.other.races..Total.population..White",names,fixed=TRUE)] #2017-2018
good_names50 = names[grepl('Percent..SEX.AND.AGE..Total.population..Male',names,fixed=TRUE)] #2014,2016
good_names51 = names[grepl('Percent.Estimate..SEX.AND.AGE..Total.population..Male',names,fixed=TRUE)] #2017,2018

# ved added
good_names52 = names[grepl('Estimate..GROSS.RENT..Occupied.units.paying.rent..No.rent.paid.2018', names,fixed=TRUE)] #for goodanames10 2018
good_names53 = names[grepl('Percent.Estimate..YEAR.STRUCTURE.BUILT..Total.housing.units..Built.2014.or.later.2018', names,fixed=TRUE)]  #for goodanames11 2018
good_names54 = names[grepl('Estimate..ROOMS..Total.housing.units..Median.rooms.2014', names,fixed=TRUE)]  #for goodanames12 2014
good_names55 = names[grepl('Percent.Estimate..ROOMS..Total.housing.units..Median.rooms.2018', names,fixed=TRUE)]  #for goodanames12 2018
good_names56 = names[grepl('Estimate..HOUSING.TENURE..Occupied.housing.units..Average.household.size.of.owner.occupied.unit.2018', names,fixed=TRUE)]  #for goodanames13 2018
good_names = c(good_names1,good_names2,good_names3,good_names4,good_names5,good_names6,good_names7,
               good_names8,good_names9,good_names10,good_names11,good_names12,good_names13,good_names14,
               good_names15,good_names16,good_names17,good_names18,good_names19,good_names20,good_names21,
               good_names22, good_names25, good_names26,good_names27,good_names28,good_names29,good_names30,
               good_names31,good_names32, good_names33, good_names34, good_names35, good_names36, good_names37,
               good_names38, good_names39, good_names40, good_names41,good_names42, good_names43, good_names44,
               good_names45, good_names46,good_names47, good_names48, good_names49, good_names50, good_names51,
               good_names52, good_names53,good_names54, good_names55,good_names56,
               'id','Geographic.Area.Name.x','Percent_Change_in_Median_Home_Value_2010_to_2014',
               'Percent_Change_in_Median_Home_Value_2014_to_2018')

ix <- which(names %in% good_names)
all_data <- all_data[,ix]

all_data$Percent_Change_in_Recent_Move_In_2010_to_2014 <- (all_data$Percent..YEAR.HOUSEHOLDER.MOVED.INTO.UNIT..Occupied.housing.units..Moved.in.2010.or.later.2014- all_data$Percent..YEAR.HOUSEHOLDER.MOVED.INTO.UNIT..Moved.in.2005.or.later.2010)/(all_data$Percent..YEAR.HOUSEHOLDER.MOVED.INTO.UNIT..Moved.in.2005.or.later.2010)*100
all_data$Percent_Change_in_Gas_Usage_2010_to_2014 <- (all_data$Percent..HOUSE.HEATING.FUEL..Occupied.housing.units..Utility.gas.2014-all_data$Percent..HOUSE.HEATING.FUEL..Utility.gas.2010)/(all_data$Percent..HOUSE.HEATING.FUEL..Utility.gas.2010)*100
all_data$Percent_Change_in_Plumbing_2010_to_2014 <- (all_data$Percent..SELECTED.CHARACTERISTICS..Occupied.housing.units..Lacking.complete.plumbing.facilities.2014-all_data$Percent..SELECTED.CHARACTERISTICS..Lacking.complete.plumbing.facilities.2010)
all_data$Percent_Change_in_Large_Households_2010_to_2014 <- (all_data$Renter.occupied.housing.units..Estimate..HOUSEHOLD.SIZE..4.or.more.person.household.2014 - all_data$Renter.occupied.housing.units..Estimate..HOUSEHOLD.SIZE..4.or.more.person.household.2010)/(all_data$Renter.occupied.housing.units..Estimate..HOUSEHOLD.SIZE..4.or.more.person.household.2010)*100
all_data$Percent_Change_in_Children_2010_to_2014 <- (all_data$Renter.occupied.housing.units..Estimate..FAMILY.TYPE.AND.PRESENCE.OF.OWN.CHILDREN..No.related.children.under.18.years.2014 - all_data$Renter.occupied.housing.units..Estimate..FAMILY.TYPE.AND.PRESENCE.OF.OWN.CHILDREN..No.related.children.under.18.years.2010)/(all_data$Renter.occupied.housing.units..Estimate..FAMILY.TYPE.AND.PRESENCE.OF.OWN.CHILDREN..No.related.children.under.18.years.2010)*100
all_data$Percent_Change_in_Rent_2010_to_2014 <- (all_data$Estimate..GROSS.RENT..Occupied.units.paying.rent..Median..dollars..2014-all_data$Estimate..GROSS.RENT..Median..dollars..2010)/(all_data$Estimate..GROSS.RENT..Median..dollars..2010)*100
all_data$Percent_Change_in_Income_2010_to_2014 <- (all_data$Occupied.housing.units..Estimate..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2014.INFLATION.ADJUSTED.DOLLARS...Median.household.income..dollars..2014-all_data$Occupied.housing.units..Estimate..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2010.INFLATION.ADJUSTED.DOLLARS...Median.household.income..dollars..2010)/ (all_data$Occupied.housing.units..Estimate..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2010.INFLATION.ADJUSTED.DOLLARS...Median.household.income..dollars..2010)*100

# Kira's percent change
all_data$Percent_Change_in_Vacant_Housing_Units_2010_to_2014 <- (all_data$Percent..HOUSING.OCCUPANCY..Total.housing.units..Vacant.housing.units.2014 - all_data$Percent..HOUSING.OCCUPANCY..Vacant.housing.units.2010)/all_data$Percent..HOUSING.OCCUPANCY..Vacant.housing.units.2010 * 100
all_data$Percent_Change_in_Occupied_Housing_Units_2010_to_2014 <- (all_data$Percent..HOUSING.OCCUPANCY..Total.housing.units..Occupied.housing.units.2014 - all_data$Percent..HOUSING.OCCUPANCY..Occupied.housing.units.2010) / all_data$Percent..HOUSING.OCCUPANCY..Occupied.housing.units.2010 * 100
all_data$Percent_Change_in_Total_Housing_Units_2010_to_2014 <- (all_data$Estimate..HOUSING.OCCUPANCY..Total.housing.units.2014 - all_data$Estimate..HOUSING.OCCUPANCY..Total.housing.units.2010) / all_data$Estimate..HOUSING.OCCUPANCY..Total.housing.units.2010 * 100
all_data$Percent_Change_in_Homeowner_Vacancy_Rate_2010_to_2014 <- (all_data$Estimate..HOUSING.OCCUPANCY..Homeowner.vacancy.rate.2014 - all_data$Estimate..HOUSING.OCCUPANCY..Homeowner.vacancy.rate.2010) / all_data$Estimate..HOUSING.OCCUPANCY..Homeowner.vacancy.rate.2010 * 100
all_data$Percent_Change_in_Renter_Occupied_Housing_Tenure_2010_to_2014 <- (all_data$Percent..HOUSING.TENURE..Occupied.housing.units..Renter.occupied.2014 - all_data$Percent..HOUSING.TENURE..Renter.occupied.2010) / all_data$Percent..HOUSING.TENURE..Renter.occupied.2010 * 100
all_data$Percent_Change_in_No_Vehicles_Available_2010_to_2014 <- (all_data$Percent..VEHICLES.AVAILABLE..Occupied.housing.units..No.vehicles.available.2014- all_data$Percent..VEHICLES.AVAILABLE..No.vehicles.available.2010)/all_data$Percent..VEHICLES.AVAILABLE..No.vehicles.available.2010 * 100
all_data$Percent_Change_in_No_Telephone_Service_Availble_2010_to_2014 <- (all_data$Percent..SELECTED.CHARACTERISTICS..Occupied.housing.units..No.telephone.service.available.2014- all_data$Percent..SELECTED.CHARACTERISTICS..No.telephone.service.available.2010) / all_data$Percent..SELECTED.CHARACTERISTICS..No.telephone.service.available.2010 * 100
all_data$Percent_Change_in_Males_2010_to_2014 <- (all_data$Percent..SEX.AND.AGE..Total.population..Male.2014- all_data$Percent..SEX.AND.AGE..Male.2010)/all_data$Percent..SEX.AND.AGE..Male.2010*100
all_data$Percent_Change_in_Whites_2010_to_2014 <- (all_data$Percent..RACE..Race.alone.or.in.combination.with.one.or.more.other.races..Total.population..White.2014- all_data$Percent..RACE..White.2010)/all_data$Percent..RACE..White.2010*100

all_data <- all_data[!is.na(all_data$Percent_Change_in_Recent_Move_In_2010_to_2014),]
all_data <- all_data[!is.na(all_data$Percent_Change_in_Plumbing_2010_to_2014),]
all_data <- all_data[!is.na(all_data$Percent_Change_in_Large_Households_2010_to_2014),]
all_data <- all_data[!is.na(all_data$Percent_Change_in_Children_2010_to_2014),]
all_data <- all_data[!is.na(all_data$Percent_Change_in_Rent_2010_to_2014),]
all_data <- all_data[!is.na(all_data$Percent_Change_in_Income_2010_to_2014),]
all_data <- all_data[!is.na(all_data$Percent_Change_in_Vacant_Housing_Units_2010_to_2014),]
all_data <- all_data[!is.na(all_data$Percent_Change_in_Occupied_Housing_Units_2010_to_2014),]
all_data <- all_data[!is.na(all_data$Percent_Change_in_Total_Housing_Units_2010_to_2014),]
all_data <- all_data[!is.na(all_data$Percent_Change_in_Homeowner_Vacancy_Rate_2010_to_2014),]
all_data <- all_data[!is.na(all_data$Percent_Change_in_Renter_Occupied_Housing_Tenure_2010_to_2014),]
all_data <- all_data[!is.na(all_data$Percent_Change_in_No_Vehicles_Available_2010_to_2014),]
all_data <- all_data[!is.na(all_data$Percent_Change_in_No_Telephone_Service_Availble_2010_to_2014),]
all_data <- all_data[!is.na(all_data$Percent_Change_in_Males_2010_to_2014),]
all_data <- all_data[!is.na(all_data$Percent_Change_in_Whites_2010_to_2014),]


all_data <- all_data[!is.infinite(all_data$Percent_Change_in_Recent_Move_In_2010_to_2014),]
all_data <- all_data[!is.infinite(all_data$Percent_Change_in_Plumbing_2010_to_2014),]
all_data <- all_data[!is.infinite(all_data$Percent_Change_in_Large_Households_2010_to_2014),]
all_data <- all_data[!is.infinite(all_data$Percent_Change_in_Children_2010_to_2014),]
all_data <- all_data[!is.infinite(all_data$Percent_Change_in_Rent_2010_to_2014),]
all_data <- all_data[!is.infinite(all_data$Percent_Change_in_Income_2010_to_2014),]
all_data <- all_data[!is.infinite(all_data$Percent_Change_in_Vacant_Housing_Units_2010_to_2014),]
all_data <- all_data[!is.infinite(all_data$Percent_Change_in_Occupied_Housing_Units_2010_to_2014),]
all_data <- all_data[!is.infinite(all_data$Percent_Change_in_Total_Housing_Units_2010_to_2014),]
all_data <- all_data[!is.infinite(all_data$Percent_Change_in_Homeowner_Vacancy_Rate_2010_to_2014),]
all_data <- all_data[!is.infinite(all_data$Percent_Change_in_Renter_Occupied_Housing_Tenure_2010_to_2014),]
all_data <- all_data[!is.infinite(all_data$Percent_Change_in_No_Vehicles_Available_2010_to_2014),]
all_data <- all_data[!is.infinite(all_data$Percent_Change_in_No_Telephone_Service_Availble_2010_to_2014),]
all_data <- all_data[!is.infinite(all_data$Percent_Change_in_Males_2010_to_2014),]
all_data <- all_data[!is.infinite(all_data$Percent_Change_in_Whites_2010_to_2014),]

census_tracts = list(
  'Census Tract 4, Fulton County, Georgia',
  'Census Tract 5, Fulton County, Georgia',
  'Census Tract 6, Fulton County, Georgia',
  'Census Tract 7, Fulton County, Georgia',
  'Census Tract 10.01, Fulton County, Georgia',
  'Census Tract 10.02, Fulton County, Georgia',
  'Census Tract 12.02, Fulton County, Georgia',
  'Census Tract 18, Fulton County, Georgia',
  'Census Tract 19, Fulton County, Georgia',
  'Census Tract 21, Fulton County, Georgia',
  'Census Tract 26, Fulton County, Georgia',
  'Census Tract 118, Fulton County, Georgia')

near_atl_data = all_data[all_data$Geographic.Area.Name.x %in% census_tracts, ]

