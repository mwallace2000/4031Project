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
all_data$Percent_Change_in_Median_Home_Value_2010_to_2018 <- (all_data$Estimate..VALUE..Owner.occupied.units..Median..dollars..2018 - all_data$Estimate..VALUE..Median..dollars..2010)/(all_data$Estimate..VALUE..Median..dollars..2010)*100

all_data <- all_data[!is.na(all_data$Percent_Change_in_Median_Home_Value_2010_to_2018),]
all_data <- all_data[!is.infinite(all_data$Percent_Change_in_Median_Home_Value_2010_to_2018),]

g = lm(all_data$Percent_Change_in_Median_Home_Value_2010_to_2018~all_data$Estimate..UNITS.IN.STRUCTURE..Total.housing.units.2010, na.action=na.exclude)
summary(g)

names = names(all_data)
good_names1 = names[grepl('Percent..HOUSING.OCCUPANCY..Vacant.housing.units',names,fixed=TRUE)]
good_names2 = names[grepl('Estimate..HOUSING.OCCUPANCY..Homeowner.vacancy.rate',names,fixed=TRUE)]
good_names3 = names[grepl('Estimate..HOUSING.OCCUPANCY..Total.housing.units',names,fixed=TRUE)]
good_names4 = names[grepl('Percent..HOUSING.OCCUPANCY..Occupied.housing.units',names,fixed=TRUE)]
good_names5 = names[grepl('Percent..HOUSING.TENURE..Renter.occupied',names,fixed=TRUE)]
good_names6 = names[grepl('Percent..VEHICLES.AVAILABLE..No.vehicles.available',names,fixed=TRUE)]
good_names7 = names[grepl('Percent..SELECTED.CHARACTERISTICS..No.telephone.service.available',names,fixed=TRUE)]
good_names8 = names[grepl('Percent..SEX.AND.AGE..Male',names,fixed=TRUE)]
good_names9 = names[grepl('Percent..UNITS.IN.STRUCTURE..1.unit..detached',names,fixed=TRUE)]
good_names10 = names[grepl('Estimate..GROSS.RENT..No.rent.paid',names,fixed=TRUE)]
good_names11 = names[grepl('Percent..YEAR.STRUCTURE.BUILT..Built.2005.or.later',names,fixed=TRUE)]
good_names12 = names[grepl('Estimate..ROOMS..Median.rooms',names,fixed=TRUE)]
good_names13 = names[grepl('Estimate..HOUSING.TENURE..Average.household.size.of.owner.occupied.unit',names,fixed=TRUE)]
good_names14 = names[grepl('Percent..YEAR.HOUSEHOLDER.MOVED.INTO.UNIT..Moved.in.2000.to.2004',names,fixed=TRUE)]
good_names15 = names[grepl('Percent..HOUSE.HEATING.FUEL..Utility.gas',names,fixed=TRUE)]
good_names16 = names[grepl('Percent..SELECTED.CHARACTERISTICS..Lacking.complete.plumbing.facilities',names,fixed=TRUE)]
good_names17 = names[grepl('Renter.occupied.housing.units..Estimate..HOUSEHOLD.SIZE..4.or.more.person.household',names,fixed=TRUE)]
good_names18 = names[grepl('Renter.occupied.housing.units..Estimate..FAMILY.TYPE.AND.PRESENCE.OF.OWN.CHILDREN..No.related.children.under.18.years',names,fixed=TRUE)]
good_names19 = names[grepl('Estimate..VALUE..Median..dollars',names,fixed=TRUE)]
good_names20 = names[grepl('Estimate..VALUE..Owner.occupied.units..Median..dollars',names,fixed=TRUE)]
good_names21 = names[grepl('Estimate..GROSS.RENT..Median..dollars',names,fixed=TRUE)]
good_names22 = names[grepl('Occupied.housing.units..Estimate..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2015.INFLATION.ADJUSTED.DOLLARS...Median.household.income..dollars',names,fixed=TRUE)]
good_names = c(good_names1,good_names2,good_names3,good_names4,good_names5,good_names6,good_names7,
               good_names8,good_names9,good_names10,good_names11,good_names12,good_names13,good_names14,
               good_names15,good_names16,good_names17,good_names18,good_names19,good_names20,good_names21, good_names22)

ix <- which(names %in% good_names)
all_data <- all_data[,ix]