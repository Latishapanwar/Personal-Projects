library(httr)
library(ggplot2)

url_api = 'https://api.jcdecaux.com/vls/v1/stations?'
response = GET(url_api, query = 
                 list(apiKey = '537eb2f907c103123b3b0bc9c53840771c9cb529', 
                      contract = 'dublin'))
bike_info = content(response)

bikeData = data.frame(NULL)
len_bike = length(bike_info)

for (i in 1: len_bike){
  df = data.frame(bike_info[i])
  bikeData = rbind(bikeData, df)
}
View(bikeData)

#2.1
#Creating data frame for only the columns of interest
bike_overview <- bikeData[ , c("name", "address", "banking", "bike_stands", 
                               "available_bike_stands", "available_bikes")]
View(bike_overview)

#total number of bike stations
tot.bke.statn <- nrow(bike_overview)

#how many stations has banking facility available and how many do not
statn.with.bankfacility <- sum(bike_overview$banking == "TRUE")
statn.without.bankfacility <- sum(bike_overview$banking == "FALSE")

#total number of bike stands across all the bike stations
tot.bke.stnds <- sum(bike_overview$bike_stands)

#available number of bike stands across all stations
tot.avl.bke.stnds <- sum(bike_overview$available_bike_stands)

#bikes available 
tot.bkes.avl <- sum(bike_overview$available_bikes)

#Average available stands and bikes
avg.available.stands <- round(mean(bike_overview$available_bike_stands))
avg.available.bikes <- round(mean(bike_overview$available_bikes))

#To put all the information collected above in one data frame
Information <- c("Total bike stations in Dublin", "Number of stations with banking facility", 
                 "Number of stations without banking facility", "Total bike stands across all bike stations",
                 "Available bike stands", "Available bikes", "Average available stands", "Average available bikes")

Count <- c(nrow(bike_overview), statn.with.bankfacility, statn.without.bankfacility, 
           tot.bke.stnds, tot.avl.bke.stnds, tot.bkes.avl, avg.available.stands, avg.available.bikes)

Summary_dub_bikes <- data.frame(Information, Count)
View(Summary_dub_bikes)

#Finding the busiest bike stations(According to Dublin app if there is less or no space available to park bikes, then it is considered busy)#

busy.bke.statn <- bike_overview$available_bike_stands < 5
busy.bke.statn.df <- bike_overview[busy_bke_statn, ]
View(busy.bke.statn.df)

#2.2

bike.info <- filter(bike_overview, available_bike_stands > 12 & available_bikes > 15)
qplot(name, available_bikes, data = bike.info, colour = banking)

qplot(name, available_bikes, data = bike.info, xlab = "Name of Stations", ylab = "Number of bikes available", colour = banking)
