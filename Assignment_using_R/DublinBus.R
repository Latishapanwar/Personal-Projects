library(readr)
library(dplyr)

#download the dataset from the internet, unzip it and view the files
download.file('https://data.dublinked.ie/dataset/a97edfe6-1ee2-494c-9998-c7ab29214d59/resource/e95bd0b4-1ac3-471d-93ea-2d129c8e8dfe/download/googletransitdublinbusp20130315-1546.zip', 
              destfile = 'DublinBusGTFS.zip')
unzip('DublinBusGTFS.zip', exdir = './DublinBusGTFSData')
list.files('DublinBusGTFSData')

#importing the files to R
agency <- read_csv('./DublinBusGTFSData/agency.txt')
calendar <- read_csv('./DublinBusGTFSData/calendar.txt')
calendar.dates <- read_csv('./DublinBusGTFSData/calendar_dates.txt')
routes <- read_csv('./DublinBusGTFSData/routes.txt')
shapes <- read_csv('./DublinBusGTFSData/shapes.txt')
stop.times <- read_csv('./DublinBusGTFSData/stop_times.txt')
stops <- read_csv('./DublinBusGTFSData/stops.txt')
transfers <- read_csv('./DublinBusGTFSData/transfers.txt')
trips <- read_csv('./DublinBusGTFSData/trips.txt')

#Shaping the data frames for the necessary files
#As all the entries under the variables 'trip_headsign' and 'block_id' are NAs, hence ignoring these columns
trips <- trips %>%
  select(-c(trip_headsign, block_id)) %>%
  group_by(route_id)

#Grouping by stop name for the table Stops
stops <- group_by(stops, stop_name)

#Entries under 'departure_time' are exactly same as entries under 'arrival_time', 
#hence ignoring departure_time. Also, there is no change in the pickup type and dropoff type 
#for all the entries, hence removing these table too from the data frame.
stop.times <- select(stop.times, -c(departure_time, pickup_type, drop_off_type))
#stop.distances <- stoptimes %>%
#  group_by(trip_id, stop_headsign) %>%
#  summarise(max_shape_traveled=max(shape_dist_traveled)) %>%
#  ungroup()

#Grouping data by 'shape_id' and finding maximum distance/ shape travelled
shapes <- group_by(shapes, shape_id)
dist.shape.covered <- summarise(shapes, total_dist_traveled = max(shape_dist_traveled))

#Removing 'agency_id' and 'route_type' as they do not provide critical information about the data
routes <- select(routes, -c(agency_id, route_type))

#Removing 'start_date' and 'end_date' as it is not useful in the analysis
calendar <- select(calendar, -c(start_date, end_date))

stoptimes <- inner_join(stops, stop.times, by = 'stop_id')
shapes.trips <- inner_join(dist.shape.covered, trips, by = 'shape_id')
trips.routes <- inner_join(routes, shapes.trips, by = 'route_id')
trips.stoptimes <- inner_join(stoptimes, trips.routes, by = 'trip_id')
detailed.trips.frame <- inner_join(calendar, trips.stoptimes, by = 'service_id')

#EXPLORATORY DATA ANALYSIS

#how many stops are there in Dublin?
tot.stops <- length(unique(stops$stop_id))
cat("Total number of stops in Dublin", tot.stops)

#how many stops are there in each area?
stops.count <- stops %>%
  summarise(total_stops = sum(!is.na(stop_id))) %>%
  arrange(desc(total_stops))
cat("Total number of stops in each area")
head(stops.count)

#how many buses can be accessed from a particular area?
bus.count.by.area <- stoptimes %>%
  summarise(no_of_buses_accessible = sum(!is.na(stop_name))) %>%
  arrange(desc(no_of_buses_accessible))
cat("Number of buses available from each area")
head(bus.count.by.area)


#minimum and maximum distance covered by every route?
trips.routes.grp1 <- group_by(trips.routes, route_id, route_long_name)
route.travelled <- summarise(trips.routes.grp1, min_dist_traveled = min(total_dist_traveled), max_dist_traveled = max(total_dist_traveled))
arrange(route.travelled, desc(max_dist_traveled))
cat("Minimum and Maximum distance travelled by bus on each route")
head(route.travelled)

#trips made per route
trips.routes.grp2 <- group_by(trips.routes, route_id, route_short_name, route_long_name)
trips.per.route <- summarise(trips.routes.grp2, total.trips.made = sum(!is.na(trip_id)))
ggplot(data = trips.routes) + geom_bar(mapping = aes(x = route_short_name)) + labs(x = "Routes(by short name)", y = "Trips made per route") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#trips made for each service
qplot(sort(service_id), data = trips.routes, xlab = "Services", ylab = "No. of Trips", 
      geom = 'bar', main = "Trips covered for each service", colour = I('cyan'))

#3.2
#Route = 39A
route39A <- filter(trips.stoptimes, route_short_name == '39A')

#how many buses arrives/ departs from each area?
route39A.grp1 <- route39A %>%
  group_by(stop_name) %>%
  summarise(no.of.buses = sum(!is.na(trip_id)))
ggplot(data = route39A.grp1) + geom_col(aes(x = reorder(stop_name, -no.of.buses), 
                                            y = no.of.buses), fill = "dodgerblue4") + labs(x = "Name of Stops", y = "Number of Buses", title = "Number of 39A buses arriving/ departing in each area") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#data frames based on directions(outbound(1)-going towards college/ inbound(0)-coming towards home)
route39A.dir1 <- route39A %>%
  filter(direction_id == 1) %>%
  arrange(arrival_time)
route39A.dir0 <- route39A %>%
  filter(direction_id == 0) %>%
  arrange(arrival_time)

#Filtering out particular stop that is 'Navan Road'(stop_sequence 32) from where I take bus to college 
stop32 <- route39A.dir1 %>%
  filter(stop_sequence == 32) %>%
  arrange(arrival_time)
#changing the format of 'arrival_time' variable
stop32$arrival_time <- format(stop32$arrival_time, "%H:%M:%S")
#How many buses go towards college
cat("Number of buses going towards college")
length(unique(stop32$trip_id))


#Filtering out particular stop that is 'Aston Quay'(stop_sequence 22) from where I take bus to college 
stop22 <- route39A.dir0 %>%
  filter(stop_sequence == 22) %>%
  arrange(arrival_time)
stop22$arrival_time <- format(stop22$arrival_time, "%H:%M:%S")
#How many buses come in the direction of my home when coming back from college?
length(unique(stop22$trip_id))


#Frequency of buses before 11am while going towards college
y <- "11:00:00"
bus.avl.32 <- strptime(stop32$arrival_time, "%H:%M:%S") < strptime(y, "%H:%M:%S")
sum(bus.avl.32)
#first 39A bus at my stop
min(stop32$arrival_time)

#Frequency of buses after 6pm when coming back home in the evening
x <- "18:00:00"
bus.avl.22 <- strptime(stop22$arrival_time, "%H:%M:%S") > strptime(x, "%H:%M:%S")
sum(bus.avl.22)
#last bus when coming back from college
max(stop22$arrival_time)

#Time taken to reach college
#Mostly, I take bus around 9.30am, so I am chosing a trip_id for this '3110.2978.0-39A-b12-1.247.I' from stop_sequence 32
# and I get down at stop_sequence 50 and stop no. 1479
time.to.reach <- (route39A.dir1$stop_sequence == 32 | route39A.dir1$stop_sequence == 50) & 
  route39A.dir1$trip_id == '3110.2978.0-39A-b12-1.247.I'
time.to.reach.stp50 <- route39A.dir1[time.to.reach, ]
time.to.reach.stp50$arrival_time <- format(time.to.reach.stp50$arrival_time, "%H:%M:%S")
max.time <- max(time.to.reach.stp50$arrival_time) 
min.time <- min(time.to.reach.stp50$arrival_time)
total.time.taken <- strptime(max.time, "%H:%M:%S") - strptime(min.time, "%H:%M:%S")
round(total.time.taken)

#time taken to come back home
#Mostly, I take bus around 7 pm, so I am chosing a trip_id for this '10039.2955.0-39A-b12-1.244.O' from stop_sequence 32
# and I get down at stop_sequence 50 and stop no. 1479
time.to.reach.back <- (route39A.dir0$stop_sequence == 22 | route39A.dir0$stop_sequence == 39) & 
  route39A.dir0$trip_id == '10039.2955.0-39A-b12-1.244.O'
time.to.reach.stp39 <- route39A.dir0[time.to.reach.back, ]
time.to.reach.stp39$arrival_time <- format(time.to.reach.stp39$arrival_time, "%H:%M:%S")
max.timeb <- max(time.to.reach.stp39$arrival_time) 
min.timeb <- min(time.to.reach.stp39$arrival_time)
tot.time.taken <- strptime(max.timeb, "%H:%M:%S") - strptime(min.timeb, "%H:%M:%S")
round(tot.time.taken)

#How many stops are there in my route
df.filtered.route <- route39A.dir1 %>%
  filter(trip_id == '3110.2978.0-39A-b12-1.247.I') %>%
  arrange(stop_sequence)
all.stops <- between(df.filtered.route$stop_sequence, 32, 50)
all.stops <- sum(all.stops)
total.stops <- all.stops - 2

#Average distance travelled per trip
avg.dist <- round(mean(trips.stoptimes$total_dist_traveled))

#
route39A.grp3 <- group_by(route39A, trip_id)

route39A.dir0<- route39A.dir0 %>%
  group_by(trip_id) %>%
  mutate(start_time=min(arrival_time),end_time=max(arrival_time))
route39A.dir0$start_time<-as.POSIXct(route39A.dir0$start_time,format = "%H:%M:%S")
route39A.dir0$end_time<-as.POSIXct(route39A.dir0$end_time,format = "%H:%M:%S")
route39A.dir0<- route39A.dir0 %>%
  group_by(trip_id) %>%
  mutate(time_taken=difftime(end_time,start_time,units="mins"))
route39A.dir0$time_taken<-as.numeric(route39A.dir0$time_taken)

route39A.dir1<- route39A.dir1 %>%
  group_by(trip_id) %>%
  mutate(start_time=min(arrival_time),end_time=max(arrival_time))
route39A.dir1$start_time<-as.POSIXct(route39A.dir1$start_time,format = "%H:%M:%S")
route39A.dir1$end_time<-as.POSIXct(route39A.dir1$end_time,format = "%H:%M:%S")
route39A.dir1<- route39A.dir1 %>%
  group_by(trip_id) %>%
  mutate(time_taken=difftime(end_time,start_time,units="mins"))
route39A.dir1$time_taken<-as.numeric(route39A.dir1$time_taken)

#graph to plot variation in time
cat("Red line indicates time taken when going to college and Blue line indicates time taken by bus when coming back home")
ggplot()+geom_smooth(data=route39A.dir0,aes(x=start_time,y=time_taken),na.rm=T,colour="blue")+geom_smooth(data=route39A.dir1,aes(x=start_time,y=time_taken),na.rm=T,colour="red")+labs(title="Variation of total time taken during different point of time",x="Start Time",y="Total Time Taken")
