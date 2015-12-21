##Divvy bike data munge and incorporate google data
setwd("~/Documents/Documents/Projects/Divvy")

Divvy_Trips_2015.Q1 <- read.csv("~/Google Drive/Documents/Projects/Divvy/data/Divvy_Trips_2015-Q1.csv")
Divvy_Trips_2014.Q3.07 <- read.csv("~/Google Drive/Documents/Projects/Divvy/data/Divvy_Trips_2014-Q3-07.csv")

df <- head(Divvy_Trips_2015.Q1,10000)
df <- Divvy_Trips_2015.Q1
df <- Divvy_Trips_2014.Q3.07
library(plyr)
library(dplyr)
library(lubridate)
library(binhf)
# http://s4rdd.blogspot.com/2012/12/google-maps-api-decoding-polylines-for.html
library(maptools)
library(maps)
library(mapdata)
library(rjson)
library(stringr)
library(RCurl)
library(ggplot2)

df$starttimePosix  <- mdy_hm(df$starttime) 
df$stoptimePosix  <- mdy_hm(df$stoptime)
df$startdatePosix  <- as.Date(df$starttime,format="%m/%d/%Y %H:%M") 
df$stopdatePosix  <- as.Date(df$stoptime,format="%m/%d/%Y %H:%M")
df$weekday <- wday(df$starttimePosix)
##Pick a day and get a random bike##
#How many stations did they go to

##Some interesting queries
df %>% group_by(starttimePosix) %>% summarise(Total=n())
df %>% group_by(startdatePosix) %>% summarise(Total=n(),Sum = (length(unique(bikeid)),Another=))) 
df %>% filter(startdatePosix == as.Date("2015-01-02")) %>% group_by(bikeid) %>% summarise(Total=n()) %>% filter(Total > 5) 

###Lets filter down only long bike rides
longbikerides <- df %>% group_by(startdatePosix,bikeid) %>% summarise(Total=n()) %>% filter(Total >= 10) 
df %>% filter(longbikerides$startdatePosix == startdatePosix & longbikerides$bikeid == bikeid)
total <- NULL
for (n in 1:nrow(longbikerides))
{temp <- subset(df,longbikerides$startdatePosix[n] == startdatePosix & longbikerides$bikeid[n] == bikeid)
temp <- temp %>% arrange((starttimePosix)) %>% mutate(key = (1:nrow(temp)-1))
total <- rbind(total,temp)
#print(n)
}
  
##Lets focus on July 1 (decided this later, should move this up earlier)
total <- subset(total, startdatePosix == "2014-07-01")


###Need to do joins to get lat/long for stations:
colnames(Divvy_Stations_2015)[1]<-"from_station_id"
library(plyr)
c <- join(total, Divvy_Stations_2015, by="from_station_id")
colnames(c)[19] <- "pickupy"
colnames(c)[20] <- "pickupx"
colnames(Divvy_Stations_2015)[1]<-"to_station_id"
d <- join(c, Divvy_Stations_2015, by="to_station_id")
colnames(d)[24] <- "dropoffy"
colnames(d)[25] <- "dropoffx"
d$medallion <- paste(d$bikeid,day(d$starttimePosix),sep = "_")
colnames(d)[2] <- "pickuptime"
colnames(d)[3] <- "dropofftime"
finaldf <- subset(d, select = c(pickuptime,dropofftime,medallion,tripduration,usertype,gender,
                                birthyear,pickupy,pickupx,dropoffy,dropoffx,key))
finaldf$fare <- 1
finaldf$passengers <- 1
df <- finaldf

##Adds it into a google friendly format
df$origin <- paste(df$pickupy,df$pickupx,sep=",")
df$destination <- paste(df$dropoffy,df$dropoffx,sep=",")
travelMode <- "driving"

df$pickuptime <- as.character(df$pickuptime)
df$nextpickuptime <- shift(df$pickuptime,1,dir="left")
df$removekey <- shift(df$key,1,dir="left")
#df$nextlocation <- shift(df$origin,1,dir="left")

# build the URL
baseUrl <- "https://maps.googleapis.com/maps/api/directions/json?"
df$origin <- gsub(" ", "+", df$origin)
df$destination <- gsub(" ", "+", df$destination)

##Limit it about 30 trips 
df <- head(df, 320)
df$nextpolyline <- 99
for (n in 1:nrow(df))
{
finalUrl <- paste(baseUrl
                  , "origin=", df$origin[n]
                  , "&destination=", df$destination[n]
                  , "&sensor=false"
                  , "&mode=", travelMode
                  #, "&departure_time=", as.integer(departureTime)
                  , "&key=AIzaSyDq1YfyGZI4gH328ssCGj5GwxiM1Dwtsrs"
                  , sep = "")

# get the JSON returned by Google and convert it to an R list
url_string <- URLencode(finalUrl)
trip <- fromJSON(paste(readLines(url_string), collapse = ""))
df$trippolyline[n] <- trip$routes[[1]]$overview_polyline$points

if (df$destination[n] == df$origin[n+1]) {df$nextpolyline[n] <- df$trippolyline[n+1]
print ("0")
} else {
  finalUrl <- paste(baseUrl
                    , "origin=", df$destination[n]
                    , "&destination=", df$origin[n+1]
                    , "&sensor=false"
                    , "&mode=", travelMode
                    #, "&departure_time=", as.integer(departureTime)
                    , "&key=AIzaSyDq1YfyGZI4gH328ssCGj5GwxiM1Dwtsrs"
                    , sep = "")
  
  # get the JSON returned by Google and convert it to an R list
  url_string <- URLencode(finalUrl)
  trip <- fromJSON(paste(readLines(url_string), collapse = ""))
  df$nextpolyline[n] <- trip$routes[[1]]$overview_polyline$points
  Sys.sleep(2)
}


}

##Set final trip points to 0
df$nextpickuptime<-ifelse(df$removekey==0,
                           0, 
                           df$nextpickuptime)

df$nextpolyline<-ifelse(df$removekey==0,
                          0, 
                          df$nextpolyline)

df$origin <- NULL
df$destination <- NULL
df$removekey  <- NULL
df$newdate<- NULL
write.csv(df,"workingdf.csv",row.names = FALSE)


