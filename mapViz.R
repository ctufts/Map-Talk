rm(list = ls())
##################### Import Libraries ##############################################
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(leaflet)
library(stringr)
library(rgdal)
library(readr)
source('fte_theme.R')
source('multiplot.R')

######################### Import data And Clean Up######################################
# import the first 50k rows of the 2012-2014 crime data
ds <- read_csv('https://data.phila.gov/api/views/7ret-xhtc/rows.csv?accessType=DOWNLOAD', n_max = 50000)
# rename column 3 and 7
names(ds)[3] <- 'Dispatch.Date.Time'
names(ds)[7] <- 'General.Crime.Category'

# remove uncategorized crimes
ds <- filter(ds, General.Crime.Category != 'All Other Offenses' )
# change label for DUI's
ds$General.Crime.Category[ds$General.Crime.Category == "DRIVING UNDER THE INFLUENCE"] <- "DUI"
# convert date time to a datetime
# and the date to a date object
ds$Dispatch.Date.Time <- mdy_hms(ds$Dispatch.Date.Time)
ds$Dispatch.Date <- as.Date(ds$Dispatch.Date.Time)




# remove parentheses and commas from coordinates

ds$Coordinates<- gsub(")", "", ds$Coordinates)
ds$Coordinates<- gsub("\\(", "", ds$Coordinates)
coord.list <- (str_split(ds$Coordinates, ","))
# break into long/lat coordinates
y <- as.numeric(unlist(lapply(coord.list, function(x)x[1])))
x <- as.numeric(unlist(lapply(coord.list, function(x)x[2])))
# create data frame with coordiantes, dates, districts and crime category
coordinates <- data.frame(POINT_Y = y,
                          POINT_X = x,
                          General.Crime.Category = ds$General.Crime.Category,
                          District = ds$District,
                          Dispatch.Date = ds$Dispatch.Date.Time
)
# remove all rows missing coordinates
coordinates <- na.omit(coordinates)
# 22 and 23 districts were merged a few years back
coordinates$District[coordinates$District==23] <- 22


############# Import Shape Data for Police Districts #######################
# this snippet was taken from:
# http://zevross.com/blog/2014/04/11/using-r-to-quickly-create-an-interactive-online-map-using-the-leafletr-package/

url<-"http://data.phl.opendata.arcgis.com/datasets/62ec63afb8824a15953399b1fa819df2_0.zip"
downloaddir<-getwd()
destname<-"districts.zip"
download.file(url, destname)
unzip(destname, exdir=downloaddir, junkpaths=TRUE)

filename<-list.files(downloaddir, pattern=".shp", full.names=FALSE)
filename<-gsub(".shp", "", filename)

districts<-readOGR(downloaddir, filename) 

############################ calculate the number of events in each district###############
# group by crime
district.log <- coordinates %>% group_by(General.Crime.Category, District) %>%
  summarise(
    events = n()
  )

############################Draw Map##################################
# Only view arson events
crime.type.selected <- 'Arson'
# Arsons in each district
# sort by district
district.counts <- filter(district.log, General.Crime.Category == crime.type.selected) %>% 
  arrange(District)

# gather coordinates for this specific crime
point.coord <- filter(coordinates, General.Crime.Category == crime.type.selected)

# create color palette for the districts in the map
pal <- colorNumeric(
  palette = "Greys",
  domain = district.counts$events
)


# create map 
crime.map <- leaflet() %>%
  addProviderTiles('CartoDB.Positron') %>%
  setView(lng=-75.16048, lat=39.99000, zoom =11) %>%
  addPolygons(data = districts,
              stroke = T, smoothFactor = 0.2, fillOpacity = 0.5,
              color = "#000000", weight = 2, 
              fillColor = ~pal(district.counts$events)
              # color = ~pal(states@data$DISTRICT_)
  )%>% 
  addLegend("bottomright", pal = pal, values = district.counts$events,
            title = "Incidents 2012-2014",
            opacity = 1
  )%>%
  addCircles(lat = point.coord$POINT_Y, lng = point.coord$POINT_X,
             fillOpacity = 0.8 , opacity = 0.8, radius = 120, fillColor = "#F0F340",
             color = "#6A727B", weight = 2,
             popup = paste("District:",point.coord$District, "<br/> Time:", point.coord$Dispatch.Date))

# display map
print(crime.map)