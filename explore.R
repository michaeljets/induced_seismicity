setwd("~/Stat_157/induced_seismicity")

library(dplyr)
library(stringr)

library(maps)
library(maptools)

library(sp)  # vector data
library(raster)  # raster data
library(rgdal)  # input/output, projections
library(rgeos)  # geometry ops
library(spdep)  # spatial dependence

source('rcomcat/searchcomcat.R')

stime = as.POSIXct("1970-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
etime = as.POSIXct("2017-10-16 23:59:59", format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

ca_dat = comcathypocsv(minmagnitude = 2,
                    starttime = stime,
                    endtime = etime,
                    minlatitude = 32.495,
                    maxlatitude = 42.183,
                    minlongitude = -124.805,
                    maxlongitude = -113.994,
                    eventtype = "earthquake")
ca_dat_store = ca_dat

ok_dat = comcathypocsv(minmagnitude = 2,
                       starttime = stime,
                       endtime = etime,
                       minlatitude = 33.406,
                       maxlatitude = 37.171,
                       minlongitude = -103.14,
                       maxlongitude = -94.043,
                       eventtype = "earthquake")
ok_dat_store = ok_dat

# write.csv(ca_dat, file="rawdata/earthquakes/california/ca_eq_raw.csv")
# write.csv(ok_dat, file="rawdata/earthquakes/oklahoma/ok_eq_raw.csv")


# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees

latlong2county <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  # states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  counties <- map('county', 'california', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get indices of the Polygons object containing each point 
  indices <- over(pointsSP, counties_sp)
  
  # Return the state names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  countyNames[indices]
}

ca_dat = ca_dat %>%
        mutate(longitude = as.numeric(as.character(longitude)),
               latitude = as.numeric(as.character(latitude)))
ca_dat$county = latlong2county(ca_dat[ , c('longitude','latitude')])
kern_dat=subset(ca_dat, ca_dat$county=='california,kern')

# write.csv(kern_dat, file="rawdata/earthquakes/california/kern_eq.csv")

kern_dat = kern_dat %>% 
            mutate(time_year = as.numeric(substr(time, 1, 4))) %>%
            mutate(time_dec = floor(time_year/10)*10)
hist(kern_dat$time_year)
        
x = get_map(location = c(lon = -119.022102, lat = 35.3733), zoom = 8)
ggmap(x) +
  geom_point(aes(x=longitude, y=latitude, col=as.factor(time_dec)), data = kern_dat)

