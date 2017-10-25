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

library(ggmap)
# citation('ggmap')

library(spatstat)

source('rcomcat/searchcomcat.R')

stime = as.POSIXct("1980-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
etime = as.POSIXct("2017-10-16 23:59:59", format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

ca_dat = read.csv("rawdata/earthquakes/california/ca_eq_raw.csv")

# ca_dat = comcathypocsv(minmagnitude = 3,
#                     starttime = stime,
#                     endtime = etime,
#                     minlatitude = 32.495,
#                     maxlatitude = 42.183,
#                     minlongitude = -124.805,
#                     maxlongitude = -113.994,
#                     eventtype = "earthquake")
# ca_dat_store = ca_dat

# ok_dat = comcathypocsv(minmagnitude = 2,
#                        starttime = stime,
#                        endtime = etime,
#                        minlatitude = 33.406,
#                        maxlatitude = 37.171,
#                        minlongitude = -103.14,
#                        maxlongitude = -94.043,
#                        eventtype = "earthquake")
# ok_dat_store = ok_dat

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


latlong2state <- function(pointsDF) {
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  indices <- over(pointsSP, states_sp)
  
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}


ca_dat = ca_dat %>%
        mutate(longitude = as.numeric(as.character(longitude)),
               latitude = as.numeric(as.character(latitude)),
               time_year = as.numeric(substr(time, 1, 4)),
               time_dec = floor(time_year/10)*10,
               state = latlong2state(data.frame(longitude, latitude))) %>%
        filter(state == 'california')

graphics::hist(ca_dat$time_year, breaks = 1980:2017)
plot(x=1980:2017, y=table(ca_dat$time_year), type = 'b')

# ca_dat$county = latlong2county(ca_dat[ , c('longitude','latitude')])
# kern_dat=subset(ca_dat, ca_dat$county=='california,kern')

# write.csv(kern_dat, file="rawdata/earthquakes/california/kern_eq.csv")


# kern_dat = kern_dat %>% 
#             mutate(time_year = as.numeric(substr(time, 1, 4))) %>%
#             mutate(time_dec = floor(time_year/10)*10)
# hist(kern_dat$time_year)
#         
# x = get_map(location = c(lon = -119.022102, lat = 35.3733), zoom = 8)
# ggmap(x) +
#   geom_point(aes(x=longitude, y=latitude, col=as.factor(time_dec)), data = kern_dat)

cal_sp = SpatialPointsDataFrame(ca_dat[, c("longitude", "latitude")],
                                 data.frame(ID=seq(1:nrow(ca_dat))),
                                 proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
cal_eq_points = spTransform(cal_sp, CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84" ))
cal_eq_points_df = data.frame(cal_eq_points)
cal_eq_points_df$time = ca_dat$time_year

california = map_data("state") %>% filter(region == 'california')
california_sp = SpatialPointsDataFrame(california[, c("long", "lat")], 
                                       data.frame(ID=seq(1:nrow(california))), 
                                       proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
california_points = spTransform(california_sp, CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84" ))
california_points_df = data.frame(california_points)
california_points_df$group = california$group


ca_wells = readOGR(dsn = 'rawdata/wells/california/shapefiles/AllWells',
                   layer = "AllWells_20170316")
ca_wells_points = spTransform(ca_wells, CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84" ))
ca_wells_df = data.frame(ca_wells_points)

ca_wells_df2 = ca_wells_df %>%
                filter(WellStatus == 'A',
                       Latitude != 0 | Longitude != 0)

ggplot() + 
  geom_polygon(data=california_points_df, aes(x=long,y=lat,group=group)) +
  geom_point(data=cal_eq_points_df, aes(x=longitude,y=latitude, color = time)) + 
  geom_point(data=ca_wells_df2, aes(x=coords.x1,y=coords.x2), color = 'red')
