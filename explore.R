setwd("~/Stat_157/induced_seismicity")

library(dplyr)
library(stringr)
library(raster)
library(rgdal)
library(sp)
library(maps)
library(maptools)


# dat = data.frame()
# start_times = paste0(seq(2010, 1970, by = -5), '-01-01')
# end_times = c('2017-10-14', start_times[1:8])
# for (i in 1:length(times)){
#   str1 = "https://earthquake.usgs.gov/fdsnws/event/1/query.csv?starttime="
#   start = start_times[i]
#   str2 = "%2000:00:00&endtime="
#   end = end_times[i]
#   str3 = "%2023:59:59&maxlatitude=42.183&minlatitude=32.495&maxlongitude=-113.994&minlongitude=-124.805&minmagnitude=2.0&orderby=time"
#   print(time)
#   dat = bind_rows(dat, read.csv(url(paste0(str1, start, str2, end, str3))))
# }

dat1 = read.csv('ca_eq1.csv')
dat2 = read.csv('ca_eq2.csv')
dat3 = read.csv('ca_eq3.csv')
dat4 = read.csv('ca_eq4.csv')
dat5 = read.csv('ca_eq5.csv')
dat6 = read.csv('ca_eq6.csv')

full_dat = rbind(dat1,dat2,dat3,dat4,dat5,dat6)
full_dat = full_dat %>% distinct()

# write.csv(full_dat, 'ca_eq_full.csv', row.names = F)

# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees

latlong2state <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get indices of the Polygons object containing each point 
  indices <- over(pointsSP, states_sp)
  
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}

full_dat$state = latlong2state(full_dat[c('longitude','latitude')])
ca_dat=subset(full_dat, full_dat$state=='california') 


ca_dat = ca_dat %>%
  filter(type == 'earthquake')

ca_dat = ca_dat %>% mutate(time_year = as.numeric(substr(time, 1, 4)))
hist(ca_dat$time_year)





