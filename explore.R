# setwd("~/Stat_157/induced_seismicity")

library(dplyr)
library(stringr)
library(tidyr)

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


# read california as polygon
california = map_data("state") %>% filter(region == 'california')
california_poly = Polygon(california[ , c("long", "lat")])
california_polys = Polygons(list(california_poly), 1)
california_spoly = SpatialPolygons(list(california_polys))
proj4string(california_spoly) = CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84" )
california_spoly_df = fortify(california_spoly)


ca_wells = readOGR(dsn = 'rawdata/wells/california/shapefiles/AllWells',
                   layer = "AllWells_20170316")
ca_wells_points = spTransform(ca_wells, CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84" ))
ca_wells_df = data.frame(ca_wells_points)

ca_wells_df2 = ca_wells_df %>%
                filter(WellStatus == 'A',
                       Latitude != 0 | Longitude != 0)

# link this wells data with the injection data

inj_wells = read.csv("rawdata/wells/california/raw_injection_wells.csv", 
                     stringsAsFactors = F, colClasses = "character")
inj_wells_store = inj_wells
inj_wells = inj_wells %>%
              dplyr::select(APINumber, CountyName, DaysInjecting,
                     DistrictNumber, InjectionDate, InjectionStatus,
                     MissingDataCode, PoolCode, PoolName,
                     PoolWellTypeStatus, Steam.WaterInjected.BBL.,
                     SurfaceInjPressure, SystemEntryDate, WellNumber,
                     WellStatus, WellTypeCode, Year)
ca_wells_df = ca_wells_df %>%
                mutate_all(funs('as.character')) %>%
                dplyr::select(APINumber, BLMWell, CompDate,
                              ConfWell, coords.x1, coords.x2,
                              County, District, DryHole, Elevation,
                              EPAWell, HydFrac, RedCanFlag,
                              RedrillFt, SpudDate, TotalDepth,
                              WellNumber, WellStatus)
final_wells = inner_join(inj_wells, ca_wells_df, by = c("APINumber")) %>%
                mutate(Year = as.numeric(Year),
                       Steam.WaterInjected.BBL. = as.numeric(Steam.WaterInjected.BBL.),
                       coords.x1 = as.numeric(coords.x1),
                       coords.x2 = as.numeric(coords.x2)) %>%
                rename(WaterInjected = Steam.WaterInjected.BBL.,
                       Longitude = coords.x1,
                       Latitude = coords.x2) %>%
                filter(Year >= 2010,
                       WellTypeCode == "WD",
                       Longitude < -80)

# change final_wells to different formats
final_wells_wide = final_wells %>%
                    filter(CountyName == 'Kern') %>%
                    dplyr::select(APINumber,
                                  InjectionDate, 
                                  WaterInjected,
                                  Longitude,
                                  Latitude) %>%
                    distinct(APINumber, InjectionDate, .keep_all = T) %>%
                    group_by(APINumber) %>%
                    spread(key = InjectionDate,
                           value = WaterInjected) %>%
                    data.frame()

row.names(final_wells_wide) = 1:nrow(final_wells_wide)
final_wells_wide_sp = SpatialPointsDataFrame(coords = final_wells_wide %>% dplyr::select(Longitude, Latitude),
                                        data = final_wells %>% dplyr::select(-Longitude, -Latitude, -APINumber),
                                        proj4string = CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84" ))

# spatial polygons
# read kern county as polygon
kern = map_data("county") %>% filter(subregion == 'kern')
kern_poly = Polygon(kern[ , c("long", "lat")])
kern_polys = Polygons(list(kern_poly), 1)
kern_spoly = SpatialPolygons(list(kern_polys))
proj4string(kern_spoly) = CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84" )
kern_spoly_df = fortify(kern_spoly)

# look at final_wells_wide in kern county
kern_wells = over()

# look at time series of some wells
x = final_wells_wide[23, ] %>% dplyr::select(-APINumber, -Longitude, -Latitude) %>% as.numeric()
x = na.omit(x)
plot(1:length(x), y = x, type = 'b')

# time series of wells in aggregate
all_inj = colSums(final_wells_wide %>% dplyr::select(-APINumber, -Longitude, -Latitude), na.rm = T)
length(all_inj)
plot(all_inj, type = 'b')

# convert final_wells into spatial point dataframe
final_wells_sp = SpatialPointsDataFrame(coords = final_wells %>% dplyr::select(Longitude, Latitude),
                                        data = final_wells %>% dplyr::select(-Longitude, -Latitude),
                                        proj4string = CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84" ))



# convert ca_dat_eq into spatial point dataframe
ca_eq = ca_dat
ca_eq_sp = SpatialPointsDataFrame(coords = ca_eq %>% dplyr::select(longitude, latitude),
                                        data = ca_eq %>% dplyr::select(-longitude, -latitude),
                                        proj4string = CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84" ))
                

# make a grid

grid = makegrid(california_points, cellsize = 0.2)
grid = SpatialPoints(grid, proj4string = california_points@proj4string)
grid = points2grid(grid)
grid_sp = SpatialGrid(grid, proj4string = california_points@proj4string)
grid_sp_df = SpatialGridDataFrame(grid, data.frame(value = 1:length(grid_sp)),
                                  proj4string = california_points@proj4string)

grid_df = data.frame(grid_sp_df)
ggplot() +
  geom_raster(data=grid_df, aes(x=x1, y=x2, fill = value))

# mapping spatial points to grid
wells_xy = data.frame(x = final_wells$Longitude,
                y = final_wells$Latitude,
                id = "A",
                stringsAsFactors = F)
coordinates(wells_xy) = ~ x + y 
proj4string(wells_xy) = proj4string(california_points)
cellIDs = over(wells_xy, grid_sp_df)
colnames(cellIDs) = "Grid"
final_wells = bind_cols(final_wells, cellIDs)

# plot

ggplot() + 
  geom_polygon(data=california_spoly_df, aes(x=long,y=lat,group=group)) +
  geom_point(data=cal_eq_points_df, aes(x=longitude,y=latitude, color = time)) + 
  geom_point(data=final_wells, aes(x=Longitude,y=Latitude), color = 'red')
  # geom_polygon(data=grid, aes(x=x1, y=x2))
