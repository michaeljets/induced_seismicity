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
california_points2 = SpatialPointsDataFrame(coords = california %>% dplyr::select(long, lat),
                                            data = data.frame(ID = 1:nrow(california)),
                                            proj4string = CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84" ))
california_poly = Polygon(california[ , c("long", "lat")])
california_polys = Polygons(list(california_poly), 1)
california_spoly = SpatialPolygons(list(california_polys))
proj4string(california_spoly) = CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84" )
california_spoly_df = fortify(california_spoly)

# wells data
ca_wells = readOGR(dsn = 'rawdata/wells/california/shapefiles/AllWells',
                   layer = "AllWells_20170316")
ca_wells_points = spTransform(ca_wells, CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84" ))
ca_wells_df = data.frame(ca_wells_points)

ca_wells_df2 = ca_wells_df %>%
                filter(WellStatus == 'A',
                       Latitude != 0 | Longitude != 0)

# link this wells data with the injection data

inj_wells = read.csv("rawdata/wells/california/raw_injection_wells.csv",
                     stringsAsFactors = F)
inj_wells_store = inj_wells
inj_wells = inj_wells %>%
              dplyr::select(APINumber, CountyName, DaysInjecting,
                     DistrictNumber, InjectionDate, InjectionStatus,
                     MissingDataCode, PoolCode, PoolName,
                     PoolWellTypeStatus, Steam.WaterInjected.BBL.,
                     SurfaceInjPressure, SystemEntryDate, WellNumber,
                     WellStatus, WellTypeCode, Year)
ca_wells_df = ca_wells_df %>%
                mutate(APINumber = as.numeric(APINumber)) %>%
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
                filter(Year >= 1980,
                       WellTypeCode == "WD",
                       Longitude < -80)

# change final_wells to different formats
final_wells_wide = final_wells %>%
                    # filter(CountyName == 'Kern') %>%
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

# row.names(final_wells_wide) = 1:nrow(final_wells_wide)
final_wells_wide_sp = SpatialPointsDataFrame(coords = final_wells_wide %>% dplyr::select(Longitude, Latitude),
                                        data = final_wells_wide %>% dplyr::select(-Longitude, -Latitude, -APINumber),
                                        proj4string = CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84" ))

# spatial polygons
# read kern county as polygon
kern = map_data("county") %>% filter(subregion == 'kern')
kern_poly = Polygon(kern[ , c("long", "lat")])
kern_polys = Polygons(list(kern_poly), 1)
kern_spoly = SpatialPolygons(list(kern_polys))
proj4string(kern_spoly) = CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84" )
kern_spoly_df = SpatialPolygonsDataFrame(kern_spoly, data = data.frame(x = "filler"))

# look at final_wells_wide in kern county
wells_wide_xy = data.frame(x = final_wells_wide$Longitude,
                      y = final_wells_wide$Latitude,
                      id = "A",
                      stringsAsFactors = F)
coordinates(wells_wide_xy) = ~ x + y 
proj4string(wells_wide_xy) = proj4string(california_points)
which_wells = over(wells_wide_xy, kern_spoly)
final_wells_wide = final_wells_wide %>% mutate(Kern = which_wells)
kern_wells_wide = final_wells_wide %>% filter(Kern == 1)

kern_inj = colSums(kern_wells_wide %>% dplyr::select(-APINumber, -Longitude, -Latitude, -Kern), na.rm = T)
length(kern_inj)
plot(kern_inj, type = 'b')

# look at time series of some wells
x = final_wells_wide[23, ] %>% dplyr::select(-APINumber, -Longitude, -Latitude) %>% as.numeric()
x = na.omit(x)
plot(1:length(x), y = x, type = 'b')

# time series of wells in aggregate
all_inj = colSums(final_wells_wide %>% dplyr::select(-APINumber, -Longitude, -Latitude), na.rm = T)
length(all_inj)
months = substr(names(all_inj), 2, 8)
plot(all_inj, type = 'b', xaxt = 'n')
axis(side=1, at=1:length(all_inj), labels=months)

# convert final_wells into spatial point dataframe
final_wells_sp = SpatialPointsDataFrame(coords = final_wells %>% dplyr::select(Longitude, Latitude),
                                        data = final_wells %>% dplyr::select(-Longitude, -Latitude),
                                        proj4string = CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84" ))



# convert ca_dat_eq into spatial point dataframe
ca_eq = ca_dat %>%
          filter(time_year >= 1980)
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
# ggplot() +
#   geom_raster(data=grid_df, aes(x=x1, y=x2, fill = value))

# mapping wells spatial points to grid
wells_xy = data.frame(x = final_wells$Longitude,
                y = final_wells$Latitude,
                id = "A",
                stringsAsFactors = F)
coordinates(wells_xy) = ~ x + y 
proj4string(wells_xy) = proj4string(california_points)
cellIDs = over(wells_xy, grid_sp_df)
colnames(cellIDs) = "Grid"
final_wells = bind_cols(final_wells, cellIDs)

# mapping wide wells spatial points to grid
wells_wide_xy = data.frame(x = final_wells_wide$Longitude,
                      y = final_wells_wide$Latitude,
                      id = "A",
                      stringsAsFactors = F)
coordinates(wells_wide_xy) = ~ x + y 
proj4string(wells_wide_xy) = proj4string(california_points)
cellIDs = over(wells_wide_xy, grid_sp_df)
colnames(cellIDs) = "Grid"
final_wells_wide = bind_cols(final_wells_wide, cellIDs)

# aggregating water injections by month in each cellID
custom_sum = function(x) {
  return(sum(x, na.rm = T))
}
wells_grouped = final_wells_wide %>%
                  group_by(Grid) %>%
                  dplyr::select(-APINumber, -Longitude, -Latitude) %>%
                  summarise_all(funs(custom_sum)) %>%
                  data.frame()
most_water = wells_grouped %>% dplyr::select(-Grid) %>% rowSums()
wells_grouped$TotalWater = most_water
wells_grouped = wells_grouped[order(wells_grouped$TotalWater, decreasing = T), ]
# wells_grouped_drop = wells_grouped %>% dplyr::select(-Grid, -TotalWater)
which_blocks = head(wells_grouped$Grid, 6)

# mapping earthquakes spatial points to grid
eq_xy = data.frame(x = ca_eq$longitude,
                      y = ca_eq$latitude,
                      id = "A",
                      stringsAsFactors = F)
coordinates(eq_xy) = ~ x + y 
proj4string(eq_xy) = proj4string(california_points)
cellIDs = over(eq_xy, grid_sp_df)
colnames(cellIDs) = "Grid"
ca_eq = bind_cols(ca_eq, cellIDs)

# examining earthquakes in the most seismic blocks
# let's look at time series from 1980 of earthquakes in 4 most seismic blocks
which_blocks = names(head(sort(table(ca_eq$Grid), decreasing = T), 6))
which_blocks2 = c('1758', '1710', '1659', '1759', '1706', '1810')
which_blocks3 = c('572', '982', '983', '778', '727', '985')

par(mfrow = c(3,2))
for (i in 1:length(which_blocks)){
  to_examine = ca_eq %>%
    filter(Grid %in% which_blocks[i]) %>%
    mutate(time = substr(time, 1, 7))
  f_levels = c()
  years = 1980:2017
  for (t in 1:length(years)){
    f_levels = c(f_levels, paste0(years[t], '-', c('01', '02', '03', '04', '05', '06',
                                                       '07', '08', '09', '10', '11', '12')))
  }
  to_examine$time = factor(to_examine$time, levels = f_levels)
  plot(table(to_examine$time), type = 'l')
}

# plot time series water injections + earthquakes

# which_blocks = as.numeric(names(head(sort(table(ca_eq$Grid), decreasing = T), 12)))
which_blocks = wells_grouped$Grid[1:6]

par(mfrow = c(3,2))
for (i in 1:length(which_blocks)){
  # if ((which_blocks[i] %in% wells_grouped$Grid) == F){
  #   print('hello')
  #   next
  # }
  

  f_levels = c()
  years = 1980:2017
  for (t in 1:length(years)){
    f_levels = c(f_levels, paste0(years[t], '-', c('01', '02', '03', '04', '05', '06',
                                                   '07', '08', '09', '10', '11', '12')))
  }
  
  eqs = ca_eq %>%
    filter(Grid == which_blocks[i]) %>%
    mutate(time = factor(substr(time, 1, 7), levels = f_levels)) %>%
    dplyr::select(time) %>%
    table()
  # print(eqs)
  
  water_inj = wells_grouped %>%
    filter(Grid == which_blocks[i]) %>%
    dplyr::select(-Grid, -TotalWater) %>%
    as.numeric()
  # water_inj = as.numeric(wells_grouped_drop[which_blocks[i], ])
  plot(water_inj, type = 'l', xaxt = 'n', col = 'blue')
  axis(side=1, at=1:length(water_inj), labels=months)
  
  plot(eqs, col = 'red', xaxt = 'n', type = 'l')
  axis(side=1, at=1:length(eqs), labels=months)
}

# plot

ggplot() + 
  geom_polygon(data=california_spoly_df, aes(x=long,y=lat,group=group)) +
  geom_point(data=cal_eq_points_df, aes(x=longitude,y=latitude, color = time)) +
  geom_point(data=final_wells_wide, aes(x=Longitude,y=Latitude), color = 'red')
  # geom_polygon(data=grid, aes(x=x1, y=x2))

# spplot(grid_sp_df, "id",
#        panel = function(...) {
#          panel.gridplot(..., border="black")
#          sp.polygons(california_spoly)
#          sp.points(wells_xy, cex=1.5)
#          panel.text(...)
#        })


# how many earthquakes in water injection regions
unique_grid = unique(wells_grouped$Grid)

how_many = c()
for (i in 1:length(unique_grid)){
  how_many[i] = sum(ca_eq$Grid %in% unique_grid[i])
}
x=data.frame(Grid = unique_grid, earthquakes = how_many)
x = x[order(x$earthquakes, decreasing = T), ]
which_blocks = head(x$Grid, 18)
