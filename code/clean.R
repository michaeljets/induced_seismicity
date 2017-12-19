# =================================================================================================
# CLEAN DATA / SET UP WORKING ENVIRONMENT

# Author: Michael Jetsupphasuk
# Last Updated: 07 December, 2017

# This file creates the relevant data frames and Spatial objects for further analysis. 

# California Boundaries:
#     - Loads boundaries from map_data from `ggplot2`, `maps` packages
#     - Creates data in following formats:
#         + data frame
#         + spatial points data frame (`sp`)
#         + spatial polygon (`sp`)

# Grid:
#     - Creates a 0.2 x 0.2 degree long/lat grid
#     - Creates a spatial grid (`sp`)
#     - Optional parameter `grid_shift` to shift the grid; default is 0

# Earthquakes:
#     - See scrape/get_eq.R for query information
#     - Filter earthquakes for only ones in california (exclude offshore)
#     - Earthquakes mapped to grid (from above)
#     - Creates a data frame

# Wells:
#     - See scrape/get_inj.R for injection information
#     - Links various data frames together to gather all relevant information
#     - Wells mapped to grid (from above)
#     - Handles missing data
#         + appends 0 injection for dates before first / after last
#         + missing dates in between the first and last date have injection equal
#           to the average of the nearest injections before/after the missing
#     - Creates data in following formats:
#         + long data frame (each row a combination of well, injection date)
#         + wide data frame (each row is a unique well)
#         + spatial points data frame (wide data)

# =================================================================================================

# LOAD LIBRARIES ----------------------------------------------------------

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


# GLOBAL PARAMETERS -------------------------------------------------------

my_crs = CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84" )

fill_dates <- function(df, alldates,
                       mindate="1980-01-01 00:00:00", 
                       maxdate="2017-06-01 00:00:00"){
  
  ## Inserts the missing dates before/after the min/max date in `df`. If there are 
  ## missing dates in between the min/max date in `df`, they are interpolated by 
  ## taking the average of the nearest injections before and after. 
  
  
  before_df = data.frame()
  after_df = data.frame()
  between_df = data.frame()
  
  before_dates = alldates[alldates < min(df$InjectionDate)]
  after_dates = alldates[alldates > max(df$InjectionDate)]
  
  # fixing the "before" dates
  if (min(df$InjectionDate) != mindate){
    before_df = data.frame(APINumber = df$APINumber[1],
                           InjectionDate = before_dates,
                           WaterInjected = 0,
                           Grid = df$Grid[1],
                           Latitude = df$Latitude[1],
                           Longitude = df$Longitude[1])
  }
  
  # fixing the "after" dates
  if (max(df$InjectionDate) != maxdate){
    after_df = data.frame(APINumber = df$APINumber[1],
                          InjectionDate = after_dates,
                          WaterInjected = 0,
                          Grid = df$Grid[1],
                          Latitude = df$Latitude[1],
                          Longitude = df$Longitude[1])
  }
  
  # fixing the "between" dates
  if (nrow(before_df)+nrow(after_df)+length(unique(df$InjectionDate)) < length(alldates)){
    ind = which((alldates %in% c(df$InjectionDate, before_dates, after_dates)) == F)
    between_dates = alldates[ind]
    between_water = rep(NA, length(between_dates))
    
    # take the midpoint of nearest data for in-between missing data
    for (i in 1:length(ind)){
      
      # find nearest water injection before
      ind_before = ind[i]-1
      while (alldates[ind_before] %in% between_dates){
        ind_before = ind_before-1
      }
      water_before = df$WaterInjected[df$InjectionDate == alldates[ind_before]]
      if (length(water_before) > 1){
        water_before = ifelse(any(water_before == 0), max(water_before, na.rm=T), sum(water_before, na.rm=T))
      }
      
      # find nearest water injection after
      ind_after = ind[i]+1
      while (alldates[ind_after] %in% between_dates){
        ind_after = ind_after+1
      }
      water_after = df$WaterInjected[df$InjectionDate == alldates[ind_after]]
      if (length(water_after) > 1){
        water_after = ifelse(any(water_after == 0), max(water_after, na.rm=T), sum(water_after, na.rm=T))
      }
      
      # average before/after water injections
      water_inter = (water_before + water_after)/2
      between_water[i] = water_inter
      
      # temp_df = df %>%
      #             filter(InjectionDate == check_date)
      # 
      # if (is.na(temp_df$InjectionStatus)){
      #   warning("Problem with injection dates")
      # }
      # 
      # if (temp_df$InjectionStatus != "0"){
      #   between_water[i] = 0
      # }
    }
    
    between_df = data.frame(APINumber = df$APINumber[1],
                            InjectionDate = between_dates,
                            WaterInjected = between_water,
                            Grid = df$Grid[1],
                            Latitude = df$Latitude[1],
                            Longitude = df$Longitude[1])
  }
  
  all_df = suppressWarnings(bind_rows(before_df, between_df, after_df, df))
  
  return(all_df %>% arrange(InjectionDate))
}


# CALIFORNIA BOUNDARIES ---------------------------------------------------

# get california boundaries
cal = map_data("state") %>% filter(region == 'california')

# convert boundaries to a SpatialPointsDataFrame
cal_points = SpatialPointsDataFrame(coords = cal %>% dplyr::select(long, lat),
                                    data = data.frame(ID = 1:nrow(cal)),
                                    proj4string = CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84" ))

# convert boundaries to a SpatialPolygon
cal_poly = Polygon(cal[ , c("long", "lat")])
cal_polys = Polygons(list(cal_poly), 1)
cal_spolys = SpatialPolygons(list(cal_polys))
proj4string(cal_spolys) = CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84" )


# GRID --------------------------------------------------------------------

# create grid from california points data
grid = makegrid(cal_points, cellsize = 0.2)
grid = SpatialPoints(grid, proj4string = my_crs)
grid = points2grid(grid)

# option to shift the grid
grid_shift = .1
grid_name = '_ne1' # to be appended to data file names
grid@cellcentre.offset = grid@cellcentre.offset + grid_shift

# convert to spatial grid
grid_sp = SpatialGrid(grid, proj4string = my_crs)


# EARTHQUAKES -------------------------------------------------------------

# read in data generated by scrape/get_eq.R
ca_eq = read.csv("rawdata/earthquakes/california/ca_eq_raw25.csv")

# convert longitude/latitude to numeric; add time columns
ca_eq = ca_eq %>%
  mutate(longitude = as.numeric(as.character(longitude)),
         latitude = as.numeric(as.character(latitude)),
         time_year = as.numeric(substr(time, 1, 4)),
         time_dec = floor(time_year/10)*10)

# convert earthquakes data frame to SpatialPoints object
ca_eq_sp = SpatialPoints(ca_eq %>% dplyr::select(longitude, latitude),
                         proj4string = my_crs)

# map earthquakes to california polygon and filter for only in state
which_ca = over(ca_eq_sp, cal_spolys)
which_ca[which_ca == 1] = "california"
ca_eq = ca_eq %>%
          mutate(state = which_ca) %>%
          filter(state == "california")

# map earthquakes spatial points to grid
eq_xy = data.frame(x = ca_eq$longitude,
                   y = ca_eq$latitude,
                   id = "A",
                   stringsAsFactors = F)
coordinates(eq_xy) = ~ x + y 
proj4string(eq_xy) = my_crs
cellIDs = over(eq_xy, grid_sp)
ca_eq$Grid = cellIDs


# WELLS -------------------------------------------------------------------

## Read in well locations

# shapefile from ftp://ftp.consrv.ca.gov/pub/oil/GIS/Shapefiles/
well_locs = readOGR(dsn = 'rawdata/wells/california/shapefiles/AllWells',
                   layer = "AllWells_20170316")
            
well_locs = spTransform(well_locs, CRSobj = my_crs)

# we will ignore wells with obviously inaccurate latitude/longitude
# only include columns that might be relevant
well_locs_df = data.frame(well_locs) %>%
                filter(Latitude != 0,
                       Longitude != 0) %>%
                mutate(APINumber = as.numeric(as.character(APINumber))) %>%
                dplyr::select(APINumber, BLMWell, CompDate,
                              ConfWell, coords.x1, coords.x2,
                              County, District, DryHole, Elevation,
                              EPAWell, HydFrac, RedCanFlag,
                              RedrillFt, SpudDate, TotalDepth,
                              WellNumber, WellStatus)

## Read in injection data

# read in data generated by scrape/get_inj.R
inj_wells = readRDS("rawdata/wells/california/raw_injection_wells.rds")

# select only columns that might be relevant
inj_wells = inj_wells %>%
              mutate(APINumber = as.numeric(APINumber)) %>%
              dplyr::select(APINumber, CountyName, DaysInjecting,
                            DistrictNumber, InjectionDate, InjectionStatus,
                            MissingDataCode, Steam.WaterInjected.BBL.,
                            SurfaceInjPressure, SystemEntryDate, WellNumber,
                            WellStatus, WellTypeCode, Year)

## Join location and injection data

# data in long format (i.e. each row is a combination of well, injection date)
wells_long = inner_join(inj_wells, well_locs_df, by = c("APINumber")) %>%
                mutate(Year = as.numeric(Year),
                       Steam.WaterInjected.BBL. = as.numeric(Steam.WaterInjected.BBL.),
                       coords.x1 = as.numeric(coords.x1),
                       coords.x2 = as.numeric(coords.x2)) %>%
                rename(WaterInjected = Steam.WaterInjected.BBL.,
                       Longitude = coords.x1,
                       Latitude = coords.x2) %>%
                filter(Year >= 1980,
                       WellTypeCode == "WD",
                       Longitude < -80,
                       InjectionDate != "2017-07-01 00:00:00",
                       InjectionDate != "2017-08-01 00:00:00",
                       InjectionDate != "2017-09-01 00:00:00",
                       InjectionDate != "2017-10-01 00:00:00",
                       InjectionDate != "2017-11-01 00:00:00",
                       InjectionDate != "2017-12-01 00:00:00") %>%
                distinct()

# mapping long wells to grid
wells_long_xy = data.frame(x = wells_long$Longitude,
                           y = wells_long$Latitude,
                           id = "A",
                           stringsAsFactors = F)
coordinates(wells_long_xy) = ~ x + y 
proj4string(wells_long_xy) = my_crs
cellIDs = over(wells_long_xy, grid_sp)
wells_long$Grid = cellIDs

# handle missing injection dates

all_dates = sort(unique(wells_long$InjectionDate))

wells_long = wells_long %>%
                group_by(APINumber) %>%
                do(fill_dates(., all_dates))


# also create a wide data frame (i.e. each row is a unique well)
wells_wide = wells_long %>%
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

# mapping wide wells spatial points to grid
wells_wide_xy = data.frame(x = wells_wide$Longitude,
                           y = wells_wide$Latitude,
                           id = "A",
                           stringsAsFactors = F)
coordinates(wells_wide_xy) = ~ x + y 
proj4string(wells_wide_xy) = my_crs
cellIDs = over(wells_wide_xy, grid_sp)
wells_wide$Grid = cellIDs

# convert wide wells to a SpatialPointsDataFrame
wells_wide_sp = SpatialPointsDataFrame(coords = wells_wide %>% 
                                                  dplyr::select(Longitude, Latitude),
                                       data = wells_wide %>% 
                                                dplyr::select(-Longitude, -Latitude),
                                       proj4string = my_crs)



# WATER AND EARTHQUAKE BY GRID --------------------------------------------

# flatten water across blocks
grid_inj = wells_wide %>%
  dplyr::select(-APINumber, -Longitude, -Latitude) %>%
  group_by(Grid) %>%
  summarise_all(sum, na.rm = T)

# all grid blocks with non-zero water injection
which_no_water = (rowSums(grid_inj %>% dplyr::select(-Grid)) == 0)
grid_inj = grid_inj %>% filter(which_no_water == F)

# all grid blocks with non-zero water injection and non-zero earthquakes
unique_grid = unique(grid_inj$Grid)
how_many = c()
for (i in 1:length(unique_grid)){
  how_many[i] = sum(ca_eq$Grid %in% unique_grid[i])
}

eq_block = data.frame(Grid = unique_grid, earthquakes = how_many)
final_blocks = sort(eq_block$Grid[eq_block$earthquakes > 0])

# final data frames

# convert time in ca_eq to factor with correct levels
months = substr(colnames(wells_wide %>% 
                           dplyr::select(-APINumber, -Longitude, -Latitude, -Grid)), 
                start = 2, stop = 8)

ca_eq = ca_eq %>%
  mutate(time = as.character(time),
         time = substr(time, 1, 7),
         time = gsub('-', '.', time),
         time = factor(time, levels = months))

# generate earthquake time series
final_eqs = ca_eq %>%
  filter(Grid %in% final_blocks) %>%
  arrange(Grid) %>%
  group_by(Grid) %>%
  dplyr::select(time) %>%
  table() %>%
  matrix(length(final_blocks), 450) %>%
  data.frame()

# final water time series
final_water = grid_inj %>%
  filter(Grid %in% final_blocks) %>%
  arrange(Grid) %>%
  dplyr::select(-Grid)


# SAVING ------------------------------------------------------------------

rm(inj_wells)
rm(which_ca)
rm(well_locs)
rm(well_locs_df)
# save.image(file = "clean_data.RData")
# save.image()


# Writing -----------------------------------------------------------------

# write files
write.csv(final_water, file = paste0("data/final_water", grid_name, ".csv"), row.names = F)
write.csv(final_eqs, file = paste0("data/final_eqs", grid_name, ".csv"), row.names = F)
write.table(final_blocks, file = paste0("data/final_blocks", grid_name, ".txt"), row.names = F, col.names = F)
write.table(months, file = "data/months.txt", row.names = F, col.names = F)


# ARCHIVED CODE -----------------------------------------------------------

# wells_long2 = wells_long %>%
#   group_by(APINumber) %>%
#   mutate(Latitude = Latitude[is.na(Latitude)==F][1],
#          Longitude = Longitude[is.na(Longitude)==F][1])


# compute_d <- function(x,y){
#   dot = sum(x*y)
#   n = length(x)
#   result = n*(n+1)*(2*n+1)/3 - 2*dot
#   return(result)
# }
# 
# # Do not "re-rank" at each lag; i.e. do not throw away information
# # Which lag tends to have the highest rank correlation?
# set.seed(12879273)
# B = 100000
# max_lag = c()
# a = 1:450
# for (i in 1:B){
#   b = sample(1:450, 450, replace=F)
#   ccfs = ccf(a, b, plot = F, lag.max = 450)
#   cors = as.numeric(ccfs$acf)
#   lags = as.numeric(ccfs$lag)
#   which_maxs = which(cors == max(cors))
#   which_max = ifelse(length(which_maxs)==1, which_maxs, sample(which_maxs, 1))
#   max_lag[i] = lags[which_max]
# }
# max_lag2 = factor(max_lag, levels = -449:449)
# plot(table(max_lag2))
# 
# # "Re-rank" at each lag
# # Which lag tends to have the highest rank correlation?
# B = 1000
# max_lag2 = c()
# a = 1:200
# for (i in 1:B){
#   b = sample(1:200, 200, replace=F)
#   ccfs = c()
#   for (l in 0:198){
#     a2 = a[1:(200-l)]
#     b2 = b[(l+1):200]
#     # a2 = rank(a2) # re-rank
#     # b2 = rank(b2) # re-rank
#     ccfs[l+1] = cor(a2,b2)
#   }
#   max_lag2[i] = which(ccfs == max(ccfs)[1]) - 1
# }
# max_lag2 = factor(max_lag2, levels = 0:199)
# plot(table(max_lag2))
# 
#=================================================#
# # Don't use all data; use let's say 400
# B = 10000
# max_lag = c()
# a = 1:150
# b = 1:200
# for (i in 1:B){
#   ccfs = c()
#   b = sample(b, 200, replace=F)
#   for (l in 0:12){
#     b2 = b[(l+1):(150+l)]
#     # b2 = rank(b2) # re-rank
#     if (length(b2) != 150) (warning("what the fuck"))
#     ccfs[l+1] = cor(a,b2)
#   }
#   print(round(ccfs, 2))
#   argmax = which(ccfs == max(ccfs))
#   print(argmax - 1)
#   if (length(argmax) != 1) (warning("aight then"))
#   max_lag[i] = argmax[sample(1:length(argmax), 1)] - 1 # choose random if multiple maxima
# }
# max_lag2 = factor(max_lag, levels = 0:12)
# plot(table(max_lag2))

# diff_months <- function(end_date, start_date) {
#   ed <- as.POSIXlt(end_date)
#   sd <- as.POSIXlt(start_date)
#   return(12 * (ed$year - sd$year) + (ed$mon - sd$mon))
# }
# 
# 
# unique_id = paste0(wells_long$APINumber, '/', wells_long$InjectionDate)
# length(unique_id)
# length(unique(unique_id))
# which_wrong = names(table(unique_id)[table(unique_id) > 1])
# wells_long$unique_id = unique_id
# 
# View(wells_long %>% 
#        filter(unique_id %in% which_wrong) %>% 
#        arrange(APINumber, InjectionDate) %>% 
#        select(APINumber, InjectionDate, WaterInjected))
# 
# x=wells_long %>%
#   group_by(APINumber) %>%
#   summarize(length(InjectionDate) - diff_months(max(InjectionDate), min(InjectionDate)) - 1) %>%
#   as.data.frame()
# colnames(x) = c('APINumber', 'months')
# 
# x=wells_long %>%
#   group_by(APINumber) %>%
#   mutate(dup = ifelse(length(InjectionDate) == length(unique(InjectionDate)), 'duplicates', 'ok')) %>%
#   select(APINumber, dup) %>%
#   as.data.frame() %>%
#   distinct()
# table(x$dup)
