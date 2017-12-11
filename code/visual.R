# =================================================================================================
# CREATING VISUALIZATIONS

# Author: Michael Jetsupphasuk
# Last Updated: 07 December, 2017

# This file creates plots for data exploration. Load .RData created by clean.R


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

months = substr(colnames(wells_wide %>% 
                           dplyr::select(-APINumber, -Longitude, -Latitude, -Grid)), 
                start = 2, stop = 8)

# sum while ignoring NA
custom_sum <- function(x) {
  return(sum(x, na.rm = T))
}

# convert a time series (numeric vector) to cummulative ts (not on 0-1 scale)
convert_cumm <- function(vec){
  first_vec = vec[1]
  if (length(vec) == 1){
    return(first_vec)
  }
  vec[2] = vec[1] + vec[2]
  return(c(first_vec, convert_cumm(vec[2:length(vec)])))
}

# SIMPLE EXPLORATORY -------------------------------------------------------------

# time series of wells in aggregate

all_inj = colSums(wells_wide %>% dplyr::select(-APINumber, -Longitude, -Latitude, -Grid), na.rm = T)
plot(all_inj, type = 'b', xaxt = 'n')
axis(side=1, at=1:length(all_inj), labels=months)


# wells, earthquakes, grid on california map

cal_spolys_df = fortify(cal_spolys)
grid_df = data.frame(SpatialPoints(grid, my_crs)@coords)
ggplot() +
  geom_polygon(data = cal_spolys_df, aes(x = long, y = lat, group = group)) +
  geom_tile(data = grid_df, aes(x = x1, y = x2), color = 'yellow', alpha = 0) +
  geom_point(data = ca_eq, aes(x = longitude, y = latitude, color = time_year), show.legend = F, size = .75) +
  geom_point(data = wells_wide, aes(x = Longitude, y = Latitude), show.legend = F, size = .75, color = 'red')
  


# INJECTIONS + EARTHQUAKES EACH BLOCK -------------------------------------

# for each grid block, getting total water injected per month 
wells_grouped = wells_wide %>%
  group_by(Grid) %>%
  dplyr::select(-APINumber, -Longitude, -Latitude) %>%
  summarise_all(funs(custom_sum)) %>%
  data.frame()

# get cummulative water injected per block
cumm_water = wells_grouped %>% 
              dplyr::select(-Grid) %>% 
              rowSums()
wells_grouped$CummWater = cumm_water
wells_grouped = wells_grouped %>% 
                  arrange(desc(CummWater)) %>%
                  filter(CummWater != 0) # remove blocks with no injection

# how many earthquakes in blocks with non-zero water injection
unique_grid = unique(wells_grouped$Grid)

how_many = c()
for (i in 1:length(unique_grid)){
  how_many[i] = sum(ca_eq$Grid %in% unique_grid[i])
}

eq_block = data.frame(Grid = unique_grid, earthquakes = how_many)
eq_block = eq_block %>% arrange(desc(earthquakes))

sum(eq_block$earthquakes >= 10)

# plot map of california, grid, and earthquake locations
# where are the blocks with non-zero water injection and eq >= 10? 
ca_eq_temp = ca_eq %>% 
              filter(Grid %in% unique(eq_block$Grid[eq_block$earthquakes >= 10])) %>% 
              dplyr::select(longitude, latitude)
ggplot() +
  geom_polygon(data = cal_spolys_df, aes(x = long, y = lat, group = group)) +
  geom_tile(data = grid_df, aes(x = x1, y = x2), color = 'yellow', alpha = 0) +
  geom_point(data = ca_eq_temp, aes(x = longitude, y = latitude, color = 'red'))

# convert time in ca_eq to factor with correct levels
ca_eq = ca_eq %>%
          mutate(time = as.character(time),
                 time = substr(time, 1, 7),
                 time = gsub('-', '.', time),
                 time = factor(time, levels = months))

# plot earthquakes, water injection time series for each relevant block
for (i in 1:length(unique_grid)){
  eqs = ca_eq %>%
    filter(Grid == unique_grid[i]) %>%
    dplyr::select(time) %>%
    table() %>% 
    as.numeric()
  # eqs_cumm = convert_cumm(eqs) / sum(eqs)

  water_inj = wells_grouped %>%
    filter(Grid == unique_grid[i]) %>%
    dplyr::select(-Grid, -CummWater) %>%
    as.numeric()
  water_inj_cumm = convert_cumm(water_inj) / sum(water_inj)
  
  # non cummulative plots
  path_save = paste0("pictures/eq_inj25/not_cumm/", "grid", unique_grid[i], ".jpeg")
  jpeg(filename = path_save,
       width = 1500, height = 600)
  
  par(mfrow = c(2,1))
  
  plot(water_inj, type = 'l', xaxt = 'n', col = 'blue',
       main = "Wastewater Injection Time Series",
       xlab = "Time (months)",
       ylab = "Injections (bbl)")
  axis(side=1, at=1:length(water_inj), labels=months)
  
  plot(eqs, col = 'red', xaxt = 'n', type = 'l',
       main = "Earthquake Time Series",
       xlab = "Time (months)",
       ylab = "Number of Earthquakes (>2.5)")
  axis(side=1, at=1:length(water_inj), labels=months)
  
  dev.off()
  
###############################################################################  
  
  # cummulative plots
  path_save = paste0("pictures/eq_inj25/cumm/", "grid", unique_grid[i], ".jpeg")
  jpeg(filename = path_save,
       width = 1500, height = 600)
  
  par(mfrow = c(2,1))
  
  plot(water_inj_cumm, type = 'l', xaxt = 'n', col = 'blue',
       main = "Cumm. Wastewater Injection Time Series",
       xlab = "Time (months)",
       ylab = "Cumm. Injections (bbl)")
  axis(side=1, at=1:length(water_inj_cumm), labels=months)
  
  plot(eqs, col = 'red', xaxt = 'n', type = 'l',
       main = "Earthquake Time Series",
       xlab = "Time (months)",
       ylab = "Number of Earthquakes (>2.5)")
  axis(side=1, at=1:length(water_inj_cumm), labels=months)
  
  dev.off()
}


