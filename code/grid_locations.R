# =================================================================================================
# GET GRID LOCATIONS

# Author: Michael Jetsupphasuk
# Last Updated: 07 December, 2017

# This file reports the grid coordinates for the blocks with significant p-values.

# =================================================================================================

# LOAD LIBRARIES ----------------------------------------------------------

library(sp)
library(ggmap)
library(dplyr)
library(spdep)

# load("clean_data.RData")

# GET OKLAHOMA GRID -------------------------------------------------------

# get oklahoma boundaries
okl = map_data("state") %>% filter(region == 'oklahoma')

# convert boundaries to a SpatialPointsDataFrame
okl_points = SpatialPointsDataFrame(coords = okl %>% dplyr::select(long, lat),
                                    data = data.frame(ID = 1:nrow(okl)),
                                    proj4string = CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84" ))

# convert boundaries to a SpatialPolygon
okl_poly = Polygon(okl[ , c("long", "lat")])
okl_polys = Polygons(list(okl_poly), 1)
okl_spolys = SpatialPolygons(list(okl_polys))
proj4string(okl_spolys) = CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84" )

# create grid from oklahoma points data
grid_ok = makegrid(okl_points, cellsize = 0.2)
grid_ok = SpatialPoints(grid_ok, proj4string = my_crs)
grid_ok = points2grid(grid_ok)
grid_sp_ok = SpatialGrid(grid_ok, proj4string = my_crs)


# GET COORDINATES ---------------------------------------------------------

# read data
pvals_ca = read.csv("results/pval_blocks_ca.csv", stringsAsFactors = F)
pvals_ok = read.csv("results/pval_blocks_ok.csv", stringsAsFactors = F)

# get coordinates
grid_blocks_ca = pvals_ca$Grid[pvals_ca$P.value <= .05]
grid_blocks_ok = pvals_ok$Grid[pvals_ok$P.value <= .05]

coords_ca = coordinates(grid_sp)[grid_blocks_ca, ]
coords_ok = coordinates(grid_sp_ok)[grid_blocks_ok, ]

colnames(coords_ca) = c("Longitude", "Latitude")
colnames(coords_ok) = c("Longitude", "Latitude")

# write data
write.csv(data.frame(coords_ca), "results/coordinates_ca.csv", row.names = F)
write.csv(data.frame(coords_ok), "results/coordinates_ok.csv", row.names = F)


# VISUALIZATIONS ----------------------------------------------------------

# load data

water_ca = read.csv("data/final_water.csv")
eqs_ca = read.csv("data/final_eqs.csv")

water_ok = read.csv("data/final_water_ok.csv")
eqs_ok = read.csv("data/final_eqs_ok.csv")


# get kern county coordinates
kern = map_data("county") %>% filter(region == 'california', subregion = 'kern')

# significant blocks on california map
cal_spolys_df = fortify(cal_spolys)
grid_df_ca = data.frame(SpatialPoints(grid, my_crs)@coords)
ggplot() +
  geom_polygon(data = cal_spolys_df, aes(x = long, y = lat, group = group)) +
  geom_polygon(data = kern, aes(x = long, y = lat, group = group), alpha = 0.75, fill = 'blue') +
  geom_tile(data = grid_df_ca, aes(x = x1, y = x2), color = 'yellow', alpha = 0) +
  geom_point(data = data.frame(coords_ca), aes(x = Longitude, y = Latitude), size = 2, color = 'green') +
  geom_point(data = data.frame(long = -119, lat = 35), aes(x = long, y = lat), size = 2, color = 'red') +
  xlab("Longitude") +
  ylab("Latitude")


# significant on oklahoma map
okl_spolys_df = fortify(okl_spolys)
grid_df_ok = data.frame(SpatialPoints(grid_ok, my_crs)@coords)
ggplot() +
  geom_polygon(data = okl_spolys_df, aes(x = long, y = lat, group = group)) +
  geom_tile(data = grid_df_ok, aes(x = x1, y = x2), color = 'yellow', alpha = 0) +
  geom_point(data = data.frame(coords_ok), aes(x = Longitude, y = Latitude), size = 2, color = 'green') +
  xlab("Longitude") +
  ylab("Latitude")

