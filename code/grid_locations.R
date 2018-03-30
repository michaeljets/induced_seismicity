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

# combine pvals data with coordinate data
pvals_ca = pvals_ca %>%
            mutate(Longitude = coordinates(grid_sp)[pvals_ca$Grid, 1],
                   Latitude = coordinates(grid_sp)[pvals_ca$Grid, 2])
pvals_ok = pvals_ok %>%
            mutate(Longitude = coordinates(grid_sp_ok)[pvals_ok$Grid, 1],
                   Latitude = coordinates(grid_sp_ok)[pvals_ok$Grid, 2])

# # write data
# write.csv(pvals_ca, "results/final_ca.csv", row.names = F)
# write.csv(pvals_ok, "results/final_ok.csv", row.names = F)


# VISUALIZATIONS ----------------------------------------------------------

# load data

water_ca = read.csv("data/final_water.csv")
eqs_ca = read.csv("data/final_eqs.csv")

water_ok = read.csv("data/final_water_ok.csv")
eqs_ok = read.csv("data/final_eqs_ok.csv")


# get kern county coordinates
kern = map_data("county") %>% filter(region == 'california', subregion == 'kern')

# add mcclure findings
mcclure_ca = data.frame(Latitude = c(33.8, 36.2, 34.2),
                        Longitude = c(-118.4, -120.4, -118.4))
mcclure_ok = data.frame(Latitude = c(34.6, 35.6, 36.8, 36.2, 36.6, 36.8, 37, 
                                     35, 35.8, 36.6, 36.8, 36.4, 36.6, 36.8),
                        Longitude = c(-96.2, -97.2, -98.2, -96.8, -98, -98.6, -98, 
                                      -96, -97.4, -97.8, -98, -97, -97.4, -97.8))

# significant blocks on california map
cal_spolys_df = fortify(cal_spolys)
grid_df_ca = data.frame(SpatialPoints(grid, my_crs)@coords)
ggplot() +
  geom_polygon(data = cal_spolys_df, aes(x = long, y = lat, group = group), fill = 'white') +
  geom_polygon(data = kern, aes(x = long, y = lat, group = group, fill = 'Kern County'), alpha = 0.35) +
  geom_tile(data = grid_df_ca, aes(x = x1, y = x2), color = 'grey', alpha = 0) +
  geom_point(data = data.frame(pvals_ca %>% filter(P.value.lower.bound <= .05)), 
             aes(x = Longitude, y = Latitude, shape = 'Sig. Blocks'), size = 2, color = 'black') +
  geom_point(data = data.frame(long = -119, lat = 35), aes(x = long, y = lat, shape = 'Tejon Oil Field'), size = 2) +
  geom_point(data = mcclure_ca, aes(x = Longitude, y = Latitude, shape = '(McClure 2017)'), size = 2) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_fill_manual(name = "", values = 'grey') +
  scale_shape_manual(name = "", values = c(3, 1, 2),
                     breaks = c('Sig. Blocks', '(McClure 2017)', 'Tejon Oil Field')) +
  theme(legend.position = c(.8, .7),
        legend.title = element_blank())

# save plot
ggsave("results/cal_sig_map.png", scale = 2)


# significant on oklahoma map
okl_spolys_df = fortify(okl_spolys)
grid_df_ok = data.frame(SpatialPoints(grid_ok, my_crs)@coords)
ggplot() +
  geom_polygon(data = okl_spolys_df, aes(x = long, y = lat, group = group), fill = 'white') +
  geom_tile(data = grid_df_ok, aes(x = x1, y = x2), color = 'grey', alpha = 0) +
  geom_point(data = data.frame(pvals_ok %>% filter(P.value.lower.bound <= .05)), 
             aes(x = Longitude, y = Latitude, shape = 'Sig. Blocks'), size = 2, color = 'black') +
  geom_point(data = mcclure_ok, aes(x = Longitude, y = Latitude, shape = '(McClure 2017)'), size = 2) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_shape_manual(name = "", values = c(3, 1), breaks = c('Sig. Blocks', '(McClure 2017)')) +
  theme(legend.position = c(.2, .2),
        legend.title = element_blank())

# save plot
ggsave("results/okl_sig_map.png", width = 10, height = 6)

