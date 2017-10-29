dir = "induced_seismicity/rawdata/wells/california"
setwd(dir)

library(data.table)
library(rgdal)
library(sp)
library(raster)
library(ggplot2)
library(rgeos)
library("lattice")
wells = readRDS("final_wells.rds")
wells = data.table(wells)
wells = wells[Year >=2010,]

###grid layout. 

shape = readOGR(dsn = "state_shapefiles/", "CA_State_TIGER2016")

# ca.utm = spTransform(shape, CRSobj = "+proj=utm +zone=11 ellps=WGS84")
# 
# x.cells = ceiling((ca.utm@bbox[1,2] - ca.utm@bbox[1,1])/6000)
# y.cells = ceiling((ca.utm@bbox[2,2] - ca.utm@bbox[2,1])/10000)
# ca.grid = raster(extent(ca.utm), nrow=y.cells, 
#                   ncol=x.cells, crs=ca.utm@proj4string)
# ca.grid.poly = as(as(ca.grid, "SpatialPixels"),"SpatialPolygons")
# ca.grid.utm = spTransform(ca.grid.poly, CRSobj = "+proj=utm +zone=11 ellps=WGS84")
# ca.grid.inter = gIntersection(ca.grid.utm, ca.utm, byid = TRUE)
#


LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}

LongLatToUTM(x,y,15) # need to figure california zone 

xy = LongLatToUTM(wells$Longitude,wells$Latitude)


poi = data.frame(x=xy$X,
                  y=xy$Y,
                  id="A", stringsAsFactors=F)
coordinates(poi) = ~ x + y
proj4string(poi) = proj4string(shape)

# Compute grid dimensions
cellSize = 60000
bb = bbox(shape)
cs = c(3.2808, 3.2808)*cellSize  # cell size 6km x 6km (for illustration)
# 1 ft = 3.28084 m
cc = bb[, 1] + (cs/2)  # cell offset
cd = ceiling(diff(t(bb))/cs)  # number of cells per direction
grd = GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)

sp_grd = SpatialGridDataFrame(grd,
                               data=data.frame(id=1:prod(cd)),
                               proj4string=CRS(proj4string(shape)))

# Cell data
numCells = prod(cd)

nunique = length(unique(cellIDs$id))


spplot(sp_grd, "id",
       panel = function(...) {
         panel.gridplot(..., border="black")
         sp.polygons(shape)
         sp.points(poi, cex=1.5)
         panel.text(...)
       })


################################################################
library(ggmap)
map = get_map(location = "california", zoom =6, source = "google", maptype = "roadmap")
contours = stat_density2d(aes(x= Longitude, y = Latitude,
                              fill = ..level.., alpha=..level..),
                          size = 0.1, data = wells, n = c(51,48),
                          geom = "polygon")


ggmap(map,extent = "device") +
  contours + 
  scale_alpha_continuous(range = c(0.2, 0.6), guide = 'none') + 
  scale_fill_gradient("Water_injected\nDensity") + 
  ggtitle("Water Injection in California")

ggmap(map, extent = "device") + 
  geom_point(data = wells,aes(x = Longitude, y = Latitude), color = "blue")


wells_freq = wells[,.(freq = .N), by = gridId]
wells_freq = wells_freq[order(-freq),]
## cell ID: 
# high frequency: 572, 982, 983 
# low frequency: 931, 727, 985




par(mfrow= c(3,2))

wells = data.table(wells)
col = c("InjectionDate", "Longitude", "Latitude", "Year", "WaterInjected", "gridId")
wells = wells[,..col]
wells$InjectionDate = as.POSIXct(wells$InjectionDate)
cell572 = ts(wells[gridId == 572,]$WaterInjected, start = c(2010, 1), end = c(2017,12), frequency = 12)
cell982 = ts(wells[gridId == 982,]$WaterInjected, start = c(2010, 1), end = c(2017,12), frequency = 12)
cell983 = ts(wells[gridId == 983,]$WaterInjected, start = c(2010, 1), end = c(2017,12), frequency = 12)
cell778 = ts(wells[gridId == 778,]$WaterInjected, start = c(2010, 1), end = c(2017,12), frequency = 12)
cell727 = ts(wells[gridId == 727,]$WaterInjected, start = c(2010, 1), end = c(2017,12), frequency = 12)
cell985 = ts(wells[gridId == 985,]$WaterInjected, start = c(2010, 1), end = c(2017,12), frequency = 12)



plot.ts(cell572)
plot.ts(cell982)
plot.ts(cell983)
plot.ts(cell778)
plot.ts(cell727)
plot.ts(cell985)


cell1758 = ts(wells[gridId == 1758,]$WaterInjected, start = c(2010, 1), end = c(2017,12), frequency = 12)
cell1710 = ts(wells[gridId == 1710,]$WaterInjected, start = c(2010, 1), end = c(2017,12), frequency = 12)
cell1659 = ts(wells[gridId == 1659,]$WaterInjected, start = c(2010, 1), end = c(2017,12), frequency = 12)
cell1759 = ts(wells[gridId == 1759,]$WaterInjected, start = c(2010, 1), end = c(2017,12), frequency = 12)
cell1706 = ts(wells[gridId == 1706,]$WaterInjected, start = c(2010, 1), end = c(2017,12), frequency = 12)
cell1810 = ts(wells[gridId == 1810,]$WaterInjected, start = c(2010, 1), end = c(2017,12), frequency = 12)



plot.ts(cell1758)
plot.ts(cell1710)
plot.ts(cell1659)
plot.ts(cell1759)
plot.ts(cell1706)
plot.ts(cell1810)

