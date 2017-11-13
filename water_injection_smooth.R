library(data.table)
library(forecast)


load("clean_data.RData")

wells_wide= data.table(wells_wide)
wells_wide[is.na(wells_wide)]  = 0

wells =  melt(wells_wide, id.vars =c("APINumber","Longitude","Latitude", 'Grid'), measure.vars = 4:459)
wells$Date = gsub(substr(wells$variable, 2, 11), pattern = "\\.", replacement = "-")
wells$Date = as.Date(wells$Date)
wells_summary = wells[, list(WaterInject = sum(value)), by=c("Grid", "Date")][order(Grid),]


grid_id = unique(wells_summary$Grid)

for (b in grid_id){
  dat = wells_summary[Grid == b, ]
  n = nrow(dat)
  dat$WaterInject = ts(dat$WaterInject, start = c(1980,1), frequency = 12)
  hw_smooth = HoltWinters(dat$WaterInject, beta = F, gamma = F, alpha =0.4)$fitted[,1]
  wells_summary[Grid == b,][2:n,]$WaterInject = hw_smooth
}