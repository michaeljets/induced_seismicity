# =================================================================================================
# AGGREGATING INJECTION DATA

# Author: Michael Jetsupphasuk
# Last Updated: 11 December, 2017

# Pull Date: 11 December, 2017

# This file takes the .xlsx files from http://www.occeweb.com/og/ogdatafiles2.htm
# and aggregates them into a single data frame that is saved as an .rds file

# =================================================================================================

library(dplyr)

# For each year, use an inner join to combine the injection and general data from Access database
# Join by column "PWT__ID"
# This code does not filter for water disposal wells, etc.

years = 2011:2016 # all years available from database
list_dfs = list() # dummy list to store data frames for each year

for (i in 1:length(years)){
  str_general = paste0("rawdata/wells/oklahoma/", years[i], ' 1012A UIC volumes.csv')
  gen = read.csv(str_general, stringsAsFactors = F)
  gen = gen %>% 
          select(API, Lat_Y, Long_X, ReportYear, ends_with('.Vol')) %>%
          group_by(API) %>% 
          mutate(Jan.Vol = max(Jan.Vol),
                 Feb.Vol = max(Feb.Vol),
                 Mar.Vol = max(Mar.Vol),
                 Apr.Vol = max(Apr.Vol),
                 May.Vol = max(May.Vol),
                 Jun.Vol = max(Jun.Vol),
                 Jul.Vol = max(Jul.Vol),
                 Aug.Vol = max(Aug.Vol),
                 Sep.Vol = max(Sep.Vol),
                 Oct.Vol = max(Oct.Vol),
                 Nov.Vol = max(Nov.Vol),
                 Dec.Vol = max(Dec.Vol)) %>%
          distinct() %>%
          data.frame()
  list_dfs[[i]] = gen
}

# Some data frames have different classes for same columns
# Coerce all to "character" to resolve conflicts when binding
list_dfs = lapply(list_dfs, function(df) mutate_all(df, funs('as.character')))

# Combine all data frames into one
raw_injection = bind_rows(list_dfs)

# Save data frame as .rds
saveRDS(raw_injection, 'rawdata/wells/oklahoma/raw_injection_wells.rds')

