setwd("ftp_data/injection_data/csvs")

library(dplyr)

years = 1977:2017
list_dfs = list()
for (i in 1:length(years)){
  str_general = paste0(years[i], 'general.csv')
  str_inject = paste0(years[i], 'injections.csv')
  gen = read.csv(str_general, stringsAsFactors = F)
  inj = read.csv(str_inject, stringsAsFactors = F)
  combine = inner_join(gen, inj, by = "PWT__ID") %>%
              mutate(Year = years[i])
  list_dfs[[i]] = combine
}

list_dfs = lapply(list_dfs, function(df) mutate_all(df, funs('as.character')))
raw_injection = bind_rows(list_dfs)

write.csv(raw_injection, '../../../rawdata/wells/california/raw_injection_wells.csv')

# clean_injection =  raw_injection %>%
#                     mutate(Year = as.numeric(Year),
#                            Steam.WaterInjected.BBL. = as.numeric(Steam.WaterInjected.BBL.)) %>%
#                     filter(Year >= 1980,
#                            WellTypeCode == "WD")
# 
# write.csv('rawdata/wells/california/clean_injection_wells.csv')

