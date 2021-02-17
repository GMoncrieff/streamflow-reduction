library(sp)
library(raster)
library(dplyr)
library(readr)
library(tidyr)
library(RArcInfo)
library(stringr)
library(feather)

#citation:
#Agricultural Research Council. National Invasive Alien Plant Survey (NIAPS) 2011 [Raster] 2011. Available from the Biodiversity GIS website, downloaded on 25 February 2019
#unuzipped NIAPS download location
niapshome <- 'data/raw/NIAPSlndscabunRSA/'

#create R raster
niapsrasloc <- paste0(niapshome,'niapslnrsaab1/w001001.adf')
niaps <- raster(niapsrasloc)
writeRaster(niaps,'data/output/NIAPS.grd')

#create R df
niapsdatloc <- paste0(niapshome,'info/')
NIAPSdata <- get.tabledata(niapsdatloc,'NIAPSLNRSAAB1.VAT')
niaps_data <- as_data_frame(NIAPSdata)
colnames(niaps_data) <- str_trim(colnames(niaps_data))

#wrangle
niaps_clean <- niaps_data %>% 
  gather("species","density",5:116) %>% #into rows
  separate(species,c("spp","spp_suf"),-4,remove = FALSE) %>% #split to get levels
  mutate(level=case_when(spp_suf == "ARAV" ~ "average",spp_suf =="ARLW"~ "lower", spp_suf =="ARUP" ~ "upper", TRUE ~ "percent")) %>% #rename levels
  filter(level!="percent") %>% #remove percents
  mutate(percentage = pmin(((density*0.16)/COUNT),1)) %>% #recalc percents
  select(c('VALUE','spp','percentage','level')) %>% #drop cols
  spread(level,percentage) %>% #into cols
  mutate(std=(((average-lower)+(upper-average))/2)/1.96)

#export
write_feather(niaps_clean,'data/output/niaps_clean.feather')
  
