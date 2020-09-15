library(raster)
library(feather)
library(dplyr)
library(tidyr)
library(gdalUtils)
library(sf)
library(fitdistrplus) 
library(ggplot2)

#script to prepare data for stan modelling

#do you want to view plots?
makeplots <- FALSE

#PART A spatial data prep

#index raster ----
index <- raster('data/temp/template_temp.tif')

if(!file.exists("data/output/index.feather")){
  index_sp <- as_tibble(rasterToPoints(index)) %>%
    dplyr::select(-template_temp)
  index_df <- raster::extract(index,index_sp,cellnumbers=TRUE,df=TRUE) %>% 
    cbind(index_sp) %>%
    dplyr::select(x,y,cells)
  write_feather(index_df,'data/output/index.feather')
} else
{
  index_df <- read_feather('data/output/index.feather')
}

#prep data for extractions

#rivers ----
if(!((file.exists("data/output/river10.tif"))&(file.exists("data/output/river20.tif")))){
  #10m buffer
  rivere10  <- raster('data/output/re_10m.tif')
  rivere10 <- resample(rivere10,index,filename = 'data/temp/re_10m_r.tif',method='ngb')
  
  riverp10  <- raster('data/output/rp_10m.tif')
  riverp10 <- resample(riverp10,index,filename = 'data/temp/rp_10m_r.tif',method='ngb')

  #20m buffer
  rivere20  <- raster('data/output/re_20m.tif')
  rivere20 <- resample(rivere20,index,filename = 'data/temp/re_20m_r.tif',method='ngb')
  
  riverp20  <- raster('data/output/rp_20m.tif')
  riverp20 <- resample(riverp20,index,filename = 'data/temp/rp_20m_r.tif',method='ngb')
  
  #% of months with flow
  flow_per  <- raster('data/output/flow_per.tif')
  flow_per <- resample(flow_per,index,filename = 'data/temp/flow_per_r.tif',method='ngb')
  
  #final prob riparian
  river10 <- riverp10 + (rivere10 * flow_per)
  river10[is.na(river10[])] <- 0 
  writeRaster(river10,'data/output/river10.tif')
  
  river20 <- riverp20 + (rivere20 * flow_per)
  river20[is.na(river20[])] <- 0 
  writeRaster(river20,'data/output/river20.tif')
} else {
  river10 <- raster('data/output/river10.tif')
  river20 <- raster('data/output/river20.tif')
}

#gwater ----
if(!file.exists("data/output/gwater.tif")){
  gwater  <- raster('data/output/gwater_10m.tif')
  gwater  <- resample(gwater,index,filename = 'data/temp/gwater_r.tif',method='ngb')
  gwater[is.na(gwater[])] <- 0
  writeRaster(gwater,"data/output/gwater.tif")
} else {
  gwater <- raster("data/output/gwater.tif")
}

#pixel age (wilson 2010) ----
if(!file.exists("data/output/rt_i.tif")){
  rtm  <- raster('data/output/rtm.tif')
  rtsd  <- raster('data/output/rtsd.tif')
  rtm <- projectRaster(rtm,index,method='ngb')
  rtsd <- projectRaster(rtsd,index,method='ngb')
  
  #sample to same grid and interpolate to fill a few gaps
  rtmi  <- resample(rtm,index,filename = 'data/temp/rtm_r.tif',method='ngb',overwrite=TRUE)
  rtmi <- focal(rtmi, w=matrix(1,nrow=5, ncol=5), fun=mean, NAonly=TRUE, na.rm=TRUE) 
  rtmi <- focal(rtmi, w=matrix(1,nrow=5, ncol=5), fun=mean, NAonly=TRUE, na.rm=TRUE) 
  rtmi <- focal(rtmi, w=matrix(1,nrow=5, ncol=5), fun=mean, NAonly=TRUE, na.rm=TRUE) 
  rtmi <- focal(rtmi, w=matrix(1,nrow=5, ncol=5), fun=mean, NAonly=TRUE, na.rm=TRUE) 
  rtmi <- focal(rtmi, w=matrix(1,nrow=5, ncol=5), fun=mean, NAonly=TRUE, na.rm=TRUE) 
  rtmi <- focal(rtmi, w=matrix(1,nrow=5, ncol=5), fun=mean, NAonly=TRUE, na.rm=TRUE) 
  
  rtsdi  <- resample(rtsd,index,filename = 'data/temp/rtsd_r.tif',method='ngb',overwrite=TRUE)
  rtsdi <- focal(rtsdi, w=matrix(1,nrow=5, ncol=5), fun=mean, NAonly=TRUE, na.rm=TRUE) 
  rtsdi <- focal(rtsdi, w=matrix(1,nrow=5, ncol=5), fun=mean, NAonly=TRUE, na.rm=TRUE) 
  rtsdi <- focal(rtsdi, w=matrix(1,nrow=5, ncol=5), fun=mean, NAonly=TRUE, na.rm=TRUE) 
  rtsdi <- focal(rtsdi, w=matrix(1,nrow=5, ncol=5), fun=mean, NAonly=TRUE, na.rm=TRUE) 
  rtsdi <- focal(rtsdi, w=matrix(1,nrow=5, ncol=5), fun=mean, NAonly=TRUE, na.rm=TRUE) 
  rtsdi <- focal(rtsdi, w=matrix(1,nrow=5, ncol=5), fun=mean, NAonly=TRUE, na.rm=TRUE) 
  
  writeRaster(rtmi,"data/output/rtm_i.tif",overwrite=T)
  writeRaster(rtsdi,"data/output/rtsd_i.tif",overwrite=T)
} else {
  rtmi <- raster("data/output/rtm_i.tif")
  rtsdi <- raster("data/output/rtsd_i.tif")
  
  ages <-stack(rtmi,rtsdi)
index_df <- read_feather('data/output/index.feather')
ages <- raster::extract(ages,index_df[,1:2],cellnumbers=TRUE,df=TRUE)
test<-left_join(sp_data_long,ages,by='cells')

}

# adam wilson climate ----
#precip
if(!file.exists("data/output/map_i.tif")){
  map  <- raster('data/output/map.tif')
  map <- projectRaster(map,index,method='ngb')
  mapi  <- resample(map,index,filename = 'data/temp/map_r.tif',method='ngb',overwrite=TRUE)
  #adams precip map constrains the study area. so no map, no data.
  #mapi[is.na(mapi[])] <- 0
  writeRaster(mapi,"data/output/map_i.tif")
} else {
  mapi <- raster("data/output/map_i.tif")
}

#precip sd
if(!file.exists("data/output/mapsd_i.tif")){
  mapsd  <- raster('data/output/map_sd.tif')
  mapsd <- projectRaster(mapsd,index,method='ngb')
  mapsdi  <- resample(mapsd,index,filename = 'data/temp/mapsd_r.tif',method='ngb')
  #we have a few NA values here (0.01%) that we need to interpolate
  mapsdi <- focal(mapsdi, w=matrix(1,nrow=5, ncol=5), fun=mean, NAonly=TRUE, na.rm=TRUE) 
  mapsdi <- focal(mapsdi, w=matrix(1,nrow=5, ncol=5), fun=mean, NAonly=TRUE, na.rm=TRUE) 
  mapsdi <- focal(mapsdi, w=matrix(1,nrow=5, ncol=5), fun=mean, NAonly=TRUE, na.rm=TRUE) 
  mapsdi <- focal(mapsdi, w=matrix(1,nrow=5, ncol=5), fun=mean, NAonly=TRUE, na.rm=TRUE) 
  mapsdi <- focal(mapsdi, w=matrix(1,nrow=5, ncol=5), fun=mean, NAonly=TRUE, na.rm=TRUE) 
  mapsdi <- focal(mapsdi, w=matrix(1,nrow=5, ncol=5), fun=mean, NAonly=TRUE, na.rm=TRUE) 

  writeRaster(mapsdi,"data/output/mapsd_i.tif")
} else {
  mapsdi <- raster("data/output/mapsd_i.tif")
}

#quat catchments ----
if(!file.exists("data/output/catch_num.tif")){
  catchn  <- raster('data/output/catch_numeric.tif')
  catchnq  <- resample(catchn,index,filename = 'data/temp/catch_num_r.tif',method="ngb")
  writeRaster(catchnq,"data/output/catch_num.tif")
} else {
  catchnq <- raster("data/output/catch_num.tif")
}

#catchment data
catch_df <- read_feather('data/output/catch.feather')


#niaps grid and df ----
#read raster
if(!file.exists("data/output/niaps_sp.tif")){
  niaps <- raster('data/output/NIAPS.grd') #already same as index
  niaps <- mask(niaps,index)
  niaps[is.na(niaps[])] <- 0
  writeRaster(niaps,'data/output/niaps_sp.tif')
} else {
  niaps <- raster('data/output/niaps_sp.tif')
}

niaps_df <- read_feather('data/output/niaps_clean.feather')

#read and spread df
if(!file.exists("data/output/niaps_long.feather")){
  
  #create 0 for all species when niaps has na
  species <- unique(niaps_df$spp)
  spp0 <- tibble(VALUE = rep(0,length(species)),spp = species, average = rep(0,length(species)),lower = rep(0,length(species)), upper = rep(0,length(species)),std = rep(0,length(species)))
  #add to naips data and sppreaad to long
  niaps_long <- rbind(niaps_df,spp0) %>%
    dplyr::rename(niaps_hmu = VALUE) %>%
    dplyr::select(niaps_hmu,spp,average,std) %>%
    tidyr::nest(average, std, .key = 'value_col') %>%
    dplyr::mutate(spp = as.factor(spp)) %>%
    tidyr::spread(spp,value_col,fill=0) %>%
    tidyr::unnest(.sep = '_')
  
  niaps_sd <- niaps_long %>%
    dplyr::select(contains("_std")) %>%
    cbind(niaps_long$niaps_hmu,.) %>%
    rename('niaps_hmu' = 'niaps_long$niaps_hmu')
  
  niaps_av <- niaps_long %>%
    dplyr::select(contains("_average")) %>%
    cbind(niaps_long$niaps_hmu,.) %>%
    rename('niaps_hmu' = 'niaps_long$niaps_hmu')
  
  write_feather(niaps_long,'data/output/niaps_long.feather')
  write_feather(niaps_sd,'data/output/niaps_sd.feather')
  write_feather(niaps_av,'data/output/niaps_av.feather')
  
} else {
  niaps_long <- read_feather('data/output/niaps_long.feather')
  niaps_sd <- read_feather('data/output/niaps_sd.feather')
  niaps_av <- read_feather('data/output/niaps_av.feather')
}
  
#extractions ----

if(!file.exists('data/output/pixel_ras.grd')){
  #for  extracting individual pixels
  pixel_ras <- stack(catchnq,subcatchnq,gwater,river10,river20,mapi,mapsdi,rtmi,rtsdi)
  pixel_ras <- trim(pixel_ras)
  writeRaster(pixel_ras,"data/output/pixel_ras.grd", format="raster")
}

if(!file.exists('data/output/sp_data_long.feather')){
  
  all_ras <- stack(niaps,catchnq,subcatchnq,gwater,river10,river20,mapi,mapsdi,rtmi,rtsdi)
  all_ras <- trim(all_ras)
  
  #extract
  sp_data <- raster::extract(all_ras,index_df[,1:2],cellnumbers=TRUE,df=TRUE)
  sp_data <- sp_data %>%
    filter(catch_num != 0)
  
  #spatial data with niaps
  sp_data_long <- sp_data %>%
    dplyr::rename('niaps_hmu' = 'niaps_sp','QNUM' = 'catch_num') %>%
    left_join(niaps_long,by='niaps_hmu') %>%
    left_join(catch_df,by='QNUM')
  
  #spatial data without niaps
  sp_data_short <- sp_data %>%
    dplyr::rename('niaps_hmu' = 'niaps_sp','QNUM' = 'catch_num') %>%
    left_join(catch_df,by='QNUM')
  
  write_feather(sp_data_long,'data/output/sp_data_long.feather')
  write_feather(sp_data_short,'data/output/sp_data_short.feather')

} else {
  sp_data_long <- read_feather('data/output/sp_data_long.feather')
  sp_data_short <- read_feather('data/output/sp_data_short.feather')
}
  

#paired catchment data ----
if(!file.exists('data/output/catchments.feather')){
  catchments <- read_feather('data/output/catchments_raw.feather')
  catchments <- catchments %>%
    dplyr::select(age,tree,con,flow_red_per_rescale,loc) %>%
    na.omit() %>% 
    as_tibble()
  
  if(makeplots){
    ggplot(catchments,aes(x=age,y=flow_red_per_rescale)) +
      geom_point(aes(color=con),alpha=0.7) +
      facet_wrap(~tree) +
      theme_classic()
    ggsave('data/results/catchment_data.pdf')
  }
  write_feather(catchments,'data/output/catchments.feather')
  
} else {
  catchments <- read_feather('data/output/catchments.feather')
}
