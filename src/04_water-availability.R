library(sp)
library(sf)
library(rgeos)
library(rgdal)
library(gdalUtils)
library(raster)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(feather)

#script to produces raster layers of rparian, ground water and dryland water availabilty

#function to rasterize using gdal
gdal_raster_fast<- function(shape_fname, source_fname, final_fname, temp_res, out_res, sh_attribute, at = TRUE, load_ras = TRUE,resamp = 'average',tempot = 'Byte'){
  #dont run if output exists
  if(!file.exists(final_fname)){
    #step 1 create template raster
    print('step 1: create template raster')
    gdalwarp(srcfile=source_fname,
             dstfile = "data/temp/temp_rasterize.tif",
             ot = tempot,
             r = 'near',
             tr = c(temp_res,temp_res),
             t_srs = '+proj=aea +lat_1=-18 +lat_2=-32 +lat_0=0 +lon_0=24 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0')
    
    #step2 rasterize vector to temp resolution
    print('step2: rasterize vector')
    gdalUtils::gdal_rasterize(src_datasource = shape_fname,
                              dst_filename = "data/temp/temp_rasterize.tif",
                              a = sh_attribute,
                              at = at,
                              init = 0,
                              tr = c(temp_res,temp_res),
                              ot = tempot
    )
    #step 3 resample to output res
    print('step3: resample to output')
    gdalwarp(srcfile="data/temp/temp_rasterize.tif",
             dstfile = final_fname,
             ot = 'Float64',
             r = resamp,
             tr = c(out_res,out_res),
             t_srs = '+proj=aea +lat_1=-18 +lat_2=-32 +lat_0=0 +lon_0=24 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0')
    
    #step 4 clean up
    print('step4: clean  up')
    
    #del temp file
    unlink("data/temp/temp_rasterize.tif")
    #retrun raster
    if(load_ras){
      ras <- raster(final_fname)
    } else {
      ras <- NULL
    }
  } else {
    #if file already exisits return null
    ras <- NULL
  }
  return(ras)
}

#function to get %months of flow for each catchment
get_flows <- function(flows){
  cat<-character(length(flows))
  fl<-numeric(length(flows))
  
  for(i in 1:length(flows)){
    dat <- suppressWarnings(read_table2(flows[i],col_names = FALSE))
    dat <- dat[-c(nrow(dat),nrow(dat)-1),2:13]
    dat <- sum(dat > 0)/sum(!is.na(dat))
    nam <- strsplit(flows[i],'/')[[1]][4]
    nam <- str_replace(nam,".ans","")
    
    cat[i] <- nam
    fl[i]  <- dat
  }
  
  res <- data.frame('QUATERNARY' = cat, 'FLOW_PER' = fl)
  return(res)
}

#template raster and study area ------
#set projecct crs
proj_crs <- '+proj=aea +lat_1=-18 +lat_2=-32 +lat_0=0 +lon_0=24 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'

#CFR extent from Wilson rainfall
if(!file.exists('data/temp/cfr_outline.shp')){
  #remove all na values
  cfr_clim <- raster('data/output/map.tif')
  cfr_clim <- !is.na(cfr_clim)
  cfr_clim[cfr_clim==0] <- NA
  
  #create outline
  cfr_clim <- cfr_clim %>%
    rasterToPolygons(dissolve = T) %>%
    st_as_sf() %>%
    st_transform(proj_crs)
  
  #write
  st_write(cfr_clim,'data/temp/cfr_outline.shp')
} else {
  cfr_clim <- st_read('data/temp/cfr_outline.shp')
}

#select only cfr catchments and join
if(!file.exists('data/temp/catch_ext.shp')){
  catch <- st_read('data/raw/QuatCatch_wr2012/QuatCatch_wr2012.shp') %>%
    st_transform(proj_crs) %>%
    filter(lengths(st_within(., cfr_clim)) > 0)
  st_write(catch,'data/temp/catch_ext.shp')
} else {
  catch <- st_read('data/temp/catch_ext.shp')
}

if(!file.exists("data/temp/template_temp.tif")){
  #create an empty template from NIAPS grid
  niaps <- raster('data/output/NIAPS.grd')
  template <- raster(extent(niaps))
  res(template) <- res(niaps)
  crs(template) <- proj_crs
  
  #set cell values to unique
  vals <- 1:ncell(template)
  template <- setValues(template, vals)
  
  #crop template to catchments extent
  template <- mask(template,catch)
  #empty
  etemp <- template
  etemp[etemp>=0] <- 0
  writeRaster(etemp,file="data/temp/template_temp.tif")
} else {
  etemp <- raster("data/temp/template_temp.tif")
}


#quat catchments ----
#quat catchments and natural flow from WR2012
#http://waterresourceswr2012.co.za/
#Bailey, A. K., & Pitman, W. V. (2015). 
#Water resources of South Africa 2012 study (WR2012). 
#WR2012 Study User’s Guide. Version 1 (No. K5/2143, p. 1). WRC Report.

#study area is quat catch in CFR
if(!file.exists("data/output/catch_numeric.tif")){
  
  #months with no flow %
  flows <- list.files('data/raw/nat_flows',full.names = TRUE)
  fl_dat<- get_flows(flows) %>%
    separate(QUATERNARY,into=c('QCAT','del'),sep=4) %>%
    select(QCAT,FLOW_PER) %>%
    rename(QUATERNARY = QCAT)
  
  #create catchment shape
  catch_fl <- st_read('data/temp/catch_ext.shp') %>%
    left_join(fl_dat,by='QUATERNARY') %>%  #select only cfr catchments and join
    mutate(QNUM = as.numeric(as.factor(QUATERNARY))) #numeric catchment id
  
  #create df
  catch_df <- catch_fl %>%
    as_tibble() %>%
    select(QNUM,QUATERNARY,CURVE,MAR4Q,MAR,FLOW_PER) %>%
    distinct()
  
  #write
  st_write(catch_fl,'data/temp/catch_flow.shp')
  write_feather(catch_df,'data/output/catch.feather')
  
  #rasterize
  gdal_raster_fast(shape_fname='data/temp/catch_flow.shp',source_fname='data/temp/template_temp.tif', final_fname='data/output/catch_numeric.tif', temp_res = 250, out_res = 250, sh_attribute = 'QNUM', at = FALSE, load_ras = FALSE,resamp = 'near',tempot = 'Float64')

} else {
  catch_fl <- st_read('data/temp/catch_flow.shp')
}

#subcatchments
if(!file.exists('data/temp/subcatch.shp')){
  #union catchment
  catchall<- catch %>% 
    mutate(bind = 1) %>% 
    group_by(bind) %>% 
    summarise(b = mean(bind)) %>% 
    st_cast()
  
  subcatch <- st_read('data/raw/NFEPA_riverFEPAs/River_FEPAs.shp') %>%
    st_transform(proj_crs) %>%
    filter(lengths(st_intersects(., catchall)) > 0) %>%
    st_join(catch_fl,largest = TRUE)
  
  st_write(subcatch,'data/temp/subcatch.shp')
  
  #write df
  subdf<-subcatch %>%
    select(UNIT_ID,QNUM) %>%
    st_set_geometry(NULL)
  write_feather(subdf,'data/output/subdf.feather')
  
  #rasterize
  gdal_raster_fast(shape_fname='data/temp/subcatch.shp',source_fname='data/temp/template_temp.tif', final_fname='data/output/subcatch.tif', temp_res = 250, out_res = 250, sh_attribute = 'UNIT_ID', at = FALSE, load_ras = FALSE,resamp = 'near',tempot = 'Float64')
  
} else {
  subcatch <- st_read('data/temp/subcatch.shp')
}

#Groundwater ----
#access to ggroundwater mapped according to Le maitre 2016
#we add seeps mapped by National Wetlands Map v5 to this map
#NWM5:
#http://gsdi.geoportal.csir.co.za/Members/HvDeventer/freshwater-ecosystems-component

if(!file.exists("data/temp/gwater.shp")){
  
  if(!file.exists("data/temp/nwm_temp.shp")){
    fgdb <- "data/raw/SAIIAE_v2_20181106.gdb"
    nwm5<- st_read(dsn=fgdb,layer="NWM5_20181105_v12_AEA") %>%
      st_zm() %>%
      filter(st_is_valid(.)) %>%
      st_transform(proj_crs) %>%
      st_intersection(catch )%>%
      mutate(FLAG = 1)
    
    st_write(nwm5,'data/temp/nwm_temp.shp')
  } else {
    nwm5<-st_read('data/temp/nwm_temp.shp')
  }
  #classes from nwm
  gclass <- c("SEEP")
  nwm_g <- nwm5 %>% 
    filter(CS_L4A %in% gclass) %>%
    select(c("FLAG","Shape")) %>%
    rename(geometry = Shape)
  
  #acess to groundwater mapped according to Le maitre 2016
  gwater_dlm<-st_read('data/raw/GW_mask/GW_mask.shp') %>%
    st_transform(proj_crs) %>%
    st_intersection(catch) %>%
    mutate(FLAG = 1) %>%
    select(c("FLAG","geometry"))
  
  gwater <- rbind(gwater_dlm,nwm_g)
  
  st_write(gwater,'data/temp/gwater.shp')
} else {
  gwater<-st_read('data/temp/gwater.shp')
}

#rasterize at 10m res
gdal_raster_fast(shape_fname='data/temp/gwater.shp',source_fname='data/temp/template_temp.tif', final_fname='data/output/gwater_10m.tif', temp_res = 10, out_res = 250, sh_attribute = 'FLAG', at = FALSE, load_ras = FALSE)

#rivers50 ----
#rivers mapped at 1:50 000
#dataset is not consistent across catchments

if(!((file.exists("data/temp/river_e.shp"))&(file.exists("data/temp/river_p.shp")))){
  
  #read each catchment seprately
  
  rivere <- st_read("data/raw/rivers50/catche_wgs.shp") %>%
    st_transform(proj_crs) %>%
    mutate(FLAG = 1) %>%
    select('RIVCLASS','FLAG','geometry')
  
  riverf <- st_read("data/raw/rivers50/catchf_wgs.shp") %>%
    st_transform(proj_crs) %>%
    mutate(FLAG = 1) %>%
    select('RIVCLASS','FLAG','geometry')
  
  riverg <- st_read("data/raw/rivers50/catchg_wgs.shp") %>%
    st_transform(proj_crs) %>%
    mutate(FLAG = 1) %>%
    select('RIVCLASS','FLAG','geometry')
  
  riverh <- st_read("data/raw/rivers50/catchh_wgs.shp") %>%
    st_transform(proj_crs) %>%
    mutate(FLAG = 1) %>%
    select('RIVCLASS','FLAG','geometry')
  
  riverj <- st_read("data/raw/rivers50/catchj_wgs.shp") %>%
    st_transform(proj_crs) %>%
    mutate(FLAG = 1) %>%
    select('RIVCLASS','FLAG','geometry')
  
  riverk <- st_read("data/raw/rivers50/catchk_wgs.shp") %>%
    st_transform(proj_crs) %>%
    mutate(FLAG = 1) %>%
    select('RIVCLASS','FLAG','geometry')
  
  #combine
  river <- rbind(rivere,riverf,riverg,riverh,riverj,riverk)
  river <- river %>%
    mutate(RIVCLASS=as.character(RIVCLASS)) %>%
    mutate(RIVCLASS=tolower(RIVCLASS))
  
  #split into perr and ephermeral
  river_p <- river %>%
    filter(RIVCLASS == 'perennial')
  
  river_e <- river %>%
    filter(RIVCLASS != 'perennial')
  
  river_n <- river %>%
    filter(is.na(RIVCLASS))
  
  river_e <- rbind(river_e,river_n)
  
  st_write(river_e,'data/temp/river_e.shp')
  st_write(river_p,'data/temp/river_p.shp')
}

#rasterize rivers usng gdal into 10m, 20m

#rasterize ephemeral rivers
gdal_raster_fast(shape_fname='data/temp/river_e.shp',source_fname='data/temp/template_temp.tif', final_fname='data/output/re_10m.tif', temp_res = 10, out_res = 250, sh_attribute = 'FLAG', at = TRUE, load_ras = FALSE)
gdal_raster_fast(shape_fname='data/temp/river_e.shp',source_fname='data/temp/template_temp.tif', final_fname='data/output/re_20m.tif', temp_res = 20, out_res = 250, sh_attribute = 'FLAG', at = TRUE, load_ras = FALSE)

#rasterize perennial rivers
gdal_raster_fast(shape_fname='data/temp/river_p.shp',source_fname='data/temp/template_temp.tif', final_fname='data/output/rp_10m.tif', temp_res = 10, out_res = 250, sh_attribute = 'FLAG', at = TRUE, load_ras = FALSE)
gdal_raster_fast(shape_fname='data/temp/river_p.shp',source_fname='data/temp/template_temp.tif', final_fname='data/output/rp_20m.tif', temp_res = 20, out_res = 250, sh_attribute = 'FLAG', at = TRUE, load_ras = FALSE)


###not run
# #rivers500 ----
# #rivers500 frm SAIIAE2018
# if(!((file.exists("data/temp/river_e.shp"))&(file.exists("data/temp/river_p.shp")))){
#   river <- st_read("data/raw/rivers500/rivers500.shp") %>% 
#     st_transform(proj_crs) %>%
#     mutate(FLAG = 1)
#   #split into perr and ephermeral
#   river_p <- river %>% filter(FLOW == 'P')
#   river_e <- river %>% filter(FLOW == 'E')
#   
#   #rasterize rivers usng gdal into 25m, 50m and 100m
#   st_write(river_e,'data/temp/river_e.shp')
#   st_write(river_p,'data/temp/river_p.shp')
# }
# 
# #rasterize ephemeral rivers
# gdal_raster_fast(shape_fname='data/temp/river_e.shp',source_fname='data/temp/template_temp.tif', final_fname='data/output/re_10m.tif', temp_res = 10, out_res = 250, sh_attribute = 'FLAG', at = TRUE, load_ras = FALSE)
# #gdal_raster_fast(shape_fname='data/temp/river_e.shp',source_fname='data/temp/template_temp.tif', final_fname='data/output/re_25m.tif', temp_res = 25, out_res = 250, sh_attribute = 'FLAG', at = TRUE, load_ras = FALSE)
# #gdal_raster_fast(shape_fname='data/temp/river_e.shp',source_fname='data/temp/template_temp.tif', final_fname='data/output/re_50m.tif', temp_res = 50, out_res = 250, sh_attribute = 'FLAG', at = TRUE, load_ras = FALSE)
# #gdal_raster_fast(shape_fname='data/temp/river_e.shp',source_fname='data/temp/template_temp.tif', final_fname='data/output/re_100m.tif', temp_res = 100, out_res = 250, sh_attribute = 'FLAG', at = TRUE, load_ras = FALSE)
# 
# #rasterize perennial rivers
# gdal_raster_fast(shape_fname='data/temp/river_p.shp',source_fname='data/temp/template_temp.tif', final_fname='data/output/rp_10m.tif', temp_res = 10, out_res = 250, sh_attribute = 'FLAG', at = TRUE, load_ras = FALSE)
# #gdal_raster_fast(shape_fname='data/temp/river_p.shp',source_fname='data/temp/template_temp.tif', final_fname='data/output/rp_25m.tif', temp_res = 25, out_res = 250, sh_attribute = 'FLAG', at = TRUE, load_ras = FALSE)
# #gdal_raster_fast(shape_fname='data/temp/river_p.shp',source_fname='data/temp/template_temp.tif', final_fname='data/output/rp_50m.tif', temp_res = 50, out_res = 250, sh_attribute = 'FLAG', at = TRUE, load_ras = FALSE)
# #gdal_raster_fast(shape_fname='data/temp/river_p.shp',source_fname='data/temp/template_temp.tif', final_fname='data/output/rp_100m.tif', temp_res = 100, out_res = 250, sh_attribute = 'FLAG', at = TRUE, load_ras = FALSE)
# 


# #watercousrses ----
# if(!file.exists("data/temp/modwater.shp")){
#   #modelled watercourse_propability_VB from SAIIAE2018
#   watermod<-st_read('data/raw/watermodel/watermodel.shp') %>%
#     st_transform(proj_crs) %>%
#     st_intersection(catch) %>%
#     mutate(FLAG = 1)
#   
#   st_write(watermod,'data/temp/modwater.shp')
# } else {
#   watermod<-st_read('data/temp/modwater.shp')
# }
# #rasterize at 25m res
# gdal_raster_fast(shape_fname='data/temp/modwater.shp',source_fname='data/temp/template_temp.tif', final_fname='data/output/wmod_25m.tif', temp_res = 25, out_res = 250, sh_attribute = 'FLAG', at = FALSE, load_ras = FALSE)
# 
# 
# 

# #Wetlands ----
# #NWM5_20181105_v12 frm SAIIAE2018
# if(!file.exists("data/temp/nwm_temp.shp")){
#   #wetlands
#   wet_rem <- c("CS","IA")
#     fgdb <- "data/raw/SAIIAE_v2_20181106.gdb"
#     nwm5<- st_read(dsn=fgdb,layer="NWM5_20181105_v12_AEA") %>%
#     st_transform(proj_crs) %>%
#     filter(!CS_L1 %in% wet_rem) %>% #remove coastal and artificial
#     st_intersection(catch )%>%
#     mutate(FLAG = 1)
#   
#   st_write(nwm5,'data/temp/nwm_temp.shp')
# } else {
#   nwm5<-st_read('data/temp/nwm_temp.shp')
# }
# 
# #rasterize at 25m res
# gdal_raster_fast(shape_fname='data/temp/nwm_temp.shp',source_fname='data/temp/template_temp.tif', final_fname='data/output/nwm_25m.tif', temp_res = 25, out_res = 250, sh_attribute = 'FLAG', at = FALSE, load_ras = FALSE)



