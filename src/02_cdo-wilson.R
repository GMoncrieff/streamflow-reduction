library(raster)

#proccess climatologies for CFR into mean and sd of annual precipitation

#raw data file weather_fine.nc contains posterior samples of daily weather from 1990 -2009
#data available from Adam Wilson (adamw@buffalo.edu)
#place this file in raw data folder

#citation:
#Wilson, A. M., & Silander, J. A. 2014
#Estimating uncertainty in daily weather interpolations: a Bayesian framework for developing climate surfaces.
#International Journal of Climatology, 34(8), 2573-2584.

#variance ppt
if(!file.exists("data/output/map_sd.tif")){
  system('cd data/raw & cdo -sqrt -timmean -yearsum -sqr -selname,ppt_sd weather_fine.nc mean_sd_ppt.nc')
  map_sd <- raster('data/raw/mean_sd_ppt.nc')
  writeRaster(map_sd,'data/output/map_sd.tif')
}

#mean ppt
if(!file.exists("data/output/map.tif")){
  system('cd data/raw & cdo -b 64 -timmean -yearsum -selname,ppt_mean weather_fine.nc map_ppt.nc')
  map <- raster('data/raw/map_ppt.nc')
  writeRaster(map,'data/output/map.tif')
}

if(!file.exists("data/output/tmax.tif")){
  system('cd data/raw & cdo -b 64 -selsmon,1 -ymonmean -selname,tmax_mean weather_fine.nc tmax.nc')
  tmax <- raster('data/raw/tmax.nc')
  writeRaster(tmax,'data/output/tmax.tif')
}

if(!file.exists("data/output/tmin.tif")){
  system('cd data/raw & cdo -b 64 -selsmon,7 -ymonmean -selname,tmin_mean weather_fine.nc tmin.nc')
  tmin <- raster('data/raw/tmin.nc')
  writeRaster(tmin,'data/output/tmin.tif')
}

if(!file.exists("data/output/mapmon_sd.tif")){
  system('cd data/raw & cdo -b 64 -timstd -ymonsum -selname,ppt_mean weather_fine.nc mapmon_sd.nc')
  mapmon_sd <- raster('data/raw/mapmon_sd.nc')
  writeRaster(mapmon_sd,'data/output/mapmon_sd.tif')
}

