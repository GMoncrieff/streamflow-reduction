library(raster)
library(sp)
library(GSIF)
library(gstat)

#covariates
#from Wilson data
tmax <- raster('data/output/tmax.tif')
tmin <- raster('data/output/tmin.tif')
pcv <- raster('data/output/mapmon_sd.tif')
map <- raster('data/output/map.tif')
vars_ras <- stack(tmax,tmin,pcv,map)
vars_ras <- projectRaster(vars_ras,crs="+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs ")
vars <- as(vars_ras,"SpatialPixelsDataFrame")

#fire return data
#mean and sd of fire turn interval
#from Wilson et al 2010
load('data/output/return_fire.Rdata')
load('data/output/return_sd.Rdata')
rtm <- as(rtm,"SpatialPointsDataFrame")
rtm <- spTransform(rtm,"+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs ")
rts <- as(rts,"SpatialPointsDataFrame")
rts <- spTransform(rts,"+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs ")

#mean return time
rtmod<-fit.gstatModel(rtm, rtmean~tmax+map+pcv+tmin, vars,method="randomForest")
rtkrig <- predict(rtmod, vars)
rtkrig <-rtkrig@predicted
rtras <- raster(rtkrig)
writeRaster(rtras,'data/output/rtm.tif')

#return time sd
vars_ras <- stack(rtras,vars_ras)
names(vars_ras) <-c("rtras","tmax","tmin","pcv","map")
vars <- as(vars_ras,"SpatialPixelsDataFrame")

rsmod<-fit.gstatModel(rts, rtsd~rtras+tmax+map+pcv+tmin, vars,method="randomForest")
rskrig <- predict(rsmod, vars)
rskrig <-rskrig@predicted
rsras <- raster(rskrig)
writeRaster(rsras,'data/output/rtsd.tif')


