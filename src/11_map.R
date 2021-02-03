library(sf)
library(raster)
library(tmap)
#install.packages("rnaturalearth")
#install.packages("rnaturalearthdata")
#install.packages("elevatr")
library(rnaturalearth)
library(elevatr)

rsabox <- st_bbox(c(xmin = 15, xmax = 33, ymax = -21, ymin = -37), crs = "+init=epsg:4326")
sites <- read_sf("data/moncrieff_sites.gpkg") %>% st_transform(crs= "+init=epsg:4326")
catch<-read_sf('data/temp/catch_ext.shp')  %>% st_transform(crs = "+init=epsg:4326")
rsa <- ne_countries(scale = "medium",country = 'south africa',returnclass='sf') %>% st_transform(crs = "+init=epsg:4326") %>% st_crop(rsabox)


#region for downloading elevation cape
catchbuf <- st_buffer(catch,0.1)
bbsp <- as(catchbuf, Class = "Spatial")
bb_utm <- st_bbox(catchbuf)
bb_map <- st_as_sfc(st_bbox(bb_utm))

#get hillshade cape
elevation <- get_elev_raster(bbsp, z = 7,clip="bbox")
elevation2 <- elevation*10
elevation2[elevation2<=0]<-NA
elevationp <- elevation
elevationp[elevationp<=0]<-NA
slope = terrain(elevation2, opt='slope')
aspect = terrain(elevation2, opt='aspect')
hill = hillShade(slope, aspect,20, 270)
hill <- projectRaster(hill,crs= "+init=epsg:4326")

#region for downloading elevation rsa
rsabuf<- st_buffer(rsa,2)
bbsp_sa <- as(rsabuf, Class = "Spatial")
bb_utm_sa <- st_bbox(rsabox)
bb_map_sa <- st_as_sfc(st_bbox(bb_utm_sa))

#get hillshade rsa
elevationsa <- get_elev_raster(bbsp_sa, z = 5,clip="bbox")
elevation2sa <- elevationsa*10
elevation2sa[elevation2sa<=0]<-NA
elevation2sa <- mask(elevation2sa,rsa)
slopesa = terrain(elevation2sa, opt='slope')
aspectsa = terrain(elevation2sa, opt='aspect')
hillsa = hillShade(slopesa, aspectsa, 20, 270)
hillsa <- projectRaster(hillsa,crs= "+init=epsg:4326")

#overview map
zafmap <- tm_shape(rsa) + 
  tm_fill(col='grey80',alpha=0.1) +
  tm_shape(hillsa) +
  tm_raster(palette = gray(0:100 / 100), n = 100, legend.show = F,alpha=0.3)  +
  tm_shape(catch) + 
  tm_borders(lwd = 0.5) +
  tm_shape(bb_map) + 
  tm_borders(lwd = 1.5) +
  tm_shape(sites) + 
  tm_dots(size=0.3,col='grey20',alpha=0.5) +
  tm_compass(type = "8star", position = c("left", "top")) +
  tm_scale_bar(breaks = c(0, 100, 200), text.size = 0.5,width=200) +
  tm_layout(frame = FALSE) +
  tm_grid(ticks = FALSE,lines=FALSE)

#zafmap

cfrmap <- tm_shape(rsa,bbox=bb_map) + 
  tm_fill(col='grey80',alpha=0.1) +
  tm_shape(hill) +
  tm_raster(palette = gray(0:100 / 100), n = 100, legend.show = F,alpha=0.3)  +
  tm_shape(elevationp) +
  tm_raster(palette = 'plasma', n = 20, legend.show = F,alpha=0.25)  +
  tm_shape(catch) + 
  tm_borders(lwd = 0.3) +
  tm_layout(frame = FALSE) 

#both
bmap <- tmap_arrange(zafmap, cfrmap)
tmap_save(bmap, "data/results/map.png")
