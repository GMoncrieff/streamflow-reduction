library(raster)
library(sp)
library(tidyr)
library(dplyr)
library(geoR)
library(spBayes)
library(sp)
library(feather)
#covariates
#from Wilson data
tmax <- raster('data/output/tmax.tif')
tmin <- raster('data/output/tmin.tif')
pcv <- raster('data/output/pcv.tif')
map <- raster('data/output/map.tif')
vars_ras <- stack(tmax,tmin,pcv,map)
vars_ras <- raster::aggregate(vars_ras,4)
vars_ras <- scale(vars_ras)
vars_ras <- projectRaster(vars_ras,crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")
vars <- as(vars_ras,"SpatialPixelsDataFrame")

#fire return data
#mean and sd of fire turn interval
#from Wilson et al 2010
load('data/output/return_fire_50.Rdata')

tmp <- raster(rt, layer = 1)
for (i in 2:nlayers(rt)) {
  tmp <- stack(tmp, raster(rt, layer = i))
}
rt <- tmp
rt <- raster::aggregate(rt,4)


rt_grid <- projectRaster(rt,crs=crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
rt <- as(rt_grid,"SpatialPointsDataFrame")

xy <- coordinates(rt)
xyz<-raster::extract(vars_ras,xy,df=TRUE)

xyz <- cbind(xyz,xy,rt@data)

xyz_long <- gather(xyz,"iter","rt",-c(tmax,tmin,pcv,map,x,y,ID))


####model fitting
coords <- cbind(xyz_long$x + rnorm(nrow(xyz_long),0,0.001), xyz_long$y + rnorm(nrow(xyz_long),0,0.001)) ## the + .001 is to get the spPedict function used in 03-predict.R to work.
## spPredict is not designed to predict directly over observation locations. So very slightly offsetting
## the coordinates makes sure the code doesn't breakdown. 

xyz_var <- xyz_long %>% dplyr::select(tmax,tmin,pcv,map,rt)

rt_mean <- mean(xyz_var$rt)
rt_sd <- sd(xyz_var$rt)

xyz_var <- xyz_var %>% mutate_each_(funs(scale),vars=c("rt"))

## fit a semi-variogram to the residuals of the non-spatial model
## to inform prior selection and starting values for MCMC sampler
m.0 <- lm(xyz_var$rt ~ xyz_var$map + xyz_var$pcv)
vario <- variog(data = resid(m.0), coords = coords)
vario.mod <- variofit(vario, cov.model = "exponential")

plot(vario)
lines(vario.mod)

#bayesian
n.samples=10000
r <- 3
priors <- list("phi.Unif"=c(0.05,1),
               "sigma.sq.IG"=c(2, 1),
               "tau.sq.IG"=c(4, 4))
starting <- list("phi"=0.7, "sigma.sq"=1.1, "tau.sq"=0.001)
tuning <- list("phi"=0.0001, "sigma.sq"=0.0001, "tau.sq"=0.001)

m.0 <- spLM(rt ~ map + pcv, coords=coords, data=xyz_var,
             starting=starting,
             knots=c(20,20),
             tuning=tuning, priors=priors, cov.model="exponential",
             n.samples=n.samples,
             n.report = 100)


## recover Beta and w MCMC samples. This alos removes burn in samples and thins.
m.1 <- spRecover(m.0, start = 5000, thin = 5, n.omp.threads = 32)

## look at MCMC chains for sigma2, tau2 and phi
plot(m.1$p.theta.recover.samples)

## look at MCMC samples for beta0 and beta1
plot(m.1$p.beta.recover.samples)

## save model so it can be loaded in '03-predict.R'
save(m.0, file = "data/output/fitted-age-model_large.RData")
save(m.1, file = "data/output/fitted-age-samples_large.RData")

####predict
load("data/output/fitted-age-model_large.RData")
xyp <- coordinates(vars_ras)
xyp_data <- raster::extract(vars_ras,xyp,df=TRUE)

m.p <- spPredict(m.0, pred.coords = xyp , pred.covars = cbind(1,xyp_data$map,xyp_data$pcv),start=5000,thin=5, n.omp.threads = 16,n.report=10,verbose= T)

y.ppd <- m.p$p.y.predictive.samples ## isolate posterior predictive distribution (ppd) samples
save(y.ppd, file = "data/output/fitted-age-rasters_long.RData")
#load("data/output/fitted-age-rasters_long.RData")

#unscale
y.ppd <- (y.ppd*rt_sd)+rt_mean

#rasterize
y.pred.all = as.data.frame(y.ppd)

## calculate median, 95% credible and standard deviation of ppd
y.ppd.cred <- t(apply(y.ppd, 1, quantile, probs = c(.5,.025,.975),na.rm=TRUE))
y.ppd.sd <- apply(y.ppd,1,sd)
y.pred.sp <- data.frame('y.pred' = y.ppd.cred[,1], 'y.sd' = y.ppd.sd,'y.low' = y.ppd.cred[,2], 'y.high' = y.ppd.cred[,3])

coordinates(y.pred.sp) <- xyp
gridded(y.pred.sp) <- TRUE

spplot(y.pred.sp['y.high'])

#full grid of data
coordinates(y.pred.all) <- xyp
gridded(y.pred.all) <- TRUE
all_grid<-stack(y.pred.all)
all_grid <-clamp(all_grid,lower=0.001)
crs(all_grid)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
all_grid <- all_grid[[1:1000]]
writeRaster(all_grid,"data/output/age_raster.grd")
save(all_grid,file="data/output/age_raster_stack.RData")

#convert to df with cellnumbers
cell <- cellFromXY(all_grid,xyp)
cell <- data.frame(cellnum=cell)
age_df <-  cbind(cell,as.data.frame(all_grid))
write_feather(age_df,"data/output/age_df.feather")
save(age_df,file="data/output/age_raster_df.RData")

     