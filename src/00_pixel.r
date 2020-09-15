library(dplyr)
library(tidyr)
library(tibble)
library(feather)
library(raster)

#read spatial data
pixels <- stack("data/output/pixel_ras.grd")

#INSERT COORDS
xy <- cbind(-390087.4, -3703355)
sp_data_long <- extract(pixels, xy) %>% as_data_frame()
sp_data_long$niaps_hmu <- 0
 
#fitted bayes models ----
stanfit <- read_feather('data/model/stanfit.feather') %>%
  filter(.draw<=1000)

#age of each pixel
##INSERT AGE
av_age <- 10

stansamp <- nrow(stanfit)

# #niaps data ----
# niaps_long <- read_feather('data/output/niaps_long.feather')
# niaps_long <- niaps_long[1,]
# niaps_long[1,] <- 0
# sp_data_long <- cbind(pixel,niaps_long)

# data on species water use from le maitre 2016 ----
spp_curves <- read_csv('data/raw/curves.csv')
spp_curves <- spp_curves %>% arrange(spp)

#niaps ----
niaps_sd <- read_feather('data/output/niaps_sd.feather')
niaps_sd <- niaps_sd[1,]
niaps_av <- read_feather('data/output/niaps_av.feather')
niaps_av <- niaps_av[1,]
#create table to join rows to niaps hmu
niaps_join <- tibble(niaps_hmu = niaps_av$niaps_hmu,niaps_rowid = seq(1:nrow(niaps_sd)))
sp_data_long <- sp_data_long %>%
  left_join(niaps_join,by = 'niaps_hmu')

##INSERT DENSITY INTO NIAPS AV
niaps_av$PINUSS_average <- 0.95

#select and order speceis
niaps_hmu<-niaps_av$niaps_hmu
niaps_av <- niaps_av %>%
  dplyr::select(-matches('niaps|AVDENS|AGAVES|CJAMAC|OPUNTS|PROSOS'))
niaps_av <- niaps_av[,order(colnames(niaps_av))]
niaps_av <- cbind(niaps_hmu,niaps_av)
niaps_av <- as.numeric(niaps_av)
#select and order speceis
niaps_hmu<-niaps_sd$niaps_hmu
niaps_sd <- niaps_sd %>%
  dplyr::select(-matches('niaps|AVDENS|AGAVES|CJAMAC|OPUNTS|PROSOS'))
niaps_sd <- niaps_sd[,order(colnames(niaps_sd))]
niaps_sd <- cbind(niaps_hmu,niaps_sd)
niaps_sd <- as.numeric(niaps_sd)

#water availabilty ----
#mean and sd of groundwater availabilty
g_mean <- 1.2
g_sd <- 0.1

#mean and sd of riparian availabilty
r_mean <- 2
r_sd <- 0.2


#catchments ----
catch_df <- read_feather('data/output/catch.feather')
#fix some broken values
catch_df$CURVE[which(catch_df$QNUM==91)] <- 5
catch_df$MAR4Q[which(catch_df$QNUM==91)] <- 11.29
catch_df$MAR[which(catch_df$QNUM==91)] <- 46.87

sp_data_long <- sp_data_long %>%
  dplyr::rename('QNUM' = 'catch_num') %>%
  left_join(catch_df,by='QNUM')

cmar <- sp_data_long %>%
  dplyr::select(QNUM,MAR4Q) %>%
  group_by(QNUM) %>%
  summarise(cmar = max(MAR4Q)) %>%
  arrange(QNUM)

#clean up
sp_data_long$ID <- 1

#all data as list ----
pdat = list(ID = sp_data_long$ID,
            sp_curve = spp_curves$spp_curve, gr_curve = spp_curves$spp_opt,
            r_mean = r_mean, r_sd = r_sd, g_mean = g_mean, g_sd = g_sd,
            flow_per = sp_data_long$FLOW_PER, river10 = pmin(pmax(sp_data_long$river10,0),1), gwater = pmin(pmax(sp_data_long$gwater,0),1),
            precip_m = sp_data_long$map_i, precip_sd = sp_data_long$mapsd_i,
            c1 = (225+(135*sp_data_long$CURVE)), c2 = (150+(90*sp_data_long$CURVE)),
            c3 = (75+(45*sp_data_long$CURVE))
)

#age
x_a_p <- av_age

#water availability
r_riparian=pmax(1,rnorm(stansamp,pdat$r_mean,pdat$r_sd))
r_ground=pmax(1,rnorm(stansamp,pdat$g_mean,pdat$g_sd))

#spp curves
x_g_p <- matrix(,stansamp,length(pdat$gr_curve))
x_t_p <- matrix(,stansamp,length(pdat$sp_curve))

for (k in 1:length(pdat$sp_curve)){
  #sample condtion
  x_g_p[,k] <- rbinom(stansamp, 1,pdat$gr_curve[k])
  #sample tree
  x_t_p[,k] <- rbinom(stansamp, 1,pdat$sp_curve[k])
}

lost_prop <- numeric(stansamp)
lost_per <- numeric(stansamp)
lost_abs <- numeric(stansamp)

#loop samples
for (j in 1:stansamp) {

           #densities for each sample
           ndens<-numeric(length(niaps_av)-1)
           for(n in 1:(length(ndens)-1)){
             ndens[n] <- max(0,min(1,rnorm(1,niaps_av[1+n],niaps_sd[1+n])))
           }
           
           #curves for species n in subcatchment m
           #same curves pars in each run
           #spp specific optimality - subcatchmment specific age
           yred<-numeric(length(pdat$sp_curve))
           
           for(n in 1:length(yred)){
             #calculate reduction
             #sample interaction
             inter_p <- x_a_p * x_g_p[j,n]
             Xbeta_p <- 1 * stanfit$`beta[1]`[j] + x_a_p * stanfit$`beta[2]`[j] + x_g_p[j,n] * stanfit$`beta[3]`[j] + x_t_p[j,n] * stanfit$`beta[4]`[j] + inter_p * stanfit$`beta_i`[j]
             #logistic transform
             mu_p = (1/(1+exp(-Xbeta_p))) 
             #refactor pars
             A_p = mu_p * stanfit$phi[j] %>%
               pmax(0.000001,.)
             B_p = (1.0 - mu_p) * stanfit$phi[j] %>%
               pmax(0.000001,.)
             #reduction at 100% cancov
               yred[n] = A_p/(A_p+B_p) #mean
               #yred[n] = rbeta(1,A_p, B_p) #sample
           }
           

           #water modification
           #sample riparian
           x_riparian <- pdat$flow_per*pdat$river10
           
           #sample groundwater
           x_ground <- pdat$gwater
           
           #water availabity modifier
           wmod <- x_riparian*r_riparian[j] + (1-pdat$river10)*x_ground*r_ground[j] + (1-pdat$river10)*(1-x_ground)
           wmod <- max(1,wmod)
           
           #actual reduction for all spp
           redfac <- yred * ndens
           redsum <- min(1,sum(redfac))

           
           #rainfall-runoff
           precip <- rnorm(1,pdat$precip_m,pdat$precip_sd) %>%
             max(0,.)
           
           runoff <- (precip - pdat$c1 + 3) +
             (pdat$c2/(exp((precip-pdat$c3)/pdat$c2)))
           
           #output results
           lost_abs[j] <- (runoff*10000) * wmod * redsum
           lost_per[j] <- ((runoff*10000) * wmod * redsum)/(runoff*10000)
           lost_prop[j] <- redsum
         } 

hist(lost_abs,xlim=c(0,300000))
#assuming an average 3*6*1.5 swimming pool
pool <- 3*6*1.5*1000
quantile(lost_abs/pool, probs = c(0.25, 0.75))


