library(dplyr)
library(tidyr)
library(tibble)
library(tidybayes)
library(feather)
library(doParallel)
library(foreach)
library(readr)
library(bigstatsr)
library(mixdist)

setwd("/home/rstudio/streamflow")

#choose uncertainty to unfreeze
#uncer <- commandArgs(trailingOnly=TRUE)
uncer <- 0

uncer <- as.numeric(uncer)

#0 = all
#1 = none
#2 = water
#3 = precip
#4 = curve
#6 = niaps
#7 = spp curves
#8 = age


mc.cores <- parallel::detectCores()-1

#fitted bayes models ----
stanfit <- read_feather('data/model/stanfit.feather') %>%
  filter(.draw<=1000)

if(!(uncer %in% c(4,0))) {
  stanfit <- stanfit %>% 
    mutate(`beta[1]`=mean(`beta[1]`),
           `beta[2]`=mean(`beta[2]`),
           `beta[3]`=mean(`beta[3]`),
           `beta[4]`=mean(`beta[4]`),
           beta_i = mean(beta_i),
           phi = mean(phi))
}

stansamp <- nrow(stanfit)
#stansamp <- 1000

#spatial data ----
sp_data_long <- read_feather('data/output/sp_data_r1_short.feather') %>% 
  rename(cellnum=age_cell)

#age data ----
#age_raster <- stack("data/output/age_raster.grd")
age_df <- read_feather('data/output/age_df.feather')

# data on species water use from le maitre 2016 ----
spp_curves <- read_csv('data/raw/curves.csv')
spp_curves <- spp_curves %>% arrange(spp)

#niaps ----
niaps_sd <- read_feather('data/output/niaps_sd.feather')
niaps_av <- read_feather('data/output/niaps_av.feather')
#create table to join rows to niaps hmu
niaps_join <- tibble(niaps_hmu = niaps_av$niaps_hmu,niaps_rowid = seq(1:nrow(niaps_sd)))
sp_data_long <- sp_data_long %>%
  left_join(niaps_join,by = 'niaps_hmu')

#select and order speceis
niaps_hmu<-niaps_av$niaps_hmu
niaps_av <- niaps_av %>%
  dplyr::select(-matches('niaps|AVDENS|AGAVES|CJAMAC|OPUNTS|PROSOS'))
niaps_av <- niaps_av[,order(colnames(niaps_av))]
niaps_av <- cbind(niaps_hmu,niaps_av)
niaps_av <- as.matrix(niaps_av)
#select and order speceis
niaps_hmu<-niaps_sd$niaps_hmu
niaps_sd <- niaps_sd %>%
  dplyr::select(-matches('niaps|AVDENS|AGAVES|CJAMAC|OPUNTS|PROSOS'))
niaps_sd <- niaps_sd[,order(colnames(niaps_sd))]
niaps_sd <- cbind(niaps_hmu,niaps_sd)
niaps_sd <- as.matrix(niaps_sd)

if(!(uncer %in% c(6,0))) {
  niaps_sd[,2:ncol(niaps_sd)] <- niaps_sd[,2:ncol(niaps_sd)]*0
}

#water availabilty ----
#mean and sd of groundwater availabilty
g_mean <- 1.2
g_sd <- 0.1

#mean and sd of riparian availabilty
r_mean <- 2
r_sd <- 0.2

#manipulate sd
if(!(uncer %in% c(2,0))){
  g_sd <- g_sd *0
  r_sd <- r_sd *0
}

#catchments ----
#quat catches
cmar <- sp_data_long %>%
  dplyr::select(QNUM,MAR4Q) %>%
  group_by(QNUM) %>%
  summarise(cmar = max(MAR4Q)) %>%
  arrange(QNUM)

#to test on a subset
# sp_data_long <- sp_data_long %>%
#   filter(QNUM %in% c(81,138))
# cmar <- cmar[c(81,138),]
# stansamp <- 1000

#fix some broken values
cmar$cmar[which(cmar$QNUM==91)] <- 11.29
sp_data_long$CURVE[which(sp_data_long$QNUM==91)] <- 5
sp_data_long$MAR4Q[which(sp_data_long$QNUM==91)] <- 11.29
sp_data_long$MAR[which(sp_data_long$QNUM==91)] <- 46.87

#for rejoining later
sp_data_join <- sp_data_long %>%
  select(c('ID','QNUM')) %>%
  rename(PID = ID)

save(sp_data_join,cmar,file='data/temp/join_data.RData')

#if no precip uncertainty
if(!(uncer %in% c(3,0))){
  sp_data_long$mapsd_i <- sp_data_long$mapsd_i *0
}

#if no age uncertainty
if(!(uncer %in% c(8,0))){
  age_mean <- data.frame(cellnum=age_df[,1], av=rowMeans(age_df[,-1],na.rm = TRUE))
  age_df[,-1] <- age_mean$av
}

#if no spp curve uncertainty
if(!(uncer %in% c(7,0))){
  spp_curves$spp_opt <- round(spp_curves$spp_opt)
  spp_curves$spp_curve <- round(spp_curves$spp_curve)
}

#create big age table ----
sp_data_join_age<-sp_data_long %>% 
  dplyr::select(cellnum)

#all data as list ----
pdat = list(M = nrow(sp_data_long), ID = sp_data_long$ID, NNUM = sp_data_long$niaps_rowid,
            sp_curve = spp_curves$spp_curve, gr_curve = spp_curves$spp_opt,
            cellnum = sp_data_long$cellnum,
            r_mean = r_mean, r_sd = r_sd, g_mean = g_mean, g_sd = g_sd,
            flow_per = sp_data_long$FLOW_PER, river10 = pmin(pmax(sp_data_long$river10,0),1), gwater = pmin(pmax(sp_data_long$gwater,0),1),
            precip_m = sp_data_long$map_i, precip_sd = sp_data_long$mapsd_i,
            c1 = (225+(135*sp_data_long$CURVE)), c2 = (150+(90*sp_data_long$CURVE)),
            c3 = (75+(45*sp_data_long$CURVE))
)


#results table ----
stream_names <- c('PID','sample','runoff','runoff_red')
stream_big <- FBM(pdat$M*stansamp,4)


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

#parallel
cl <- makeCluster(mc.cores)
registerDoParallel(cl)

#loop samples
foreach (j = 1:stansamp,
         .combine = c,
         .packages = c('dplyr',
                       'bigstatsr'
         )) %dopar% {
           
           #manual progressbar 
           line=paste0(as.character(j/stansamp*100),' %')
           write(line,file="data/temp/progress.txt",append=TRUE)  
           
           #densities for each sample
           ndens<-matrix(,nrow(niaps_av),ncol(niaps_av)-1)
           for(n in 1:ncol(ndens)){
             ndens[,n] <- pmax(0,pmin(1,rnorm(nrow(niaps_av),niaps_av[,1+n],niaps_sd[,1+n])))
           }
           
           #age for each pixel
           age_join<-age_df[,c(1,j+1)]
           x_a_p <- left_join(sp_data_join_age,age_join,by='cellnum') %>%
             dplyr::select(-cellnum)
           x_a_p <- unlist(x_a_p[,1])
           
           #curves for species n 
           #same curves pars in each run
           yred<-matrix(,pdat$M,length(pdat$sp_curve))
           
           for(n in 1:ncol(yred)){
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
             if(!(uncer %in% c(4,0))){
               yred[,n] = A_p/(A_p+B_p)
             } else {
               yred[,n] = rbeta(nrow(yred),A_p, B_p)
             }
           }
           
           #pixels
           index <- seq(1:pdat$M)+pdat$M*(j-1)
           stream_big[index,1] <- pdat$ID
           
           #water modification
           #sample riparian
           x_riparian <- pdat$flow_per*pdat$river10
           
           #sample groundwater
           x_ground <- pdat$gwater
           
           #water availabity modifier
           wmod <- x_riparian*r_riparian[j] + (1-pdat$river10)*x_ground*r_ground[j] + (1-pdat$river10)*(1-x_ground)
           wmod <- pmax(1,wmod)
           
           #actual reduction for all spp
           redfac <- yred * ndens[pdat$NNUM,]
           redsum <- pmin(1,rowSums(redfac))
           #cancov<-rowSums(ndens[pdat$NNUM,])
           
           #rainfall-runoff
           precip <- rnorm(pdat$M,pdat$precip_m,pdat$precip_sd) %>%
             pmax(0,.)
           
           runoff <- (precip - pdat$c1 + 3) +
             (pdat$c2/(exp((precip-pdat$c3)/pdat$c2)))
           
           #write results
           stream_big[index,2] <- j
           stream_big[index,3] <- runoff
           stream_big[index,4] <- runoff * wmod * redsum
           
           NULL
         } # end sample loop

stopCluster(cl)

#write final output
bname <- paste0('data/results/stream_big_',uncer,'.csv')
big_write(stream_big, bname,every_nrow=1000000,progress=TRUE)
#clean up
rm(stream_big)
unlink("data/temp/progress.txt")

