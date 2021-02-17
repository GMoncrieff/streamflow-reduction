library(dplyr)
library(tidyr)
library(ggplot2)
library(rstan)
library(feather)
library(tibble)
library(readr)
library(tidybayes)
library(loo)
library(cowplot)

rstan_options(auto_write = TRUE,javascript=FALSE)
options(mc.cores = parallel::detectCores())

#data prep ----
#catchmetns experiments data
catchments <- read_feather('data/output/catchments.feather')
catchments$locnum <- as.numeric(as.factor(catchments$loc))
# data on species water use from le maitre 2016
spp_curves <- read_csv('data/raw/curves.csv')

#fit model -----
#stan data
stdat <- list(N = nrow(catchments),
              J = max(catchments$locnum),
              K = 5,
              y=catchments$flow_red_per_rescale,
              x_l=catchments$locnum,
              x_i = rep(1,nrow(catchments)), x_a = catchments$age, x_g = as.numeric(catchments$con)-1, x_t = as.numeric(catchments$tree)-1)

mod1=stan_model('src/stan_models/streamflow1.stan') #con:age
mod2=stan_model('src/stan_models/streamflow2.stan') #none
mod3=stan_model('src/stan_models/streamflow3.stan') #spp:age
mod4=stan_model('src/stan_models/streamflow4.stan') #spp:age + con:age
#mod5=stan_model('src/stan_models/streamflow5.stan') #spp:age * con:age
#mod6=stan_model('src/stan_models/streamflow6.stan') #con:age site effect
#mod7=stan_model('src/stan_models/streamflow7.stan') #all site effect

fit1 <- sampling(mod1,data=stdat,chains=2,iter=2000,thin=1,warmup=1000,
                 control=list(max_treedepth=20),seed=42)
fit2 <- sampling(mod2,data=stdat,chains=2,iter=2000,thin=1,warmup=1000,
                 control=list(max_treedepth=20),seed=42)
fit3 <- sampling(mod3,data=stdat,chains=2,iter=2000,thin=1,warmup=1000,
                 control=list(max_treedepth=20),seed=42)
fit4 <- sampling(mod4,data=stdat,chains=2,iter=2000,thin=1,warmup=1000,
                 control=list(max_treedepth=20),seed=42)
#fit5 <- sampling(mod5,data=stdat,chains=2,iter=2000,thin=1,warmup=1000,
#                 control=list(max_treedepth=20),seed=42)
#fit6 <- sampling(mod6,data=stdat,chains=2,iter=2000,thin=1,warmup=1000,
#                 control=list(max_treedepth=20),seed=916438595,init=0)
#fit7 <- sampling(mod7,data=stdat,chains=2,iter=10000,thin=1,warmup=1000,
#                 control=list(max_treedepth=20,adapt_delta = 0.99),seed=42,init=0)

save(fit1,fit2,fit3,fit4,file="data/output/stanfits.Rdata")

#evaluate
log_lik_1 <- extract_log_lik(fit1, merge_chains = FALSE)
log_lik_2 <- extract_log_lik(fit2, merge_chains = FALSE)
log_lik_3 <- extract_log_lik(fit3, merge_chains = FALSE)
log_lik_4 <- extract_log_lik(fit4, merge_chains = FALSE)

r_eff1 <- relative_eff(exp(log_lik_1), cores = 2)
r_eff2 <- relative_eff(exp(log_lik_2), cores = 2)
r_eff3 <- relative_eff(exp(log_lik_3), cores = 2)
r_eff4 <- relative_eff(exp(log_lik_4), cores = 2)


loo_1 <- loo(log_lik_1, r_eff = r_eff1, cores = 2)
loo_2 <- loo(log_lik_2, r_eff = r_eff2, cores = 2)
loo_3 <- loo(log_lik_3, r_eff = r_eff3, cores = 2)
loo_4 <- loo(log_lik_4, r_eff = r_eff4, cores = 2)

comp <- loo_compare(loo_1, loo_2,loo_3,loo_4)
comp

#fit and save predictions for plotting -------
#predictor data
agep <- seq(from=0,to=39,by=1)
conp <- c(0,1)
treep <- c(0,1)
intp <- 1

predf <- expand.grid(agep,conp,treep,intp)
names(predf) <- c("agep","conp","treep","intp")

stdat <- list(N = nrow(catchments),
              M = nrow(predf),
              J = max(catchments$locnum),
              K = 5,
              y=catchments$flow_red_per_rescale,
              x_l=catchments$locnum,
              x_i = rep(1,nrow(catchments)), x_a = catchments$age, x_g = as.numeric(catchments$con)-1, x_t = as.numeric(catchments$tree)-1,
              x_ip = predf$intp, x_ap = predf$agep, x_gp = predf$conp, x_tp = predf$treep)


fit <- stan(file = 'src/stan_models/streamflow1_pred.stan', model_name = "betareg",
            pars = c('predy'),data = stdat,
            chains=2,iter=11000,thin=1,warmup=1000,
            control=list(max_treedepth=20),seed=42) 

##PLOT----

#scott and smith curves
po<-101.5/(1+exp(5.501)*predf$agep^-3.251)
ps<-83.5/(1+exp(5.028)*exp(predf$agep*-0.382))
eo<-101.5/(1+exp(4.275)*predf$agep^-2.971)
es<-95/(1+exp(8.972)*predf$agep^-4.026)

scottsmith<-data.frame(po=po,ps=ps,eo=eo,es=es,age=predf$agep)

#save stan results
pmul <- summary(fit)$summary
pmul <- as.data.frame(pmul[1:160,])
pmul$con <- predf$conp
pmul$age <- predf$agep
pmul$tree <- predf$treep
pmul$int <- with(pmul, interaction(con,  tree))
pmul$int <- factor(as.numeric(pmul$int) - 1)

pmul0 <- pmul %>% filter(int==0)
pmul1 <- pmul %>% filter(int==1)
pmul2 <- pmul %>% filter(int==2)
pmul3 <- pmul %>% filter(int==3)

cat0 <- catchments %>% filter(tree=="euc")
cat1 <- catchments %>% filter(tree=="pin")


E <- ggplot()+
  geom_ribbon(data=pmul0,aes(x=age,ymax=`97.5%`,ymin=`2.5%`),fill="coral2",alpha=0.2)+
  geom_ribbon(data=pmul1,aes(x=age,ymax=`97.5%`,ymin=`2.5%`),fill="cadetblue",alpha=0.2)+
  geom_ribbon(data=pmul0,aes(x=age,ymax=`75%`,ymin=`25%`),fill="coral2",alpha=0.2)+
  geom_ribbon(data=pmul1,aes(x=age,ymax=`75%`,ymin=`25%`),fill="cadetblue",alpha=0.2)+
  geom_line(data=pmul0,aes(x=age,y=mean),color="coral2")+
  geom_line(data=pmul1,aes(x=age,y=mean),color="cadetblue")+
  geom_line(data=scottsmith,aes(x=age,y=eo/100),color="black",size=0.2,linetype=3)+
  geom_line(data=scottsmith,aes(x=age,y=es/100),color="black",size=0.1,linetype=5)+
  geom_point(data=cat0,aes(x=age,y=flow_red_per_rescale,color=con))+
  labs(x="Age(years)",y="Flow reduction proportion",title="Eucalyptus") +
  scale_color_manual(name = "Growing conditions", labels = c("Optimal", "Suboptimal"),values=c("coral2", "cadetblue")) +
  ylim(c(0,1))+
  theme_bw() +
  theme(legend.position = "None")



P <- ggplot()+
  geom_ribbon(data=pmul2,aes(x=age,ymax=`97.5%`,ymin=`2.5%`),fill="coral2",alpha=0.2)+
  geom_ribbon(data=pmul3,aes(x=age,ymax=`97.5%`,ymin=`2.5%`),fill="cadetblue",alpha=0.2)+
  geom_ribbon(data=pmul2,aes(x=age,ymax=`75%`,ymin=`25%`),fill="coral2",alpha=0.2)+
  geom_ribbon(data=pmul3,aes(x=age,ymax=`75%`,ymin=`25%`),fill="cadetblue",alpha=0.2)+
  geom_line(data=pmul2,aes(x=age,y=mean),color="coral2")+
  geom_line(data=pmul3,aes(x=age,y=mean),color="cadetblue")+
  geom_line(data=scottsmith,aes(x=age,y=po/100),color="black",size=0.2,linetype=3)+
  geom_line(data=scottsmith,aes(x=age,y=ps/100),color="black",size=0.1,linetype=5)+
  geom_point(data=cat1,aes(x=age,y=flow_red_per_rescale,color=con))+
  labs(x="",y="",title="Pine") +
  scale_color_manual(name = "Growing conditions", labels = c("Optimal", "Suboptimal"),values=c("coral2", "cadetblue")) +
  ylim(c(0,1))+
  theme_bw() +
  theme(legend.position = "None")

PL <- ggplot()+
  geom_ribbon(data=pmul2,aes(x=age,ymax=`97.5%`,ymin=`2.5%`),fill="coral2",alpha=0.2)+
  geom_ribbon(data=pmul3,aes(x=age,ymax=`97.5%`,ymin=`2.5%`),fill="cadetblue",alpha=0.2)+
  geom_ribbon(data=pmul2,aes(x=age,ymax=`75%`,ymin=`25%`),fill="coral2",alpha=0.2)+
  geom_ribbon(data=pmul3,aes(x=age,ymax=`75%`,ymin=`25%`),fill="cadetblue",alpha=0.2)+
  geom_line(data=pmul2,aes(x=age,y=mean),color="coral2")+
  geom_line(data=pmul3,aes(x=age,y=mean),color="cadetblue")+
  geom_point(data=cat1,aes(x=age,y=flow_red_per_rescale,color=con))+
  labs(x="Age(years)",y="Flow reduction proportion",title="Pine") +
  scale_color_manual(name = "Growing conditions", labels = c("Optimal", "Suboptimal"),values=c("coral2", "cadetblue")) +
  ylim(c(0,1))+
  theme_bw()

L <- cowplot::get_legend(PL)

Plot <- ggdraw(plot_grid(plot_grid(E, P,nrow=1, align='v'),
                 plot_grid(L, nrow=1),
                 rel_widths=c(1, 0.2)))

ggsave("data/results/catchments.png",Plot,width=10,height=5)

###fit and save final model for use in estimating streamflow losses-------
fit <- stan(file = 'src/stan_models/streamflow1_pred.stan', model_name = "betareg",
            pars = c('beta','beta_i','phi'),data = stdat,
            chains=2,iter=2000,thin=2,warmup=1000,
            control=list(max_treedepth=20),seed=42) 


#save stan results
fname = paste0('data/model/stan_streamflow.rds')
fnamef = paste0('data/model/stanfit.feather')
saveRDS(fit,file=fname)


#convert to tidy df
stanfit <- tidybayes::tidy_draws(fit)
write_feather(stanfit,fnamef)



