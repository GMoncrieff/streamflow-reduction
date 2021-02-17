library(feather)
library(dplyr)
library(tidyr)
library(purrr)
library(raster)
library(tidybayes)
library(sf)
library(readxl)
library(ggridges)
library(ggplot2)
library(viridis)
library(readr)
library(rasterVis)
library(ggpubr)
library(fasterize)
library(colorspace)
library(stringr)
library(tricolore)
library(ggpubr)
library(xtable)

#uncer <- commandArgs()[1]
uncer <- 0

#process pixels level data in rasters ----
output_ras <- function(index,pattern,uncer,crs_index,sp_data)
{
  fname_mean <- paste0('data/spatial/',pattern, uncer,'_mean.tif')
  fname_u95 <- paste0('data/spatial/',pattern, uncer,'_u95.tif')
  fname_l05 <- paste0('data/spatial/',pattern, uncer,'_l05.tif')
  fname_mad <- paste0('data/spatial/',pattern, uncer,'_mad.tif')
  fname_mcv <- paste0('data/spatial/',pattern, uncer,'_mcv.tif')
  
  if(pattern == 'rn'){
    tbl <- read_feather(paste0('data/results/pixels_',uncer,'.feather')) %>% 
      dplyr::select(c('PID2','lower_ru','upper_ru','mean_ru')) %>% 
      rename(ID = PID2,.lower=lower_ru,.upper=upper_ru,.mean=mean_ru) %>%
      left_join(sp_data,by ='ID')
  } else if (pattern == 'rr') {
    tbl <- read_feather(paste0('data/results/pixels_',uncer,'.feather')) %>% 
      dplyr::select(c('PID2','lower_re','upper_re','mean_re')) %>% 
      rename(ID = PID2,.lower=lower_re,.upper=upper_re,.mean=mean_re) %>%
      left_join(sp_data,by ='ID')
  } else if (pattern == 'pc') {
    tbl <- read_feather(paste0('data/results/pixels_',uncer,'.feather')) %>% 
      dplyr::select(c('PID2','lower_pc','upper_pc','mean_pc')) %>% 
      rename(ID = PID2,.lower=lower_pc,.upper=upper_pc,.mean=mean_pc) %>%
      left_join(sp_data,by ='ID')
  } else if (pattern == 'mad') {
    tbl <- read_feather(paste0('data/results/mad_',uncer,'.feather')) %>% 
      dplyr::select(c('PID2','MAD4',"MED3")) %>% 
      mutate(MCV=MAD4/MED3) %>%
      rename(ID = PID2,mcv=MCV,mad=MAD4) %>%
      left_join(sp_data,by ='ID')
  }
  
  if(pattern == 'mad'){
    ras_mad <- tbl %>%
      dplyr::select(c('x','y','mad')) %>%
      rasterFromXYZ(crs = crs_index,res=res(index))
    writeRaster(ras_mad,fname_mad,overwrite=T)
    
    ras_mcv <- tbl %>%
      dplyr::select(c('x','y','mcv')) %>%
      rasterFromXYZ(crs = crs_index,res=res(index))
    writeRaster(ras_mcv,fname_mcv,overwrite=T)
  } else {
    ras_mean <- tbl %>%
      dplyr::select(c('x','y','.mean')) %>%
      rasterFromXYZ(crs = crs_index,res=res(index))
    writeRaster(ras_mean,fname_mean,overwrite=T)
    
    ras_l05 <- tbl %>%
      dplyr::select(c('x','y','.lower')) %>%
      rasterFromXYZ(crs = crs_index,res=res(index))
    writeRaster(ras_l05,fname_l05,overwrite=T)
    
    ras_u95 <- tbl %>%
      dplyr::select(c('x','y','.upper')) %>%
      rasterFromXYZ(crs = crs_index,res=res(index))
    writeRaster(ras_u95,fname_u95,overwrite=T)
  }
}

#turbo color ----

A<-c((0.18995),(0.19483),(0.19956),(0.20415),(0.2086),(0.21291),(0.21708),(0.22111),(0.225),(0.22875),(0.23236),(0.23582),(0.23915),(0.24234),(0.24539),(0.2483),(0.25107),(0.25369),(0.25618),(0.25853),(0.26074),(0.2628),(0.26473),(0.26652),(0.26816),(0.26967),(0.27103),(0.27226),(0.27334),(0.27429),(0.27509),(0.27576),(0.27628),(0.27667),(0.27691),(0.27701),(0.27698),(0.2768),(0.27648),(0.27603),(0.27543),(0.27469),(0.27381),(0.27273),(0.27106),(0.26878),(0.26592),(0.26252),(0.25862),(0.25425),(0.24946),(0.24427),(0.23874),(0.23288),(0.22676),(0.22039),(0.21382),(0.20708),(0.20021),(0.19326),(0.18625),(0.17923),(0.17223),(0.16529),(0.15844),(0.15173),(0.14519),(0.13886),(0.13278),(0.12698),(0.12151),(0.11639),(0.11167),(0.10738),(0.10357),(0.10026),(0.0975),(0.09532),(0.09377),(0.09287),(0.09267),(0.0932),(0.09451),(0.09662),(0.09958),(0.10342),(0.10815),(0.11374),(0.12014),(0.12733),(0.13526),(0.14391),(0.15323),(0.16319),(0.17377),(0.18491),(0.19659),(0.20877),(0.22142),(0.23449),(0.24797),(0.2618),(0.27597),(0.29042),(0.30513),(0.32006),(0.33517),(0.35043),(0.36581),(0.38127),(0.39678),(0.41229),(0.42778),(0.44321),(0.45854),(0.47375),(0.48879),(0.50362),(0.51822),(0.53255),(0.54658),(0.56026),(0.57357),(0.58646),(0.59891),(0.61088),(0.62233),(0.63323),(0.64362),(0.65394),(0.66428),(0.67462),(0.68494),(0.69525),(0.70553),(0.71577),(0.72596),(0.7361),(0.74617),(0.75617),(0.76608),(0.77591),(0.78563),(0.79524),(0.80473),(0.8141),(0.82333),(0.83241),(0.84133),(0.8501),(0.85868),(0.86709),(0.8753),(0.88331),(0.89112),(0.8987),(0.90605),(0.91317),(0.92004),(0.92666),(0.93301),(0.93909),(0.94489),(0.95039),(0.9556),(0.96049),(0.96507),(0.96931),(0.97323),(0.97679),(0.98),(0.98289),(0.98549),(0.98781),(0.98986),(0.99163),(0.99314),(0.99438),(0.99535),(0.99607),(0.99654),(0.99675),(0.99672),(0.99644),(0.99593),(0.99517),(0.99419),(0.99297),(0.99153),(0.98987),(0.98799),(0.9859),(0.9836),(0.98108),(0.97837),(0.97545),(0.97234),(0.96904),(0.96555),(0.96187),(0.95801),(0.95398),(0.94977),(0.94538),(0.94084),(0.93612),(0.93125),(0.92623),(0.92105),(0.91572),(0.91024),(0.90463),(0.89888),(0.89298),(0.88691),(0.88066),(0.87422),(0.8676),(0.86079),(0.8538),(0.84662),(0.83926),(0.83172),(0.82399),(0.81608),(0.80799),(0.79971),(0.79125),(0.7826),(0.77377),(0.76476),(0.75556),(0.74617),(0.73661),(0.72686),(0.71692),(0.7068),(0.6965),(0.68602),(0.67535),(0.66449),(0.65345),(0.64223),(0.63082),(0.61923),(0.60746),(0.5955),(0.58336),(0.57103),(0.55852),(0.54583),(0.53295),(0.51989),(0.50664),(0.49321),(0.4796))
B<-c((0.07176),(0.08339),(0.09498),(0.10652),(0.11802),(0.12947),(0.14087),(0.15223),(0.16354),(0.17481),(0.18603),(0.1972),(0.20833),(0.21941),(0.23044),(0.24143),(0.25237),(0.26327),(0.27412),(0.28492),(0.29568),(0.30639),(0.31706),(0.32768),(0.33825),(0.34878),(0.35926),(0.3697),(0.38008),(0.39043),(0.40072),(0.41097),(0.42118),(0.43134),(0.44145),(0.45152),(0.46153),(0.47151),(0.48144),(0.49132),(0.50115),(0.51094),(0.52069),(0.5304),(0.54015),(0.54995),(0.55979),(0.56967),(0.57958),(0.5895),(0.59943),(0.60937),(0.61931),(0.62923),(0.63913),(0.64901),(0.65886),(0.66866),(0.67842),(0.68812),(0.69775),(0.70732),(0.7168),(0.7262),(0.73551),(0.74472),(0.75381),(0.76279),(0.77165),(0.78037),(0.78896),(0.7974),(0.80569),(0.81381),(0.82177),(0.82955),(0.83714),(0.84455),(0.85175),(0.85875),(0.86554),(0.87211),(0.87844),(0.88454),(0.8904),(0.896),(0.90142),(0.90673),(0.91193),(0.91701),(0.92197),(0.9268),(0.93151),(0.93609),(0.94053),(0.94484),(0.94901),(0.95304),(0.95692),(0.96065),(0.96423),(0.96765),(0.97092),(0.97403),(0.97697),(0.97974),(0.98234),(0.98477),(0.98702),(0.98909),(0.99098),(0.99268),(0.99419),(0.99551),(0.99663),(0.99755),(0.99828),(0.99879),(0.9991),(0.99919),(0.99907),(0.99873),(0.99817),(0.99739),(0.99638),(0.99514),(0.99366),(0.99195),(0.98999),(0.98775),(0.98524),(0.98246),(0.97941),(0.9761),(0.97255),(0.96875),(0.9647),(0.96043),(0.95593),(0.95121),(0.94627),(0.94113),(0.93579),(0.93025),(0.92452),(0.91861),(0.91253),(0.90627),(0.89986),(0.89328),(0.88655),(0.87968),(0.87267),(0.86553),(0.85826),(0.85087),(0.84337),(0.83576),(0.82806),(0.82025),(0.81236),(0.80439),(0.79634),(0.78823),(0.78005),(0.77181),(0.76352),(0.75519),(0.74682),(0.73842),(0.73),(0.7214),(0.7125),(0.7033),(0.69382),(0.68408),(0.67408),(0.66386),(0.65341),(0.64277),(0.63193),(0.62093),(0.60977),(0.59846),(0.58703),(0.57549),(0.56386),(0.55214),(0.54036),(0.52854),(0.51667),(0.50479),(0.49291),(0.48104),(0.4692),(0.4574),(0.44565),(0.43399),(0.42241),(0.41093),(0.39958),(0.38836),(0.37729),(0.36638),(0.35566),(0.34513),(0.33482),(0.32473),(0.31489),(0.3053),(0.29599),(0.28696),(0.27824),(0.26981),(0.26152),(0.25334),(0.24526),(0.2373),(0.22945),(0.2217),(0.21407),(0.20654),(0.19912),(0.19182),(0.18462),(0.17753),(0.17055),(0.16368),(0.15693),(0.15028),(0.14374),(0.13731),(0.13098),(0.12477),(0.11867),(0.11268),(0.1068),(0.10102),(0.09536),(0.0898),(0.08436),(0.07902),(0.0738),(0.06868),(0.06367),(0.05878),(0.05399),(0.04931),(0.04474),(0.04028),(0.03593),(0.03169),(0.02756),(0.02354),(0.01963),(0.01583))
C<-c((0.23217),(0.26149),(0.29024),(0.31844),(0.34607),(0.37314),(0.39964),(0.42558),(0.45096),(0.47578),(0.50004),(0.52373),(0.54686),(0.56942),(0.59142),(0.61286),(0.63374),(0.65406),(0.67381),(0.693),(0.71162),(0.72968),(0.74718),(0.76412),(0.7805),(0.79631),(0.81156),(0.82624),(0.84037),(0.85393),(0.86692),(0.87936),(0.89123),(0.90254),(0.91328),(0.92347),(0.93309),(0.94214),(0.95064),(0.95857),(0.96594),(0.97275),(0.97899),(0.98461),(0.9893),(0.99303),(0.99583),(0.99773),(0.99876),(0.99896),(0.99835),(0.99697),(0.99485),(0.99202),(0.98851),(0.98436),(0.97959),(0.97423),(0.96833),(0.9619),(0.95498),(0.94761),(0.93981),(0.93161),(0.92305),(0.91416),(0.90496),(0.8955),(0.8858),(0.8759),(0.86581),(0.85559),(0.84525),(0.83484),(0.82437),(0.81389),(0.80342),(0.79299),(0.78264),(0.7724),(0.7623),(0.75237),(0.74265),(0.73316),(0.72393),(0.715),(0.70599),(0.69651),(0.6866),(0.67627),(0.66556),(0.65448),(0.64308),(0.63137),(0.61938),(0.60713),(0.59466),(0.58199),(0.56914),(0.55614),(0.54303),(0.52981),(0.51653),(0.50321),(0.48987),(0.47654),(0.46325),(0.45002),(0.43688),(0.42386),(0.41098),(0.39826),(0.38575),(0.37345),(0.3614),(0.34963),(0.33816),(0.32701),(0.31622),(0.30581),(0.29581),(0.28623),(0.27712),(0.26849),(0.26038),(0.2528),(0.24579),(0.23937),(0.23356),(0.22835),(0.2237),(0.2196),(0.21602),(0.21294),(0.21032),(0.20815),(0.2064),(0.20504),(0.20406),(0.20343),(0.20311),(0.2031),(0.20336),(0.20386),(0.20459),(0.20552),(0.20663),(0.20788),(0.20926),(0.21074),(0.2123),(0.21391),(0.21555),(0.21719),(0.2188),(0.22038),(0.22188),(0.22328),(0.22456),(0.2257),(0.22667),(0.22744),(0.228),(0.22831),(0.22836),(0.22811),(0.22754),(0.22663),(0.22536),(0.22369),(0.22161),(0.21918),(0.2165),(0.21358),(0.21043),(0.20706),(0.20348),(0.19971),(0.19577),(0.19165),(0.18738),(0.18297),(0.17842),(0.17376),(0.16899),(0.16412),(0.15918),(0.15417),(0.1491),(0.14398),(0.13883),(0.13367),(0.12849),(0.12332),(0.11817),(0.11305),(0.10797),(0.10294),(0.09798),(0.0931),(0.08831),(0.08362),(0.07905),(0.07461),(0.07031),(0.06616),(0.06218),(0.05837),(0.05475),(0.05134),(0.04814),(0.04516),(0.04243),(0.03993),(0.03753),(0.03521),(0.03297),(0.03082),(0.02875),(0.02677),(0.02487),(0.02305),(0.02131),(0.01966),(0.01809),(0.0166),(0.0152),(0.01387),(0.01264),(0.01148),(0.01041),(0.00942),(0.00851),(0.00769),(0.00695),(0.00629),(0.00571),(0.00522),(0.00481),(0.00449),(0.00424),(0.00408),(0.00401),(0.00401),(0.0041),(0.00427),(0.00453),(0.00486),(0.00529),(0.00579),(0.00638),(0.00705),(0.0078),(0.00863),(0.00955),(0.01055))

turbo_colormap_data<-cbind(A,B,C) 
turbo_colormap_data_sRGB<-sRGB(turbo_colormap_data)
turbo_colormap_data_HEX = hex(turbo_colormap_data_sRGB)

Turbo <- function(pal.min = 0,pal.max = 1,out.colors = NULL,pal = turbo_colormap_data_HEX,reverse = F) {
  # pal.min = lower bound of the palette to use [0,1]
  # pal.max = upper bound of the palette [0,1]
  # out.colors = specify the number of colors to return if out.colors = 1, will return pal.min color. if unspecified, will return all the colors in the original palette that fall within the min and max boundaries
  # pal = vector of colors (HEX) in palette
  # reverse = flip palette T/F - performed as last step
  
  if(pal.min == 0){pal.start = 1}
  if(pal.min > 0){pal.start = round(length(pal)*pal.min) }
  pal.end = round(length(pal)*pal.max )
  out = pal[pal.start:pal.end]
  
  if(!is.null(out.colors)){
    pal2 = colorRampPalette(colors = out ,space="rgb", interpolate = "linear")
    out = pal2(out.colors)
  }
  if(reverse == T){out = rev(out)}
  return(out)
}

#read in data ----
dave_dat <- read_xlsx("data/raw/NIAPS based flow reductions for all invasions in all qcats.xlsx", sheet = 1) %>%
  rename('QUATERNARY'='Qcat')
catch_sp <-  st_read('data/temp/catch_ext.shp')
catch_j <- read_feather('data/output/catch.feather')

catchments <- read_feather('data/output/catchments.feather')
index <- raster('data/temp/template_temp.tif')
crs_index <- crs(index)
indexdf <- read_feather('data/output/index.feather')
sp_data <- read_feather('data/output/sp_data_r1_short.feather')

#create rasters from raw output ----

#natural
output_ras(index,'rn',uncer,crs_index,sp_data)
#reduction
output_ras(index,'rr',uncer,crs_index,sp_data)
#percent
output_ras(index,'pc',uncer, crs_index,sp_data)
#mad
output_ras(index,'mad',uncer, crs_index,sp_data)

#niaps mask ----
niaps <- raster("data/output/niaps_sp.tif")

# total summaries ----
total <- read_feather(paste0('data/results/catch_summmary_',uncer,'.feather')) %>%
  mutate(actual = RUNSUM-REDSUM,percent_r = (REDSUM/RUNSUM)*100) %>%
  rename(natural=RUNSUM,reduction=REDSUM) %>%
  group_by(SAMP) %>%
  summarise(nat=sum(natural,na.rm=T),red=sum(reduction,na.rm=T)) %>%
  mutate(per = red/nat*100,nat=nat/1000000,red=red/1000000) %>%
  dplyr::select(-SAMP) %>%
  group_by() %>%
  mean_qi()

#us: 312 Gl [274,413]
#dlm 365 Gl

#us 4.25% [3.72,5.62]
#dlm 6.99%
#catchment summaries ----

tbl <- read_feather(paste0('data/results/catch_summmary_',uncer,'.feather')) %>%
  dplyr::select(-SAMP) %>% 
  mutate(actual = RUNSUM-REDSUM,percent_r = (REDSUM/RUNSUM)*100) %>%
  rename(natural=RUNSUM,reduction=REDSUM)

tbl_all <- read_feather(paste0('data/results/catch_summmary_',uncer,'.feather')) %>%
  mutate(actual = RUNSUM-REDSUM,percent_r = (REDSUM/RUNSUM)*100) %>%
  rename(natural=RUNSUM,reduction=REDSUM)

c_summary <- tbl %>% 
  group_by(QNUM) %>%
  mean_qi()

c_all <- c_summary %>%
  left_join(catch_j, by ='QNUM') %>%
  left_join(catch_sp, by = 'QUATERNARY') %>%
  left_join(dave_dat, by = 'QUATERNARY')

c_summary_sp <- catch_sp %>%
  left_join(catch_j) %>% 
  left_join(c_summary) %>%
  left_join(dave_dat)

c_plot <- c_all %>%
  filter(.width==0.95)

c_j<-c_all %>%
  dplyr::select(c('QUATERNARY','QNUM'))

c_final_number <- tbl_all %>% 
  group_by(SAMP) %>%
  summarise(tred = sum(reduction),tnat=sum(natural)) %>%
  mutate(tred = tred/1000000,tnat = tnat/1000000,tper = (tred/tnat)*100) %>%
  summarise(lower_r = quantile(tred,c(0.025)),
            upper_r = quantile(tred,c(0.975)),
            mean_r = mean(tred),
            upper_p = quantile(tper,c(0.025)),
            lower_p = quantile(tper,c(0.975)),
            mean_p = mean(tper))
#catchment summaries no variation ----

tbl1 <- read_feather(paste0('data/results/catch_summmary_1.feather')) %>%
  dplyr::select(-SAMP) %>% 
  mutate(actual = RUNSUM-REDSUM,percent_r = (REDSUM/RUNSUM)*100) %>%
  rename(natural=RUNSUM,reduction=REDSUM)

tbl_all1 <- read_feather(paste0('data/results/catch_summmary_1.feather')) %>%
  mutate(actual = RUNSUM-REDSUM,percent_r = (REDSUM/RUNSUM)*100) %>%
  rename(natural=RUNSUM,reduction=REDSUM)

c_summary1 <- tbl1 %>% 
  group_by(QNUM) %>%
  mean_qi()

c_all1 <- c_summary1 %>%
  left_join(catch_j, by ='QNUM') %>%
  left_join(catch_sp, by = 'QUATERNARY') %>%
  left_join(dave_dat, by = 'QUATERNARY')

c_summary_sp1 <- catch_sp %>%
  left_join(catch_j) %>% 
  left_join(c_summary1) %>%
  left_join(dave_dat)

c_plot1 <- c_all1 %>%
  filter(.width==0.95)

c_j1<-c_all1 %>%
  dplyr::select(c('QUATERNARY','QNUM'))

c_final_number1 <- tbl_all1 %>% 
  group_by(SAMP) %>%
  summarise(tred = sum(reduction),tnat=sum(natural)) %>%
  mutate(tred = tred/1000000,tnat = tnat/1000000,tper = (tred/tnat)*100) %>%
  summarise(lower_r = quantile(tred,c(0.025)),
            upper_r = quantile(tred,c(0.975)),
            mean_r = mean(tred),
            upper_p = quantile(tper,c(0.025)),
            lower_p = quantile(tper,c(0.975)),
            mean_p = mean(tper))

#for analysing sources of uncertainty: calculate MAD and MEDIAN ----
c_mad <- tbl %>% 
  group_by(QNUM) %>%
  summarise(mad = mad(reduction),med = median(reduction)) %>%
  mutate(mcv = mad/med, scen = uncer)
write_feather(c_mad,paste0('data/results/uncer_',uncer,'.feather'))

#the big six data prep ----
qcar<-c('G10A','G10B','H60B','H60A','H60C','G40A','G10J', 'G10F','G10E')
qdam<-c('Wemmershoek','Berg River','Theewaterskloof A','Theewaterskloof B','Theewaterskloof C','Steenbras','Voelvlei A','Voelvlei B','Voelvlei C')
qnames<- tibble(QUATERNARY = qcar,qname = qdam)

tbl2 <- tbl %>%
  left_join(c_j,by='QNUM') %>% 
  left_join(qnames, by = 'QUATERNARY') %>%
  mutate(QNUM = as.factor(QNUM), qname = as.factor(qname)) 

tbl2_dams <- tbl2 %>%
  filter(QUATERNARY %in% c('G10A','G10B','H60B','H60A','H60C','G40A','G10J', 'G10F','G10E'))

tbl2_all <- tbl_all %>%
  left_join(c_j,by='QNUM') %>% 
  left_join(qnames, by = 'QUATERNARY') %>%
  mutate(QNUM = as.factor(QNUM), qname = as.factor(qname)) 

tbl2_dams_all <- tbl2_all %>%
  filter(QUATERNARY %in% c('G10A','G10B','H60B','H60A','H60C','G40A','G10J', 'G10F','G10E')) %>%
  dplyr::select(SAMP,reduction,natural) %>%
  group_by(SAMP) %>%
  summarise(total_loss=sum(reduction),total_nat=sum(natural)) %>%
  mutate(total_loss=total_loss/(1000000),total_nat=total_nat/(1000000),per = total_loss/total_nat*100)

hist(tbl2_dams_all$total_loss)
mean(tbl2_dams_all$total_loss)
qi(tbl2_dams_all$total_loss)

hist(tbl2_dams_all$per)
mean(tbl2_dams_all$per)
qi(tbl2_dams_all$per)

#ridgelines for major catchments ----
ggplot(tbl2_dams, aes(x = percent_r, y = qname, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 5,bandwidth=0.4, rel_min_height = 0.005) +
  scale_fill_viridis(name = "percent", option = "C",direction = 1) +
  labs(title = '') +
  xlab('percent reduction') +
  ylab('catchment') +
  theme_bw()

ggsave(paste0('data/results/ridgelines_',uncer,'.png'),width=6,height=5,scale=1)

#ridgelines for major catchments litres ----
ggplot(tbl2_dams, aes(x = reduction/(1000*1000), y = qname, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 5,bandwidth=200, rel_min_height = 0.0001) +
  scale_fill_viridis(name = "Ml", option = "C",direction = 1) +
  labs(title = '') +
  xlab('flow reduction in Ml') +
  ylab('catchment') +
  theme_bw()

ggsave(paste0('data/results/ridgelines_m3_',uncer,'.png'),width=6,height=5,scale=1)

#ridgelines compare ----
qnames_ridge <- qnames %>%
  left_join(c_plot, by='QUATERNARY') %>%
  mutate(dave_catch =Tot_redn_m3/1000,my_catch=reduction/(1000*1000),my_catch_upper=reduction.upper/(1000*1000)) %>%
  dplyr::select(!!c('qname','my_catch','my_catch_upper','dave_catch'))

#catchments: entire region ----
c_sum_plot <- c_summary_sp %>%
  dplyr::select(lower = reduction.lower,mean = reduction,upper = reduction.upper) %>%
  gather('level','reduction',-geometry) %>%
  mutate(reduction=(reduction)/1000000)

c_per_plot <- c_summary_sp %>%
  dplyr::select(lower = percent_r.lower,mean = percent_r,upper = percent_r.upper) %>%
  gather('level','reduction',-geometry)

c_cv_plot <- c_summary_sp %>%
  left_join(c_mad,by='QNUM') %>%
  dplyr::select(mcv = mcv) %>%
  dplyr::mutate(mcv = replace_na(mcv, 0)) %>%
  gather('level','reduction',-geometry)

c_mad_plot <- c_summary_sp %>%
  left_join(c_mad,by='QNUM') %>%
  dplyr::select(mad = mad) %>%
  dplyr::mutate(mad = replace_na(mad, 0)) %>%
  gather('level','reduction',-geometry) %>%
  dplyr::mutate(reduction = reduction/1000000)

#check that all catchments have some naisp data
#nmask <- niaps>0
#ncounbt <- extract(nmask, c_summary_sp, fun=sum, na.rm=TRUE, df=TRUE)
cc <- c_sum_plot %>%
  filter(!is.na(geometry))
options(scipen=999)
theme_set(theme_bw())
P<-ggplot(c_sum_plot) + 
  geom_sf(aes(fill = reduction), lwd = 0) +
  facet_wrap(~ level,dir='v') +
  scale_fill_gradientn(name = "runoff reduction (Ml)",colours = Turbo(),trans = 'sqrt',na.value="white") +
  #scale_fill_viridis(name = "runoff reduction (Ml)", option = "inferno",trans = 'sqrt',direction = 1,na.value="white") +
  coord_sf() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave(paste0('data/results/catchments_ci_ff_',uncer,'.png'),plot=P,width=4,height=5,scale=1.5)

P<-ggplot(c_per_plot) + 
  geom_sf(aes(fill = reduction), lwd = 0) +
  facet_wrap(~ level,dir='v') +
  scale_fill_gradientn(name = "runoff reduction (%)",colours = Turbo(),na.value="white") +
  #scale_fill_viridis(name = "runoff reduction (Ml)", option = "inferno",trans = 'sqrt',direction = 1,na.value="white") +
  coord_sf() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave(paste0('data/results/catchments_ci_per_ff_',uncer,'.png'),plot=P,width=4,height=5,scale=1.5)

P1<-ggplot(c_cv_plot) + 
  geom_sf(aes(fill = reduction), lwd = 0) +
  scale_fill_gradientn(name = "Coefficient of variation",colours = Turbo(),na.value="white") +
  #scale_fill_viridis(name = "runoff reduction (Ml)", option = "inferno",trans = 'sqrt',direction = 1,na.value="white") +
  coord_sf() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave(paste0('data/results/catchments_ci_cv_ff_',uncer,'.png'),plot=P,width=5,height=3,scale=2)

P2<-ggplot(c_mad_plot) + 
  geom_sf(aes(fill = reduction), lwd = 0) +
  scale_fill_gradientn(name = "Median absolute deviation (Ml)",colours = Turbo(),trans = 'sqrt',na.value="white") +
  #scale_fill_viridis(name = "runoff reduction (Ml)", option = "inferno",trans = 'sqrt',direction = 1,na.value="white") +
  coord_sf() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave(paste0('data/results/catchments_ci_mad_ff_',uncer,'.png'),plot=P,width=5,height=3,scale=2)

Ps<-ggarrange(P1, P2,legend = "top")
ggsave(paste0('data/results/catchments_both_ff_',uncer,'.png'),plot=Ps,width=5,height=2,scale=2.5)


#pixel plots data prep ----
Mean<-raster(paste0('data/spatial/rr',uncer,'_mean.tif'))
Upper<-raster(paste0('data/spatial/rr',uncer,'_u95.tif'))
Lower<-raster(paste0('data/spatial/rr',uncer,'_l05.tif'))
crsr<-'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
Mean<-projectRaster(Mean,crs=crsr)
Upper<-projectRaster(Upper,crs=crsr)
Lower<-projectRaster(Lower,crs=crsr)
Reduction<-stack(Lower,Mean,Upper)
names(Reduction) <- c('Lower','Mean','Upper')
Reduction2<-((((Reduction+1)/62500)*1000000)/1000)

Mean2 <- Mean
Mean2[Mean2>10000000] <- 10000000
Upper2 <- Upper
Upper2[Upper2>10000000] <- 10000000
Lower2 <- Lower
Lower2[Lower2>10000000] <- 10000000
Reduction3<-stack(Lower2,Mean2,Upper2)
names(Reduction3) <- c('Lower','Mean','Upper')
Reduction4<-((((Reduction3+1)/62500)*1000000)/1000)

#percent
Mean<-raster(paste0('data/spatial/pc',uncer,'_mean.tif'))
Upper<-raster(paste0('data/spatial/pc',uncer,'_u95.tif'))
Lower<-raster(paste0('data/spatial/pc',uncer,'_l05.tif'))
crsr<-'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
Mean<-projectRaster(Mean,crs=crsr)
Upper<-projectRaster(Upper,crs=crsr)
Lower<-projectRaster(Lower,crs=crsr)
Reduction<-stack(Lower,Mean,Upper)
names(Reduction) <- c('Lower','Mean','Upper')

Reduction4p<-clamp(Reduction, lower=0, upper=1, useValues=TRUE)
Reduction4p<-Reduction4p*100

#mcv
Mean<-raster(paste0('data/spatial/mad',uncer,'_mcv.tif'))
Mean[is.na(Mean[])] <- 0 
inmask <- resample(index,Mean,method='ngb')
Mean <- mask(Mean,inmask)
crsr<-'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
Mean<-projectRaster(Mean,crs=crsr)

#Pixels: entire region ----

options(scipen=999)
theme_set(theme_bw())
P<-gplot(Reduction4,maxpixels=500000) + geom_tile(aes(fill = value)) +
  facet_wrap(~ variable,dir='v') +
  #scale_fill_viridis(name = "runoff reduction (kl/km2)", option = "inferno",trans = 'sqrt',direction = 1,na.value="white") +
  scale_fill_gradientn(name = "runoff reduction (kl/km2)",colours = Turbo(),trans = 'sqrt',na.value="white") +
  coord_equal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(paste0('data/results/pixelci_ff_',uncer,'.png'),plot=P,width=4,height=5,scale=1.5)

P<-gplot(Reduction4p,maxpixels=500000) + geom_tile(aes(fill = value)) +
  facet_wrap(~ variable,dir='v') +
  #scale_fill_viridis(name = "runoff reduction (kl/km2)", option = "inferno",trans = 'sqrt',direction = 1,na.value="white") +
  scale_fill_gradientn(name = "runoff reduction (%)",trans = 'sqrt',colours = Turbo(),na.value="white") +
  coord_equal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(paste0('data/results/pixelci_p_ff_',uncer,'.png'),plot=P,width=4,height=5,scale=1.5)

P<-gplot(Mean,maxpixels=500000) + geom_tile(aes(fill = value)) +
  #scale_fill_viridis(name = "runoff reduction (kl/km2)", option = "inferno",trans = 'sqrt',direction = 1,na.value="white") +
  scale_fill_gradientn(name = "coefficient of variation",trans = 'sqrt',colours = Turbo(),na.value="white") +
  coord_equal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(paste0('data/results/pixel_mad_ff_',uncer,'.png'),plot=P,width=5,height=3,scale=1.5)

#Pixels: cape fold zoom ----
dam<-read_sf('data/raw/dams500g')

dam_ras<-fasterize::fasterize(dam,Mean2)
r5 <- mask(Reduction4,dam_ras,inverse=TRUE)

P<-gplot(r5,maxpixels=50000) + 
  geom_tile(aes(fill = value)) +
  facet_wrap(~ variable,dir='h') +
  scale_fill_viridis(name = "runoff reduction (kl/km2)", option = "inferno",trans = 'sqrt',direction = 1,na.value="white") +
  coord_equal(xlim=c(18.5,19.5),ylim=c(-34.5,-33.5)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


ggsave(paste0('data/results/pixelci_ff_zoo_leg',uncer,'.png'),plot=P,width=20,height=10,scale=1)

#compare to DLM  2016 percent reduction ----
ggplot(data = c_plot, aes(y=percent_r, x=Tot_redn_pct, ymin=percent_r.lower, ymax=percent_r.upper)) +
  geom_point(alpha=0.3) +
  geom_abline(slope=1, intercept=0) +
  geom_linerange(alpha=0.5) +
  labs(title ='') +
  ylim(c(0,41)) +
  xlab('Le Maitre et al 2016') +
  xlim(c(0,41)) +
  ylab('This study') +
  theme_bw()

ggsave(paste0('data/results/compareper_',uncer,'.png'),scale=1.2,width=4,height=4)   

#compare to DLM  2016 absolute reduction ----
ccplot <- c_plot %>%
  dplyr::select(reduction,reduction.lower,reduction.upper,Tot_redn_m3) %>%
  mutate(ym=(reduction/(1000*1000))/1000,yl=(reduction.lower/(1000*1000))/1000,yu=(reduction.upper/(1000*1000))/1000,dlm=Tot_redn_m3/(1000*1000))

#summary(lm(ccplot$ym ~ ccplot$dlm))
ggplot(data = ccplot, aes(y=ym, x=dlm, ymin=yl, ymax=yu)) +
  geom_point(alpha=0.3) +
  geom_abline(slope=1, intercept=0) +
  geom_linerange(alpha=0.5) +
  geom_smooth(method='lm',se=F,color='black',lwd=0.5,linetype=2) +
  labs(title ='') +
  xlab('le Maitre et al 2016 (million m3)') +
  ylab('This study (million m3)') +
  coord_cartesian(ylim = c(0,12),xlim=c(0,12)) +
  theme_bw()

ggsave(paste0('data/results/compareabs_ff_lin_',uncer,'.png'),height=4,width=4,scale=1.2)                  
ggsave(paste0('data/results/compareabs_ff_lin_',uncer,'.eps'),device=cairo_ps, fallback_resolution = 300,height=4,width=4,scale=1.2)                  

#calc r2
r2 <- c_plot %>%
  dplyr::select(m=reduction, d=Tot_redn_m3) %>%
  mutate(m=m/(1000*1000),d=d/1000)
cor(r2$m,r2$d)
#compare to just using mean ----
ccplot <- c_plot %>%
  dplyr::select(reduction,reduction.lower,reduction.upper,Tot_redn_m3) %>%
  mutate(ym=(reduction/(1000*1000))/1000,yl=(reduction.lower/(1000*1000))/1000,yu=(reduction.upper/(1000*1000))/1000,dlm=Tot_redn_m3/(1000*1000))
ccplot1 <- c_plot1 %>%
  dplyr::select(reduction,reduction.lower,reduction.upper,Tot_redn_m3) %>%
  mutate(ym=(reduction/(1000*1000))/1000,yl=(reduction.lower/(1000*1000))/1000,yu=(reduction.upper/(1000*1000))/1000,dlm=Tot_redn_m3/(1000*1000))
ccplot$mean_red_abs <- ccplot1$ym
#summary(lm(ccplot$ym ~ ccplot$dlm))
ggplot(data = ccplot, aes(y=ym, x=mean_red_abs, ymin=yl, ymax=yu)) +
  geom_point(alpha=0.3) +
  geom_abline(slope=1, intercept=0) +
  geom_linerange(alpha=0.5) +
  geom_smooth(method='lm',se=F,color='black',lwd=0.5,linetype=2) +
  labs(title ='') +
  xlab('without uncertainty (million m3)') +
  ylab('with uncertatinty (million m3)') +
  coord_cartesian(ylim = c(0,12),xlim=c(0,12)) +
  theme_bw()

ggsave(paste0('data/results/compareabs_jenson.png'),height=4,width=4,scale=1.2)                  
ggsave(paste0('data/results/compareabs_jenson.eps'),device=cairo_ps, fallback_resolution = 300,height=4,width=4,scale=1.2)                  

#calc r2
summary(lm(ccplot$ym~ccplot$mean_red_abs))

#create plot comparing uncer scenarios ----

runs <- c(0,2,3,4,6,7,8)
for (i in runs) {
  uncer = i
  tbl <- read_feather(paste0('data/results/catch_summmary_',uncer,'.feather')) %>%
    dplyr::select(-SAMP) %>% 
    mutate(actual = RUNSUM-REDSUM,percent_r = (REDSUM/RUNSUM)*100) %>%
    rename(natural=RUNSUM,reduction=REDSUM)
  
  c_mad <- tbl %>% 
    group_by(QNUM) %>%
    summarise(mad = mad(reduction),med = median(reduction)) %>%
    mutate(mcv = mad/med, scen = uncer)
  write_feather(c_mad,paste0('data/results/scen_uncer_',uncer,'.feather'))
}

names<-data.frame(scen=runs,name=c("All","Additional water","Rainfall","Curve shape","Invasive density","Curve assignment","Age"))
files <- dir(path = 'data/results/',pattern = "scen_uncer_*",full.names=TRUE)
all_data<-data <- files %>%
  map(read_feather) %>%
  reduce(rbind) %>%
  filter(scen %in% runs) %>%
  left_join(names,by='scen')

data <- files %>%
  map(read_feather) %>%
  reduce(rbind) %>%
  filter(scen %in% runs) %>%
  left_join(names,by='scen') %>%
  dplyr::select(one_of(c('mad','name'))) %>%
  mutate(mad=mad^2) %>%
  group_by(name) %>%
  summarise(total_mad = sum(mad,na.rm=T)) %>%
  mutate(total_mad = sqrt(total_mad)/max(sqrt(total_mad))*100)

ggdotchart(data, x = "name", y = "total_mad",
           xlab= "",ylab ="Percent of uncertainty",
           color = "skyblue",                                # Color by groups
           sorting = "descending",                       # Sort value in descending order
           add = "segments",                             # Add segments from y = 0 to dots
           rotate = TRUE,                                # Rotate vertically
           dot.size = 10,                                 # Large dot size
           label = round(data$total_mad),                        # Add mpg values as dot labels
           font.label = list(color = "black", size = 9, 
                             vjust = 0.5),               # Adjust label parameters
           ggtheme = theme_pubr()                        # ggplot2 theme
)


ggsave('data/results/compare_uncer.png',height=5,width=6.5,scale=1)         

#compare catchements undertainty source ----
runs <- c(2,4,6,0)
#reshape and join to spatial and percent data (c_per_plot)
catchdat <- all_data %>%
  dplyr::filter(scen %in% runs) %>%
  dplyr::select(QNUM,scen,mad) %>%
  tidyr::spread(key=scen,value=mad)
catchdat <- c_summary_sp %>%
  left_join(catchdat,by='QNUM') %>%
  dplyr::select(`0`,`6`,`2`,`4`,QNUM)

scale <- cbind(catchdat$`2`,catchdat$`4`,catchdat$`6`)
scale <- apply(scale, 1, max,na.rm=T)
abs <- max(catchdat$`0`)
catchdat$water <- catchdat$`2`/scale
catchdat$curve <- catchdat$`4`/scale
catchdat$niaps <- catchdat$`6`/scale
catchdat$alls <-(catchdat$`0`^0.33)/(abs^0.33)
catchdat$alls[catchdat$alls<0]<-0
catchdat$alls[is.na(catchdat$alls)]<-0

tric <- Tricolore(catchdat, p1 ="water", p2 = "niaps", p3 = "curve", 
                  chroma = 0.95,
                  lightness = 0.8,
                  contrast = 0.7,
                  show_data = F,
                  breaks=2)
rgbs <- col2rgb(tric$rgb)
rgba <-rgb(rgbs[1,]/255,rgbs[2,]/255,rgbs[3,]/255,catchdat$alls)
catchdat$rgba <- rgba
#plot
P <-ggplot(catchdat) +
  geom_sf(aes(fill = rgba, geometry = geometry), size = 0.1) +
  scale_fill_identity() +
  annotation_custom(
    ggplotGrob(tric$key +
                 theme(plot.background = element_rect(fill = NA, color = NA)) +
                 labs(L = 'Water', T = 'Invasive density', R = 'Curve')),
    xmin = -350000, xmax = -100000, ymin = -3950000, ymax = -3000000) + 
  theme_void() +
  coord_sf(datum = NA) +
  theme(legend.position = "None")

P
ggsave('data/results/catch_top3_uncer.png',height=5,width=6.5,scale=2)         
#compare pixels undertainty total ----
pix_file <- list.files('data/spatial/','*_mad',full.names=T)
pstack <- stack(pix_file)#2 = water
names(pstack) <- c("All","Additional water","Rainfall","Curve shape","Invasive density","Curve assignment","Age")
pstack <- pstack^2
psum <- cellStats(pstack,stat='sum',na.rm=T)
psum <- as.data.frame(psum)
psum$name <- rownames(psum)
psum$psum <- sqrt(psum$psum)/max(sqrt(psum$psum))
psum$psum <- psum$psum*100


ggdotchart(psum, x = "name", y = "psum",
           xlab= "",ylab ="Percent of uncertainty",
           color = "skyblue",                                # Color by groups
           sorting = "descending",                       # Sort value in descending order
           add = "segments",                             # Add segments from y = 0 to dots
           rotate = TRUE,                                # Rotate vertically
           dot.size = 10,                                 # Large dot size
           label = round(psum$psum),                        # Add mpg values as dot labels
           font.label = list(color = "black", size = 9, 
                             vjust = 0.5),               # Adjust label parameters
           ggtheme = theme_pubr()                        # ggplot2 theme
)


ggsave('data/results/compare_uncer_pix.png',height=5,width=6.5,scale=1)         

# tables ----
#stan
library(rstan)
library(loo)
load("data/output/stanfits.Rdata")

#evaluate models
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


comp <- as.data.frame(loo_compare(loo_1, loo_2,loo_3,loo_4))
comp$mname <- row.names(comp)
comp <- comp[order(comp$mname),]
modelnames <- c("Age + Condition + Species + Age|Condition",
                "Age + Condition + Species",
                "Age + Condition + Species + Age|Species",
                "Age + Condition + Species + Age|Condition + Age|Species")
comp$Name <- modelnames
comp <- comp[order(comp$looic),]
comp$LOOIC <- comp$looic
comp <- comp %>%
  dplyr::select(Name,LOOIC)

print(xtable(comp), include.rownames=FALSE)

##curves
names <- c("Acacia cyclops",
           "Arundo donax",
           "Acacia melanoxylon",
           "Atriplex nummularia subsp. nummularia",
           "Acacia saligna",
           "Eucalyptus spp",
           "Hakea spp",
           "Pinus spp",
           "Populus spp",
           "Salix babylonica",
           "Tamarix chinensis",
           "Acacia spp")
sp <-  c("ACYCLO","ADONAX","AMELAN","ANUMMU","ASALIG","EUCALS","HAKEAS","PINUSS","POPULS","SBABYL","TCHINE","WATTLS")
names <- as.data.frame(names)
names$spp <- sp

curves <- read_csv("data/raw/curves.csv",col_types = cols(X1 = col_skip(), X1_1 = col_skip(),mod = col_skip()))
curve <- curves %>% inner_join(names,by='spp') %>%
  dplyr::select(Name = names, Condition = spp_opt,Species = spp_curve)

print(xtable(curve), include.rownames=FALSE)
