library(bigrquery)
library(googleCloudStorageR)
library(DBI)
library(dplyr)
library(feather)

#fix issue when downloading data
options(scipen = 20)
#uncer <- commandArgs()[1]
uncer <- 0

resfile <- paste0('data/results/','stream_big_',uncer,'.csv')

#create a service account in a google cloud project under IAM
#Service account must have BigQuery Admin and Storage Admin permission
#create jsonkey
#save this key in your project home folder

#setup storage
bucket <- 'stream-res'
auth_json <- paste0("data/path.json")
gcs_auth(auth_json)
gcs_global_bucket(bucket)

#upload results to GCS
upload_try <- gcs_upload(resfile,name=paste0('stream_big_',uncer,'.csv'))
gcs_update_object_acl(paste0('stream_big_',uncer,'.csv'), entity_type = "allUsers")
res_uri <- paste0('gs://',bucket,'/','stream_big_',uncer,'.csv')

#if that fails:
#run this output in the terminal:
#paste0('sudo gsutil cp -r ', resfile, ' gs://stream-res/')
#paste0('sudo bq mk streamflow')
#paste0('sudo bq --location=US load --source_format=CSV streamflow.allpixel_',uncer,' gs://stream-res/',resfile,' PID:FLOAT,SAMP:FLOAT,RUNOFF:FLOAT,RED:FLOAT')

#load data for joining
load('data/temp/join_data.RData')
sp_data_join <- sp_data_join %>%
  mutate(PID2 = PID) %>%
  dplyr::select(c('PID2','QNUM'))
#fix div 0 errors
cmar$cmar<-pmax(cmar$cmar,0.0001)

#send big table to bigquery
bq_auth(path=auth_json)

sdataset <- bq_dataset('streamflow','streamflow')
if(bq_dataset_exists(sdataset)) {
  bq_dataset_delete(sdataset)
} else {
  bq_dataset_create(sdataset)
}


fields <- as_bq_fields(list(
  bq_field("PID", "FLOAT"),
  bq_field("SAMP", "FLOAT"),
  bq_field("RUNOFF", "FLOAT"),
  bq_field("RED", "FLOAT")
))

bq_stream <- bq_table('streamflow','streamflow','stream')

bq_streamtab <- bq_table_create(
  bq_stream,
  fields = fields
)

bq_table_load(bq_streamtab, source_uris = res_uri,source_format='CSV',fields = fields)

#connect to bq
con <- DBI::dbConnect(
  bigrquery::bigquery(),
  project = "streamflow",
  dataset = "streamflow",
  billing = "streamflow"
)

dbListTables(con)

#send join data to bq - only run this once
bqtab  <-  bq_table('streamflow', 'streamflow', 'spdata')
bq_table_upload(bqtab, sp_data_join, fields=as_bq_fields(sp_data_join))
bqtab  <-  bq_table('streamflow', 'streamflow', 'cmar')
bq_table_upload(bqtab, cmar,fields=as_bq_fields(cmar))


stream <- tbl(con, "stream")
spdat <- tbl(con, "spdata")
cmars <- tbl(con, "cmar")

#caculate runoff bias
temp<- stream %>% 
  mutate(PID2 = sql('CAST(PID AS INT64)'), SAMP2 = sql('CAST(SAMP AS INT64)')) %>%
  left_join(spdat,by='PID2') %>%
  mutate(RM = RUNOFF*62500) %>%
  group_by(QNUM) %>%
  summarise(csums = (sum(RM)/1000)/1000000000) %>%
  compute()
temp<-temp[2][[1]][1][[1]][1]
temp <- tbl(con, temp)

#correct runoff bias
pixels <- stream %>% 
  mutate(PID2 = sql('CAST(PID AS INT64)'), SAMP2 = sql('CAST(SAMP AS INT64)')) %>%
  left_join(spdat,by='PID2') %>%
  left_join(temp,by='QNUM') %>%
  left_join(cmars,by='QNUM') %>%
  mutate(crat = cmar/csums) %>%
  mutate(RU2 = RUNOFF*crat*62500, RE2 = RED*crat*62500) %>%
  mutate(PC = RE2/RU2) %>%
  group_by(PID2) %>%
  summarise(lower_ru=sql('APPROX_QUANTILES(RU2, 100)[OFFSET(5)]'),
            upper_ru=sql('APPROX_QUANTILES(RU2, 100)[OFFSET(95)]'),
            mean_ru=sql('AVG(RU2)'),
            lower_re=sql('APPROX_QUANTILES(RE2, 100)[OFFSET(5)]'),
            upper_re=sql('APPROX_QUANTILES(RE2, 100)[OFFSET(95)]'),
            mean_re=sql('AVG(RE2)'),
            lower_pc=sql('APPROX_QUANTILES(PC, 100)[OFFSET(5)]'),
            upper_pc=sql('APPROX_QUANTILES(PC, 100)[OFFSET(95)]'),
            mean_pc=sql('AVG(PC)'))  %>%
  collect()

pfile <- paste0('data/results/pixels_',uncer,'.feather')
write_feather(pixels,pfile)

#mad for pixels
mad <- stream %>% 
  mutate(PID2 = sql('CAST(PID AS INT64)'), SAMP2 = sql('CAST(SAMP AS INT64)')) %>%
  left_join(spdat,by='PID2') %>%
  left_join(temp,by='QNUM') %>%
  left_join(cmars,by='QNUM') %>%
  mutate(crat = cmar/csums) %>%
  mutate(RU2 = RUNOFF*crat*62500, RE2 = RED*crat*62500) %>%
  mutate(PC = RE2/RU2) %>%
  mutate(MED2=sql('PERCENTILE_CONT(RE2, 0.5) OVER(PARTITION BY PID2) ')) %>%
  mutate(MAD2 = abs(RE2-MED2)) %>%
  group_by(PID2) %>%
  summarise(MAD3=sql('APPROX_QUANTILES(MAD2, 100)[OFFSET(50)]'),MED3=sql('APPROX_QUANTILES(RE2, 100)[OFFSET(50)]')) %>%
  mutate(MAD4 = MAD3*1.4826) %>%
  dplyr::select(MAD4,MED3,PID2) %>%
  collect()

pfile <- paste0('data/results/mad_',uncer,'.feather')
write_feather(mad,pfile)

#catchment sums
catchments <- stream %>% 
  mutate(PID2 = sql('CAST(PID AS INT64)'), SAMP2 = sql('CAST(SAMP AS INT64)')) %>%
  left_join(spdat,by='PID2') %>%
  left_join(temp,by='QNUM') %>%
  left_join(cmars,by='QNUM') %>%
  mutate(crat = cmar/csums) %>%
  mutate(RU2 = RUNOFF*crat*62500, RE2 = RED*crat*62500) %>%
  mutate(PC = RE2/RU2) %>%
  group_by(QNUM,SAMP) %>%
  summarise(RUNSUM=sum(RU2),
            REDSUM=sum(RE2))  %>%
  collect()

cfile <- paste0('data/results/catch_summmary_',uncer,'.feather')
write_feather(catchments,cfile)

#delete dataset 
#bq_table_delete(bq_stream)
bq_dataset_delete(sdataset, delete_contents = T)
#remove file from disk
unlink(resfile)

