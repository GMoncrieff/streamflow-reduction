#Controller script to prepare data for streamflow reduction estimations
#command to run:

#step 1: prepare invasive species data
source('src/01_niaps-lean.R')

#step 2: prepare precipitation mean and sd
source('src/02_cdo-wilson.R')

#step 3: assign spp to streamflow reduction curves
source('src/03_spp-curves.R')

#step 4: create water availabilty and template layers
source('src/04_water-availability.R')

#step 5: prepare paired catchment data
source('src/05_catchment_data.R')

#step 6: wrangle data for model
source('src/06_data-prep.R')

#step 7: fit curve models
source('src/07_curve-fit.R')

#step 8: fit age models
source('src/08_age-fit.R')
  
#args to select uncertainty run
commandArgs <- function(...) 0

    #step 9: run model
    source('src/09_model_run.R')
      
    #step 10: process results
    source('src/10_process-big-df.R')
      
    #step 11: format results
    source('src/11_format-output.R')
