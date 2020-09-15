library(dplyr)
library(tidyr)

#script to read and clean data from paired catchmetn experiments 
#raw data extracted from
#Scott, D. F., Prinsloo, F. W., Moses, G., Mehlomakulu, M., & Simmers, A. D. A. (2000). 
#A re-analysis of the South African catchment afforestation experimental data. 
#Water Research Commission, Pretoria, Report, 810(1), 
#http://www.wrc.org.za/wp-content/uploads/mdocs/810-1-00.pdf

if(!file.exists("data/output/catchments.feather")){
  # Data munging
  #westfalia d ----
  data0         <- read.csv2("data/raw/catchments/west_d.csv")
  percent = 83
  data0$percent = 83
  data0$tree = 'euc'
  data0$con = 'opt'
  data0$loc = "west_d"
  
  #calc reducation at 100% cover
  data0$flow_red_per_rescale = data0$flow_red_per/100
  data0$flow_red_per_rescale = data0$flow_red_per_rescale*(100/percent)
  data0$flow_red_per_rescale = pmin(data0$flow_red_per_rescale,0.9999)
  data0$flow_red_per_rescale = pmax(data0$flow_red_per_rescale,0.0001)
  #cal raw reduction
  data0$flow_red_per_calc = data0$flow_red_per/100
  data0$flow_red_per_calc = pmin(data0$flow_red_per_calc,0.9999)
  data0$flow_red_per_calc = pmax(data0$flow_red_per_calc,0.0001)
  
  #select only years up to begin of decline
  data          <- data0[1:13,]
  data1         <- data
  
  #lambrechbos b ----
  data0         <- read.csv2("data/raw/catchments/lamb_b.csv")
  
  percent = 82
  data0$percent = 82
  data0$tree = 'pin'
  data0$con = 'sub'
  data0$loc = "lamb_b"
  
  #calc reducation at 100% cover
  data0$flow_red_per_rescale = data0$flow_red_per/100
  data0$flow_red_per_rescale = data0$flow_red_per_rescale*(100/percent)
  data0$flow_red_per_rescale = pmin(data0$flow_red_per_rescale,0.9999)
  data0$flow_red_per_rescale = pmax(data0$flow_red_per_rescale,0.0001)
  #cal raw reduction
  data0$flow_red_per_calc = data0$flow_red_per/100
  data0$flow_red_per_calc = pmin(data0$flow_red_per_calc,0.9999)
  data0$flow_red_per_calc = pmax(data0$flow_red_per_calc,0.0001)
  
  #select only years up to begin of decline
  data          <- data0[1:20,]
  data2         <-data
  
  #catchdral peak 3 ----
  data0         <- read.csv2("data/raw/catchments/cat_3.csv")
  percent = 86
  data0$percent = 86
  data0$tree = 'pin'
  data0$con = 'sub'
  data0$loc = "cat_3"
  
  #calc reducation at 100% cover
  data0$flow_red_per_rescale = data0$flow_red_per/100
  data0$flow_red_per_rescale = data0$flow_red_per_rescale*(100/percent)
  data0$flow_red_per_rescale = pmin(data0$flow_red_per_rescale,0.9999)
  data0$flow_red_per_rescale = pmax(data0$flow_red_per_rescale,0.0001)
  #cal raw reduction
  data0$flow_red_per_calc = data0$flow_red_per/100
  data0$flow_red_per_calc = pmin(data0$flow_red_per_calc,0.9999)
  data0$flow_red_per_calc = pmax(data0$flow_red_per_calc,0.0001)
  
  #select only years up to begin of decline
  data          <- data0
  data3         <-data
  
  #mokobulaan a ----
  data0         <- read.csv2("data/raw/catchments/moko_a.csv")
  percent = 97
  data0$percent = 97
  data0$tree = 'euc'
  data0$con = 'opt'
  data0$loc = "moko_a"
  data0$flow_ci = NA
  
  #calc reducation at 100% cover
  data0$flow_red_per_rescale = data0$flow_red_per/100
  data0$flow_red_per_rescale = data0$flow_red_per_rescale*(100/percent)
  data0$flow_red_per_rescale = pmin(data0$flow_red_per_rescale,0.9999)
  data0$flow_red_per_rescale = pmax(data0$flow_red_per_rescale,0.0001)
  #cal raw reduction
  data0$flow_red_per_calc = data0$flow_red_per/100
  data0$flow_red_per_calc = pmin(data0$flow_red_per_calc,0.9999)
  data0$flow_red_per_calc = pmax(data0$flow_red_per_calc,0.0001)
  #select only years up to begin of decline
  data          <- data0[1:21,]
  data4         <-data
  
  #mokobulan b ----
  data0         <- read.csv2("data/raw/catchments/moko_b.csv")
  percent = 95
  data0$percent = 95
  data0$tree = 'pin'
  data0$con = 'opt'
  data0$loc = "moko_b"
  data0$flow_ci = NA
  
  #calc reducation at 100% cover
  data0$flow_red_per_rescale = data0$flow_red_per/100
  data0$flow_red_per_rescale = data0$flow_red_per_rescale*(100/percent)
  data0$flow_red_per_rescale = pmin(data0$flow_red_per_rescale,0.9999)
  data0$flow_red_per_rescale = pmax(data0$flow_red_per_rescale,0.0001)
  #cal raw reduction
  data0$flow_red_per_calc = data0$flow_red_per/100
  data0$flow_red_per_calc = pmin(data0$flow_red_per_calc,0.9999)
  data0$flow_red_per_calc = pmax(data0$flow_red_per_calc,0.0001)
  
  #select only years up to begin of decline
  data          <- data0
  data5         <-data
  
  #tierkloof ----
  data0         <- read.csv2("data/raw/catchments/tier.csv")
  percent = 36
  data0$percent = 36
  data0$tree = 'pin'
  data0$con = 'sub'
  data0$loc = "tier"
  
  #calc reducation at 100% cover
  data0$flow_red_per_rescale = data0$flow_red_per/100
  data0$flow_red_per_rescale = data0$flow_red_per_rescale*(100/percent)
  data0$flow_red_per_rescale = pmin(data0$flow_red_per_rescale,0.9999)
  data0$flow_red_per_rescale = pmax(data0$flow_red_per_rescale,0.0001)
  #cal raw reduction
  data0$flow_red_per_calc = data0$flow_red_per/100
  data0$flow_red_per_calc = pmin(data0$flow_red_per_calc,0.9999)
  data0$flow_red_per_calc = pmax(data0$flow_red_per_calc,0.0001)
  
  #select only years up to begin of decline
  data0         <- data0[-22,]
  data          <- data0[1:26,]
  data6         <-data
  
  #bosbouklooof ----
  data0         <- read.csv2("data/raw/catchments/bos.csv")
  percent = 57
  data0$percent = 57
  data0$tree = 'pin'
  data0$con = 'sub'
  data0$loc = "bos"
  
  #calc reducation at 100% cover
  data0$flow_red_per_rescale = data0$flow_red_per/100
  data0$flow_red_per_rescale = data0$flow_red_per_rescale*(100/percent)
  data0$flow_red_per_rescale = pmin(data0$flow_red_per_rescale,0.9999)
  data0$flow_red_per_rescale = pmax(data0$flow_red_per_rescale,0.0001)
  #cal raw reduction
  data0$flow_red_per_calc = data0$flow_red_per/100
  data0$flow_red_per_calc = pmin(data0$flow_red_per_calc,0.9999)
  data0$flow_red_per_calc = pmax(data0$flow_red_per_calc,0.0001)
  
  #select only years up to begin of decline
  data          <- data0[-37,]
  data7         <-data
  
  #beisiesvlei ----
  data0         <- read.csv2("data/raw/catchments/bei.csv")
  percent = 98
  data0$percent = 98
  data0$tree = 'pin'
  data0$con = 'sub'
  data0$loc = "bei"
  
  #calc reducation at 100% cover
  data0$flow_red_per_rescale = data0$flow_red_per/100
  data0$flow_red_per_rescale = data0$flow_red_per_rescale*(100/percent)
  data0$flow_red_per_rescale = pmin(data0$flow_red_per_rescale,0.9999)
  data0$flow_red_per_rescale = pmax(data0$flow_red_per_rescale,0.0001)
  #cal raw reduction
  data0$flow_red_per_calc = data0$flow_red_per/100
  data0$flow_red_per_calc = pmin(data0$flow_red_per_calc,0.9999)
  data0$flow_red_per_calc = pmax(data0$flow_red_per_calc,0.0001)
  
  #select only years up to begin of decline
  data          <- data0[6:nrow(data0),]
  data8         <-data
  
  #cathedral peak catchment 2 ----
  data0         <- read.csv2("data/raw/catchments/cat_2.csv")
  percent = 75
  data0$percent = 75
  data0$tree = 'pin'
  data0$con = 'sub'
  data0$loc = "cat_2"
  
  #calc reducation at 100% cover
  data0$flow_red_per_rescale = data0$flow_red_per/100
  data0$flow_red_per_rescale = data0$flow_red_per_rescale*(100/percent)
  data0$flow_red_per_rescale = pmin(data0$flow_red_per_rescale,0.9999)
  data0$flow_red_per_rescale = pmax(data0$flow_red_per_rescale,0.0001)
  #cal raw reduction
  data0$flow_red_per_calc = data0$flow_red_per/100
  data0$flow_red_per_calc = pmin(data0$flow_red_per_calc,0.9999)
  data0$flow_red_per_calc = pmax(data0$flow_red_per_calc,0.0001)
  #select only years up to begin of decline
  data          <- data0
  data9         <-data
  
  #lambrechbos a ----
  data0         <- read.csv2("data/raw/catchments/lamb_a.csv")
  percent = 89
  data0$percent = 89
  data0$tree = 'pin'
  data0$con = 'sub'
  data0$loc = "lamb_a"
  
  #calc reducation at 100% cover
  data0$flow_red_per_rescale = data0$flow_red_per/100
  data0$flow_red_per_rescale = data0$flow_red_per_rescale*(100/percent)
  data0$flow_red_per_rescale = pmin(data0$flow_red_per_rescale,0.9999)
  data0$flow_red_per_rescale = pmax(data0$flow_red_per_rescale,0.0001)
  #cal raw reduction
  data0$flow_red_per_calc = data0$flow_red_per/100
  data0$flow_red_per_calc = pmin(data0$flow_red_per_calc,0.9999)
  data0$flow_red_per_calc = pmax(data0$flow_red_per_calc,0.0001)
  #select only years up to begin of decline
  data          <- data0[1:12,]
  data10        <-data
  
  # bind all data ------
  all_data      <- bind_rows(data1,data2,data3,data4,data5,data6,data7,data8,data9,data10)
  all_data$tree <- as.factor(all_data$tree)
  all_data$con  <- as.factor(all_data$con)
  
  #write output
  write_feather(all_data,'data/output/catchments_raw.feather')
}

