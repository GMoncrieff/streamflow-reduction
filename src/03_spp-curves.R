library(dplyr)
library(tidyr)

# assignment of invasive species to strteamflow reduction curves
# data from le maitre 2016

#col1: spp name
#col2: spp curve - prob spp takes Eucalypt curve. 1 = Euc, 0 = Pine
#col3: opt curve - prob spp takes optimal curve. 1 = optimal, 0 = suboptimal
#col4: modifier - spp specific modifier. some species get assigned a vlaue based on expert knowledge. 0 = not modified, 1 = modified

curves <- c("ACYCLO",0.75,1,0,
  "AMELAN",1,1,0,
  "ASALIG",0.75,1,0,
  "ADONAX",1,1,1,
  "ANUMMU",0,0,0,
  "CDECAP",0.75,1,0,
  "CESTRS",0.75,1,0,
  "CODORA",1,1,0,
  "EUCALS",1,0,0,
  "HAKEAS",1,1,0,
  "JMIMOS",0.25,1,0,
  "LCAMAR",1,1,0,
  "MAZEDA",0.25,1,0,
  "PINUSS",0,0,0,
  "POPULS",0.25,1,0,
  "PGUAJA",0.75,1,0,
  "RRUBIG",0.25,1,0,
  "SBABYL",0.25,1,0,
  "SDIDYM",0.75,1,0,
  "SPUNIC",0.75,1,0,
  "SMAURI",1,1,0,
  "TCHINE",0.75,1,0,
  "WATTLS",1,1,0)

#reshape and name
dim(curves) = c(4,23)
curves <- as.data.frame(t(curves))
names(curves) <- c("spp","spp_curve","spp_opt","mod")
curves$spp_curve <- as.numeric(as.character(curves$spp_curve))
curves$spp_opt <- as.numeric(as.character(curves$spp_opt))
curves$mod <- as.numeric(as.character(curves$mod))

write.csv(curves,'data/output/curves.csv')